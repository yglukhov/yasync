import ../yasync

const asyncBackend {.strdefine.} = ""
when asyncBackend == "chronos":
  from chronos import poll

  type
    ChronosCont[T] = object of Cont[T]
      fut: chronos.Future[T]

  proc chronosFutureToFuture[T](f: chronos.Future[T], res: ptr ChronosCont[T]) {.asyncRaw.} =
    proc cb(p: pointer) {.gcsafe, raises: [], nimcall.} =
      let res = cast[ptr ChronosCont[T]](p)
      let f = res.fut
      res.fut = nil
      if unlikely chronos.failed(f):
        res.fail(chronos.error(f))
      else:
        when T is void:
          res.complete()
        else:
          res.complete(f.internalValue)
    chronos.addCallback(f, cb, res)
    res.fut = f
    res.setCancelCb do(e: ptr ChronosCont[T]):
      discard chronos.tryCancel(e.fut)

  proc toCompat*[T](f: yasync.Future[T]): chronos.Future[T] =
    result = chronos.newFuture[T]()
    let res = result
    f.then do(v: T, err: ref Exception):
      if likely err.isNil:
        when T is void:
          chronos.complete(res)
        else:
          chronos.complete(res, v)
      else:
        chronos.fail(res, cast[ref CatchableError](err))

    # Unfortunately chronos doesn't allow passing udata along with
    # cancel cb, so here goes a closure allocation
    chronos.`cancelCallback=`(result, proc(p: pointer) {.gcsafe, raises: [].} =
      f.cancel())

  template awaitc*[T](f: chronos.Future[T]): T =
    bind chronosFutureToFuture
    yasync.await(chronosFutureToFuture(f))

else:
  from std/asyncdispatch import poll

  proc stdFutureToFuture[T](f: asyncdispatch.Future[T], res: ptr Cont[T]) {.asyncRaw.} =
    asyncdispatch.addCallback(f) do():
      if unlikely asyncdispatch.failed(f):
        res.fail(f.error)
      else:
        when T is void:
          res.complete()
        else:
          res.complete(asyncdispatch.read(f))

  proc toCompat*[T](f: yasync.Future[T]): asyncdispatch.Future[T] =
    result = asyncdispatch.newFuture[T]()
    let res = result
    f.then do(v: T, err: ref Exception):
      if likely err.isNil:
        when T is void:
          asyncdispatch.complete(res)
        else:
          asyncdispatch.complete(res, v)
      else:
        asyncdispatch.fail(res, err)

  template awaitc*[T](f: asyncdispatch.Future[T]): T =
    bind stdFutureToFuture
    yasync.await(stdFutureToFuture(f))

proc waitForButDontRead(f: FutureBase) =
  while not f.finished:
    poll()

proc waitForAux[T](f: Future[T]): T =
  waitForButDontRead(f)
  f.read()

template waitFor*[T](f: Future[T]): T =
  block:
    type Env = asyncCallEnvType(f)
    when Env is void:
      waitForAux(f)
    else:
      var e: Env
      asyncLaunchWithEnv(e, f)
      while not e.finished:
        poll()
      e.read()
