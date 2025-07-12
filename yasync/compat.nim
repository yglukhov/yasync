import ../yasync

const asyncBackend {.strdefine.} = ""
when asyncBackend == "chronos":
  from chronos import poll
  proc stdFutureToFuture[T](f: chronos.Future[T], res: ptr Cont[T]) {.asyncRaw.} =
    chronos.addCallback(f) do(p: pointer) {.gcsafe, raises: [].}:
      if unlikely chronos.failed(f):
        res.fail(chronos.error(f))
      else:
        when T is void:
          res.complete()
        else:
          res.complete(f.internalValue)

  proc toCompat*[T](f: yasync.Future[T]): chronos.Future[T] =
    result = chronos.newFuture[T]()
    let res = result
    f.then do(v: T, err: ref Exception):
      if not err.isNil:
        chronos.fail(res, cast[ref CatchableError](err))
      else:
        when T is void:
          chronos.complete(res)
        else:
          chronos.complete(res, v)

  template awaitc*[T](f: chronos.Future[T]): T =
    bind stdFutureToFuture
    yasync.await(stdFutureToFuture(f))

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
      if not err.isNil:
        asyncdispatch.fail(res, err)
      else:
        when T is void:
          asyncdispatch.complete(res)
        else:
          asyncdispatch.complete(res, v)

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
