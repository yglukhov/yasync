from std/asyncdispatch import nil
import ../yasync

proc stdFutureToFuture[T](f: asyncdispatch.Future[T]): yasync.Future[T] =
  result.new()
  let res = result
  asyncdispatch.addCallback(f) do():
    if unlikely asyncdispatch.failed(f):
      res.fail(f.error)
    else:
      res.complete(asyncdispatch.read(f))

proc stdFutureToFuture(f: asyncdispatch.Future[void]): yasync.Future[void] =
  result.new()
  let res = result
  asyncdispatch.addCallback(f) do():
    if unlikely asyncdispatch.failed(f):
      res.fail(f.error)
    else:
      res.complete()

proc toCompat*[T](f: yasync.Future[T]): asyncdispatch.Future[T] =
  result = asyncdispatch.newFuture[T]()
  let res = result
  f.then do(v: T, err: ref Exception):
    if not err.isNil:
      asyncdispatch.fail(res, err)
    else:
      asyncdispatch.complete(res, v)

proc toCompat*(f: yasync.Future[void]): asyncdispatch.Future[void] =
  result = asyncdispatch.newFuture[void]()
  let res = result
  f.then do(err: ref Exception):
    if not err.isNil:
      asyncdispatch.fail(res, err)
    else:
      asyncdispatch.complete(res)

template awaitc*[T](f: asyncdispatch.Future[T]): T =
  yasync.await(stdFutureToFuture(f))

template awaitc*(f: asyncdispatch.Future[void]) =
  yasync.await(stdFutureToFuture(f))

proc waitForButDontRead(f: FutureBase) =
  while not f.finished:
    asyncdispatch.poll()

proc waitForAux[T](f: Future[T]): T =
  waitForButDontRead(f)
  f.read()

template waitFor*[T](f: Future[T]): T =
  block:
    type Env = asyncCallEnvType(f)
    when Env is void:
      waitForAux(f)
    else:
      if false:
        discard f
      var e: Env
      asyncLaunchWithEnv(e, f)
      while not e.finished:
        asyncdispatch.poll()
      e.read()
