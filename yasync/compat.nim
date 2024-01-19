from std/asyncdispatch import nil
import ../yasync

proc stdFutureToFuture[T](f: asyncdispatch.Future[T], env: ptr Cont[T]) {.asyncRaw.} =
  asyncdispatch.addCallback(f) do():
    if unlikely asyncdispatch.failed(f):
      env.fail(f.error)
    else:
      when T is void:
        env.complete()
      else:
        env.complere(asyncdispatch.read(f))

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
