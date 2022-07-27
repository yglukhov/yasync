from std/asyncdispatch import nil
from ../yasync import nil

proc stdFutureToFuture[T](f: asyncdispatch.Future[T]): yasync.Future[T] =
  result.new()
  let res = result
  asyncdispatch.addCallback(f) do():
    res.complete(asyncdispatch.read(f))

template awaitc*[T](f: asyncdispatch.Future[T]): T =
  yasync.await(stdFutureToFuture(f))

template awaitc*(f: asyncdispatch.Future[void]) =
  yasync.await(stdFutureToFuture(f))
