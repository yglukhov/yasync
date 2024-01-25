import yasync
import ./common

block:
  expectOutput """
  1
  5
  """

  proc foo(): int {.async.} =
    await sleep(5)
    log 1
    return 5

  let f = foo()
  f.then do(v: int, err: ref Exception):
    log v

  discard waitFor(f)

block:
  expectOutput """
  2
  void complete
  """

  proc fooVoid() {.async.} =
    await sleep(5)
    log 2

  let f = fooVoid()
  f.then do(err: ref Exception):
    log "void complete"

  waitFor(f)

block:
  expectOutput """
  hi
  8
  7
  6
  5
  123
  """
  proc test() =
    var a = "hi"
    var s = @[8, 7, 6]
    proc foo(b: int): int {.asyncClosureExperimental.} =
      log a
      await sleep(10)
      for i in s:
        log i
      log b
      123
    log waitFor foo(5)
  test()

block:
  expectOutput """
  1
  5
  2
  """

  var cb: proc(a: int): Future[void]

  proc setCallback(c: proc(a: int): Future[void]) =
    cb = c

  setCallback() do(i: int) {.async.}:
    log 1
    await sleep(10)
    log i
    await sleep(10)
    log 2

  waitFor cb(5)
