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
