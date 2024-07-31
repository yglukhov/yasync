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

block:
  expectOutput """
  new 1
  new 2
  1
  destroy 1
  b 2
  2
  destroy 2
  """

  type
    SmartPtr = object
      o: int

  proc newId(): int =
    var g {.global.} = 0
    inc g
    log "new ", g
    g

  proc `=destroy`(o: var SmartPtr) =
    if o.o != 0:
      log "destroy ", o.o
      o.o = 0

  proc `=copy`(a: var SmartPtr, b: SmartPtr) =
    if a.o != 0:
      log "delete ", a.o
    if b.o == 0:
      a.o = 0
    else:
      a.o = newId()

  proc foo() {.async.} =
    let a = SmartPtr(o: newId())
    var b = SmartPtr(o: newId())
    log 1
    await sleep(1)
    log "b ", b.o
    log 2

  waitFor foo()
