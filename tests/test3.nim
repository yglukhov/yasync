import yasync
import ./common

expectOutput """
10
"""

proc foo(): int {.async.} =
  await sleep(10)
  10

log waitFor foo()

expectOutput """
10
"""

let f = foo()
log waitFor f

expectOutput """
6
"""
proc generic1[T](a, b: T, select: bool): T {.async.} =
  await sleep(10)
  result = if select: a else: b

let a = waitFor generic1(5, 6, false)
log a

expectOutput """
hello
"""

let b = waitFor generic1("bye", "hello", false)
log b

proc generic2[T](a: T): T {.async.} =
  await sleep(5)
  log a
  if a != 0:
    (await generic2(a - 1)) + 1
  else:
    1

expectOutput """
3
2
1
0
4
"""

log waitFor generic2(3)

proc generic3(a: typedesc, b, c: float): a {.async.} =
  await sleep(5)

expectOutput """
0
"""

log waitFor generic3(int, 4, 5)

expectOutput """
123
"""

proc generic4[T](a: int): T {.async.} =
  await sleep(5)
  T(a)

log waitFor generic4[int](123)

expectOutput """
10
"""

type OOO[T] = object
  m: int

proc genericRaw1[T](a: OOO[T], env: ptr Cont[T]) {.asyncRaw.} =
  env.complete(a.m + 5)

proc foo1() {.async.} =
  log await genericRaw1(OOO[int](m: 5))

waitFor foo1()
