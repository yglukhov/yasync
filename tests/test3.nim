import yasync
import ./common

expectOutput """
10
"""
proc foo(): int {.async.} =
  await sleep(10)
  result = 10

log waitFor foo()

expectOutput """
6
"""
proc generic1[T](a, b: T, select: bool): T {.async.} =
  await sleep(10)
  result = if select: a else: b

let a = waitFor generic1(5, 6, false)
log a

