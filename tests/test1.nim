import yasync
import ./common

expectOutput """
yo: hi
hi
1
bye: 5
bar
"""

proc foo(a: int) {.async.} =
  log "hi"
  await sleep2(10)
  log 1
  await sleep(5)
  await sleep(5)
  log "bye: ", a

proc bar(yo: string) {.async.} =
  log "yo: ", yo
  await foo(5)
  await sleepRaw(5)
  log "bar"

waitFor bar("hi")

expectOutput """
12
23
13
"""

proc logThis(x = 1; y = x+1) {.async.} =
  log x, y

waitFor logThis()
waitFor logThis(2)
waitFor logThis(1, 3)

type BarEnv = asyncCallEnvType bar("hi")
# The following expectations are for 64 bit architectures
when defined(gcDestructors):
  doAssert(sizeof(BarEnv) == 208)
else:
  doAssert(sizeof(BarEnv) == 200)

expectOutput """
a: 5
a: 4
a: 3
a: 2
a: 1
"""

proc recursive1(a: int) {.async.} =
  if a > 0:
    await sleep(10)
    log "a: ", a
    await recursive1(a - 1)

waitFor recursive1(5)

# Using return value
expectOutput """
baz: 5
10
baz: 10
A: 15
baz: 5
baz: 10
B: 25
"""
proc baz(a: int): int {.async.} =
  await sleep(5)
  log "baz: ", a
  a + 5

proc baz2() {.async.} =
  log await baz(5)
  log "A: ", await baz(10)
  log "B: ", (await baz(5)) + (await baz(10))

waitFor baz2()

waitFor sleepRaw(5)

expectOutput """
0
1
2
"""

proc emptyProc*() {.async.} =
  log 1

proc callEmpty() {.async.} =
  log 0
  await emptyProc()
  log 2

discard callEmpty()

var emptyEnv: asyncCallEnvType(callEmpty())
doAssert(sizeof(emptyEnv) == sizeof(Cont[void]))

# asyncRaw implicit conversions
expectOutput """
10.0
"""

proc rawProc1(a: float, env: ptr Cont[float]) {.asyncRaw.} =
  env.complete(a * 2)

log waitFor rawProc1 5

expectOutput """
4
"""

type
  DoubleEnv = object of Cont[int]
    sth: int

proc doubleImpl(val: int, env: ptr DoubleEnv) {.asyncRaw.} =
  complete(env, val * 2)

proc double(val: int): int {.async.} =
  await doubleImpl(val)

proc foo1() {.async.} =
  let val = await double(2)
  log val

waitFor foo1()

#7
expectOutput """
50
"""

proc nothing(n: int): int {.async.} =
  n and 0x1

proc amain() {.async} =
  var coros = newSeq[Future[int]]()
  for i in 0..<100:
    coros.add(nothing(i))
  var n = 0
  for f in coros:
    inc n, await f
  log n

waitFor amain()
