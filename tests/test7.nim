import yasync
from strutils import join

proc log(s: varargs[string, `$`]) =
  when false:
    echo s.join(" ")

type
  Channel[T] = object
    waitingCont: ptr Cont[T]
    sendingCont: ptr Cont[void]
    val: T

proc send[T](c: var Channel[T], v: T, env: ptr Cont[void]) {.asyncRaw.} =
  doAssert(c.sendingCont == nil, "Too many senders")
  if c.waitingCont == nil:
    c.val = v
    c.sendingCont = env
  else:
    let cont = c.waitingCont
    c.waitingCont = nil
    cont.complete(v)
    env.complete()

proc recv[T](c: var Channel[T], env: ptr Cont[T]) {.asyncRaw.} =
  doAssert(c.waitingCont == nil, "Too many receivers")
  if c.sendingCont == nil:
    c.waitingCont = env
  else:
    let cont = c.sendingCont
    c.sendingCont = nil
    env.complete(c.val)
    cont.complete()

var channel1 = Channel[int]()
var channel2 = Channel[int]()

proc filterProcess(filter: proc(a: int): int {.nimcall.}) {.async.} =
  while true:
    await send(channel2, filter(await recv(channel1)))

var senderDone = false
proc senderProcess() {.async.} =
  for i in 1 .. 5:
    log "Sending ", i
    await send(channel1, i)
  log "Sender done"
  senderDone = true

var receiverDone = false
proc receiverProcess() {.async.} =
  while true:
    log "Waiting for recv"
    let i = await recv(channel2)
    log "Received ", i
    if i >= 10:
      break
  log "Receiver done"
  receiverDone = true

proc filter(a: int): int {.nimcall.} = a * 2

# Launch all the 3 tasks. We could do just `discard senderProcess()`
# but to prevent heap allocation, let's keep their envs in static memory.
var senderEnv: asyncCallEnvType(senderProcess())
asyncLaunchWithEnv(senderEnv, senderProcess())

var filterEnv: asyncCallEnvType(filterProcess(filter))
asyncLaunchWithEnv(filterEnv, filterProcess(filter))

var receiverEnv: asyncCallEnvType(receiverProcess())
asyncLaunchWithEnv(receiverEnv, receiverProcess())

doAssert(senderDone)
doAssert(receiverDone)
