import yasync
import ./common
from asyncdispatch import nil

type
  SleepCont = object of Cont[void]
    ppCont: ptr ptr SleepCont
    someParam: int


proc onCancelSleep(env: ptr SleepCont) =
  log "sleep cancelled: ", env.someParam
  if env.ppCont != nil:
    env.ppCont[] = nil

proc sleepRaw*(ms: int, env: ptr SleepCont) {.asyncRaw.} =
  env.someParam = 123
  log "sleeping"

  var pEnv = env
  env.ppCont = addr pEnv

  # If sleepRaw is cancelled, env will be invalid pointer
  # by the time sleepAsync completes.
  # Because we can't cancel sleepAsync, we nullify pEnv in
  # onCancelSleep.
  asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
    let e = pEnv
    if e != nil:
      e.complete()
  setCancelCb(env, onCancelSleep)

proc foo() {.async.} =
  try:
    await sleepRaw(5000)
  except CancelError:
    log "foo cancelled"

expectOutput """
sleeping
will cancel
sleep cancelled: 123
foo cancelled
did cancel
"""

let f = foo()
log "will cancel"
f.cancel()
log "did cancel"

######

expectOutput """
sleeping
will cancel
sleep cancelled: 123
did cancel
"""

let f1 = sleepRaw(1000000)
log "will cancel"
f1.cancel()
doAssert(f1.error of CancelError)
log "did cancel"
f1.cancel()

######

expectOutput """
sleeping
will cancel
sleep cancelled: 123
foo cancelled
did cancel
"""

var fenv: asyncCallEnvType(foo())
asyncLaunchWithEnv(fenv, foo())
log "will cancel"
fenv.cancel()
log "did cancel"

######

expectOutput """
sleeping
will cancel
sleep cancelled: 123
did cancel
"""

var sleepEnv2: asyncCallEnvType(sleepRaw(100000))
asyncLaunchWithEnv(sleepEnv2, sleepRaw(100000))
log "will cancel"
sleepEnv2.cancel()
log "did cancel"
