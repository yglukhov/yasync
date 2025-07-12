import std/[exitprocs, strutils]
import yasync
from yasync/compat import waitFor

const asyncBackend {.strdefine.} = ""
const useChronosBackend* = asyncBackend == "chronos"
when useChronosBackend:
  from chronos import nil
  proc sleepRaw*(ms: int, env: ptr Cont[void]) {.asyncRaw.} =
    chronos.addCallback(chronos.sleepAsync(ms)) do(p: pointer):
      env.complete()

  proc sleep*(ms: int): Future[void] =
    result.new()
    let res = result
    chronos.addCallback(chronos.sleepAsync(ms)) do(p: pointer):
      res.complete()

else:
  from asyncdispatch import nil
  proc sleepRaw*(ms: int, env: ptr Cont[void]) {.asyncRaw.} =
    asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
      env.complete()

  proc sleep*(ms: int): Future[void] =
    result.new()
    let res = result
    asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
      res.complete()

export waitFor

var logBuffer {.threadvar.}: string
var expectedOutput {.threadvar.}: string

proc log*(v: varargs[string, `$`]) {.gcsafe.} =
  for a in v:
    logBuffer &= a
  logBuffer &= '\n'

proc checkOutput() =
  if logBuffer == expectedOutput:
    discard
  else:
    echo "Unexpected output"
    echo "Expected:"
    echo expectedOutput
    echo "Actual:"
    echo logBuffer
    doAssert(false, "Unexpected output")

proc expectOutput*(v: string) =
  checkOutput()
  expectedOutput = v.unindent
  logBuffer = ""

addExitProc do():
  checkOutput()
  echo "OK"

proc sleep2*(ms: int) {.async.} =
  await sleep(ms div 2)
  await sleep(ms div 2)
