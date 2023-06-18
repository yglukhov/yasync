import std/[exitprocs, strutils]
import yasync
from yasync/compat import waitFor
from asyncdispatch import nil

export waitFor

var logBuffer = ""
var expectedOutput = ""

proc log*(v: varargs[string, `$`]) =
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

proc sleep*(ms: int, env: ptr Cont[void]) {.asyncRaw.} =
  asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
    env.complete()

proc sleep*(ms: int): Future[void] =
  result.new()
  let res = result
  asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
    res.complete()

proc sleep2*(ms: int) {.async.} =
  await sleep(ms div 2)
  await sleep(ms div 2)
