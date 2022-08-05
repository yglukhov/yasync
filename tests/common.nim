import std/[exitprocs, strutils]
import yasync
from asyncdispatch import nil

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
    assert(false, "Unexpected output")

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

proc waitForButDontRead*(f: FutureBase) =
  while not f.finished:
    asyncdispatch.poll()

proc waitForAux*[T](f: Future[T]): T =
  waitForButDontRead(f)
  f.read()

template waitFor*[T](f: Future[T]): auto =
  block:
    type Env = asyncCallEnvType(f)
    when Env is void:
      waitForAux(f)
    else:
      if false:
        discard f
      var e: Env
      asyncLaunchWithEnv(e, f)
      while not e.finished:
        asyncdispatch.poll()
      e.read()
