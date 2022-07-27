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
  expectedOutput = v
  logBuffer = ""

proc onQuit() {.noconv.} =
  checkOutput()
  echo "OK"

addQuitProc(onQuit)

proc sleep(ms: int, env: ptr Cont[void]) {.asyncRaw.} =
  asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
    env.complete()

proc sleep*(ms: int): Future[void] =
  result.new()
  let res = result
  asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
    res.complete()

proc waitFor*[T](f: Future[T]): T =
  while not f.finished:
    asyncdispatch.poll()
  f.read()