import yasync
import ./common

proc raiseException() =
  log "raising"
  raise newException(ValueError, "This is a test exception")

proc foo() {.async.} =
  await sleep(10)
  log "hi"
  raiseException()
  log "done"

# proc convertAsyncStackTrace(t: seq[StackTraceEntry]): seq[StackTraceEntry] =
#   if t[^1].line == -100:
#     discard
#   else:
#     return t

# proc processStackTrace(t: seq[StackTraceEntry]): string =
#   for en in t:
#     echo en.filename, "(", en.line, "): ", en.procname
#   discard

proc baz() {.async.} =
  raiseException()

proc bar() {.async.} =
  await sleep(10)
  log 1
  await baz()
  log 2

expectOutput """
1
raising
exception
"""

try:
  waitFor bar()
except Exception as e:
  log "exception"

expectOutput """
hi
raising
exception
"""

try:
  waitFor foo()
except Exception as e:
  log "exception"

expectOutput """
raising
exception
"""

try:
  waitFor baz()
except Exception as e:
  log "exception"
