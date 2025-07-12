# asyncdispatch compatibility

from ./common import log, expectOutput, useChronosBackend
when useChronosBackend:
  from chronos import sleepAsync
else:
  from asyncdispatch import sleepAsync

import yasync
import yasync/compat

expectOutput """
hi
bye
1
"""

proc foo(): int {.async.} =
  log "hi"
  # asyncdispatch futures should be awaited with `awaitc`
  awaitc sleepAsync(10)
  log "bye"
  return 1

log common.waitFor foo()

expectOutput """
hi
bye
1
"""

when useChronosBackend:
  log chronos.waitFor foo().toCompat()
else:
  log asyncdispatch.waitFor foo().toCompat()

proc fooVoid() {.async.} =
  log 1
  awaitc sleepAsync(10)
  log 2

expectOutput """
1
2
"""

when useChronosBackend:
  chronos.waitFor fooVoid().toCompat()
else:
  asyncdispatch.waitFor fooVoid().toCompat()

import asyncdispatch_functions

proc getContent() {.async.} =
  let content = awaitc (awaitc getResponse()).body
  assert(content == "hello")
  echo content

common.waitFor getContent()
