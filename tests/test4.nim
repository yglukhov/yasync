# asyncdispatch compatibility

from asyncdispatch import waitFor, sleepAsync
import yasync
import yasync/compat
from ./common import log, expectOutput

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

log asyncdispatch.waitFor foo().toCompat()

proc fooVoid() {.async.} =
  log 1
  awaitc sleepAsync(10)
  log 2

expectOutput """
1
2
"""

asyncdispatch.waitFor fooVoid().toCompat()

import asyncdispatch_functions

proc getContent() {.async.} =
  let content = awaitc (awaitc getResponse()).body
  assert(content == "hello")
  echo content

common.waitFor getContent()
