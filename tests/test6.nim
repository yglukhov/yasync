import yasync
import yasync/compat
import ./common

expectOutput """
1
5
2
"""

var cb: proc(a: int): Future[void]

proc setCallback(c: proc(a: int): Future[void]) =
  cb = c

setCallback() do(i: int) {.async.}:
  log 1
  await sleep(10)
  log i
  await sleep(10)
  log 2

waitFor cb(5)
