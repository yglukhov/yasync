import yasync
# import yasync/compat
import ./common
# import httpclient
# from asyncdispatch import nil

expectOutput """
yo: hi
hi
1
bye: 5
bar
"""

proc foo(a: int) {.async.} =
  log "hi"
  await sleep(10)
  log 1
  await sleep(10)
  # let c = newAsyncHttpClient()
  # let cn = awaitc c.getContent("https://google.com")
  # echo cn
  # bar("yo")
  log "bye: ", a

proc bar(yo: string) {.async.} =
  log "yo: ", yo
  await foo(5)
  log "bar"

waitFor bar("hi")

expectOutput """
Bar env size: 136
"""

type BarEnv = asyncCallEnvType bar("hi")
log "Bar env size: ", sizeof(BarEnv)

expectOutput """
a: 5
a: 4
a: 3
a: 2
a: 1
"""

proc recursive1(a: int) {.async.} =
  if a > 0:
    await sleep(10)
    log "a: ", a
    await recursive1(a - 1)

waitFor recursive1(5)
