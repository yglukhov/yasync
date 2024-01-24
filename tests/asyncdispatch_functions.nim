import asyncdispatch

type
  Response* = object
    body*: Future[string]

proc getBody(): Future[string] {.async.} =
  await sleepAsync(5)
  return "hello"

proc getResponse*(): Future[Response] {.async.} =
  await sleepAsync(5)
  return Response(body: getBody())
