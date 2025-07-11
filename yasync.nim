import std/[macros, tables, hashes, strutils]

type
  ProcType = proc(e: pointer) {.gcsafe, nimcall.}
  ContFlags = enum
    fAllocated

  ContHeader = object
    p: ProcType
    e: ptr ContBase
    error: ref Exception
    flags: pointer # used for ContFlags, and Substates offset

  ContBase = object of RootObj
    `<state_reserved>`: int
    h: ContHeader

  Cont*[T] = object of ContBase
    when T isnot void:
      result*: T

  FutureBase* = ref ContBase
  Future*[T] = ref Cont[T]

  AsyncEnv*[T] = object
    env*: T

  CancelError* = object of CatchableError

{.pragma: silent, inline, stackTrace: off, lineTrace: off.}

proc finished(f: ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished(f: ptr ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: FutureBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: AsyncEnv): bool {.inline.} = finished(cast[ptr ContBase](addr f))

proc getSubstatesOffset(h: ContHeader): uint {.silent.} =
  cast[uint](h.flags) shr 1

proc setSubstatesOffset(h: var ContHeader, retType, substates: typedesc) {.silent.} =
  # Task offset in bytes = addr(Substates) - addr(ContHeader)
  assert(getSubstatesOffset(h) == 0, "yasync internal error")
  type ReferenceStruct = object
    h: ContHeader
    when retType isnot void:
      r: retType
    s: substates
  const o = offsetof(ReferenceStruct, s).uint
  h.flags = cast[pointer](cast[uint](h.flags) or (o shl 1))

proc newFuture*(T: typedesc): Future[T] =
  result.new()

proc markAllocatedEnv(h: var ContHeader) {.silent.} =
  const flag = uint(1 shl ord(fAllocated))
  h.flags = cast[pointer](cast[uint](h.flags) or flag)

proc isAllocatedEnv(h: ContHeader): bool {.silent.} =
  const flag = uint(1 shl ord(fAllocated))
  (cast[uint](h.flags) and flag) != 0

proc markAllocatedEnv(e: ptr ContBase) {.silent.} = markAllocatedEnv(e.h)
proc isAllocatedEnv(e: ptr ContBase): bool {.silent.} = isAllocatedEnv(e.h)

template setStateFinished[T](a: T) =
  a.`<state_reserved>` = -1

proc resume(p: ptr ContBase) =
  var p = p
  while not p.isNil:
    if p.finished:
      let p1 = p.h.e
      if isAllocatedEnv(p):
        let pp = cast[ref ContBase](p)
        GC_unref(pp)

      p = p1
    else:
      let f = p.h.p
      if f.isNil:
        break
      try:
        f(p)
      except Exception as e:
        # Closure iterators throwing exceptions do not necessarily have finished state
        # so fix it here
        p.setStateFinished()
        p.h.error = e
      if not p.finished:
        break

proc launch(p: ptr ContBase) {.silent.} =
  p.h.p(p)

proc launchf(p: ptr ContBase): bool {.silent.} =
  launch(p)
  not finished(p)

proc complete*[T](resFut: ptr Cont[T], v: T) =
  resFut.setStateFinished()
  resFut.result = v
  resume(resFut)

proc complete*(resFut: ptr Cont[void]) =
  resFut.setStateFinished()
  resume(resFut)

proc complete*[T](resFut: Future[T], v: T) {.inline.} =
  complete(cast[ptr Cont[T]](resFut), v)

proc complete*(resFut: Future[void]) {.inline.} =
  complete(cast[ptr Cont[void]](resFut))

proc fail*(resFut: ptr ContBase, err: ref Exception) =
  resFut.setStateFinished()
  resFut.h.error = err
  resume(resFut)

proc fail*(resFut: FutureBase, err: ref Exception) {.inline.} =
  fail(cast[ptr ContBase](resFut), err)

proc error*(f: FutureBase): ref Exception {.inline.} = f.h.error

type
  CB[T] = ref object of ContBase
    cb: proc(v: T, error: ref Exception) {.gcsafe.}
    f: Future[T]

  CBVoid = ref object of ContBase
    cb: proc(error: ref Exception) {.gcsafe.}
    f: Future[void]

proc onComplete[T](p: pointer) {.nimcall, gcsafe.} =
  # let p = cast[ptr ContBase](p)
  when T is void:
    let p = cast[CBVoid](p)
    p.setStateFinished()
    p.cb(p.f.h.error)
  else:
    let p = cast[CB[T]](p)
    p.setStateFinished()
    p.cb(p.f.result, p.f.h.error)

proc then*[T](f: Future[T], cb: proc(v: T, error: ref Exception) {.gcsafe.}) =
  assert(f.h.e.isNil, "Future already has a callback")
  if f.finished:
    cb(f.result, f.h.error)
  else:
    let c = CB[T](f: f, cb: cb)
    markAllocatedEnv(c.h)
    GC_ref(c)
    c.h.p = onComplete[T]
    f.h.e = cast[ptr ContBase](c)

proc then*(f: Future[void], cb: proc(error: ref Exception) {.gcsafe.}) =
  assert(f.h.e.isNil, "Future already has a callback")
  if f.finished:
    cb(f.h.error)
  else:
    let c = CBVoid(f: f, cb: cb)
    markAllocatedEnv(c.h)
    GC_ref(c)
    c.h.p = onComplete[void]
    f.h.e = cast[ptr ContBase](c)

proc checkFinished(resFut: ptr ContBase) {.stackTrace: off.} =
  assert(resFut.finished)
  if not resFut.h.error.isNil:
    raise resFut.h.error

proc read[T](resFut: Cont[T]): T =
  checkFinished(addr resFut)
  when T isnot void:
    return resFut.result

proc read*[T](resFut: Future[T]): T =
  checkFinished(cast[ptr ContBase](resFut))
  when T isnot void:
    return resFut.result

proc readAux[T](a: T): auto {.inline.} =
  when compiles(a.result2):
    a.result2
  else:
    discard

proc read*(resFut: AsyncEnv): auto =
  checkFinished(cast[ptr ContBase](addr resFut))
  readAux(resFut.env)

proc setCancelCb*[T: Cont](p: ptr T, cb: proc(p: ptr T) {.nimcall.}) {.silent.} =
  p.h.p = cast[ProcType](cb)

proc raiseCancelError(c: ptr ContBase) =
  c.fail(newException(CancelError, ""))

proc cancel(c: ptr ContBase) =
  type
    DummySubstates = object
      case sub: uint8
      of 0:
        tmpFut: FutureBase
      of 1:
        sub1: ContBase
      else:
        discard

  var c = c
  while true:
    let o = getSubstatesOffset(c.h)
    if o == 0:
      let cancelProc = c.h.p
      if not cancelProc.isNil:
        c.h.p = nil
        cancelProc(c)

      raiseCancelError(c)
      break
    else:
      let subs = cast[ptr DummySubstates](cast[uint](c) + o + offsetof(ContBase, h).uint)
      let state = subs.sub
      if state == 0:
        let pFut = cast[ptr FutureBase](cast[uint](subs) + offsetof(DummySubstates, tmpFut).uint)
        let fut = cast[ptr ContBase](pFut[])
        if fut.isNil:
          raiseCancelError(c)
          break
        c = cast[ptr ContBase](fut)
      else:
        c = cast[ptr ContBase](cast[uint](subs) + offsetof(DummySubstates, sub1).uint)

proc cancel*(f: FutureBase) {.silent.} =
  cancel(cast[ptr ContBase](f))

proc cancel*(e: var AsyncEnv) {.silent.} =
  cancel(cast[ptr ContBase](addr e))

template thisEnv(a: var ContHeader): ptr ContBase =
  cast[ptr ContBase](cast[int](addr(a)) - sizeof(int) * 2)

type
  AsyncProcData = object
    procPtrName: string # C Symbol of proc ptr
    envType: NimNode

proc hash(n: NimNode): Hash = hash($n)
var asyncData {.compileTime.} = initTable[NimNode, AsyncProcData]()

iterator arguments(formalParams: NimNode): tuple[idx: int, name, typ, default: NimNode] =
  proc stripSinkFromArgType(t: NimNode): NimNode =
    result = t
    if result.kind == nnkBracketExpr and result.len == 2 and result[0].kind == nnkSym and $result[0] == "sink":
      result = result[1]

  formalParams.expectKind(nnkFormalParams)
  var iParam = 0
  for i in 1 ..< formalParams.len:
    let pp = formalParams[i]
    for j in 0 .. pp.len - 3:
      var t = copyNimTree(stripSinkFromArgType(pp[^2]))
      if t.kind == nnkEmpty: t = newCall("typeof", pp[^1])
      yield (iParam, pp[j], t, pp[^1])
      inc iParam

template keepFromReordering[T](v: T) =
  # This proc is used to make nim place iterator variables in its environment
  # in the same order they are defined in the iterator.
  # It was not needed until https://github.com/nim-lang/Nim/pull/22559
  # It is no longer needed after https://github.com/nim-lang/Nim/pull/23787
  discard v

const canLiftLocals = compiles(block:
                                 var a {.liftLocals.}: int
                                 a)

proc sameIdent(a: NimNode, s: string): bool =
  cmpIgnoreStyle($a, s) == 0

proc isGenericArgType(t: NimNode): bool =
  if t.kind == nnkIdent and sameIdent(t, "typedesc"):
    return true
  elif t.kind == nnkBracketExpr and t[0].kind == nnkSym and sameIdent(t[0], "typedesc"):
    return true

proc makeArgDefs(prc: NimNode): NimNode =
  let varSection = newNimNode(nnkVarSection)
  result = newNimNode(nnkStmtList)
  result.add(varSection)
  for i, n, t, d in arguments(prc.params):
    if not isGenericArgType(t):
      when canLiftLocals:
        varSection.add(newIdentDefs(newTree(nnkPragmaExpr, n, newTree(nnkPragma, ident"noinit", ident"liftLocals")), t))
      else:
        varSection.add(newIdentDefs(newTree(nnkPragmaExpr, n, newTree(nnkPragma, ident"noinit")), t))
        result.add(newCall(bindSym"keepFromReordering", n))

proc transformReturnStmt(n, resSym: NimNode): NimNode =
  result = n
  case n.kind
  of nnkReturnStmt:
    if n[0].kind != nnkEmpty:
      let res = n[0]
      result = quote do:
        `resSym` = `res`
        return
  else:
    for i in 0 ..< result.len:
      result[i] = transformReturnStmt(result[i], resSym)

macro argFieldAccess(o: typed, idx: static[int]): untyped =
  let t = getType(o)
  t.expectKind(nnkObjectTy)
  let rl = t[2]
  var idx = idx + 3
  if $rl[2] == "result2": inc idx
  result = newDotExpr(o, rl[idx])

template fillArg[TEnv, TArg](e: var TEnv, idx: int, arg: TArg) =
  argFieldAccess(e, idx) = arg

template fillArgPtr[TEnv, TArg](e: ref TEnv | ptr TEnv, idx: int, arg: TArg) =
  argFieldAccess(e[], idx) = arg

proc dummyAwaitMarkerMagic[T](f: Future[T]): T {.importc: "yasync_please_report_bug_if_this_symbol_is_missing_on_linkage".}

proc getHeader[T](env: var T): ptr ContHeader {.silent.} =
  cast[ptr ContHeader](cast[int](addr env) + offsetof(ContBase, h))

proc closureEnvType(a: NimNode): NimNode =
  let im = getImplTransformed(a)
  result = getType(im.params[^1])
  result = result[^1]

macro getClosureEnvType(a: typed): untyped =
  result = closureEnvType(a)

macro registerAsyncData(dummyCall: typed, procPtrName: static[string], iterSym: typed): untyped =
  asyncData[dummyCall[0]] = AsyncProcData(envType: newCall(bindSym"getClosureEnvType", iterSym), procPtrName: procPtrName)

macro registerAsyncRawData(dummyCall: typed, procPtrName: static[string], envType: typed): untyped =
  asyncData[dummyCall[0]] = AsyncProcData(envType: envType, procPtrName: procPtrName)

macro asyncCallEnvType*(call: Future): untyped =
  ## Returns type of async environment for the `call`
  ## This type can be used with `asyncLaunchWithEnv`
  ## Returns `void` if the `call` can not be rewritten to `asyncLaunchWithEnv`
  if call.kind == nnkCall:
    let d = asyncData.getOrDefault(call[0])
    if d.envType != nil:
      return newTree(nnkBracketExpr, bindSym"AsyncEnv", d.envType)
  return ident"void"

proc asyncCallProcPtrNameAux(call: NimNode): string =
  call.expectKind(nnkCall)
  let d = asyncData.getOrDefault(call[0])
  doAssert(d.envType != nil, "yasync internal error")
  d.procPtrName

macro asyncCallProcPtrName(call: Future): untyped =
  newLit(asyncCallProcPtrNameAux(call))

template setProc(h: ptr ContHeader, prc: ProcType) = h.p = prc

var counter {.compileTime.} = 0
proc genIterPtrName(s: string): string {.compileTime.} =
  inc counter
  var i = 0
  result = s & "_" & $counter
  while i < result.len:
    if result[i] notin {'A'..'Z','a'..'z','_','0'..'9'}:
      result.delete(i .. i)
    else:
      inc i

proc makeDummySelfCall(prc: NimNode): NimNode =
  result = newCall(prc.name)

  let genericParams = prc[2]
  genericParams.expectKind({nnkEmpty, nnkGenericParams})
  if genericParams.kind == nnkGenericParams:
    let genericCall = newTree(nnkBracketExpr, result[0])
    for p in genericParams:
      for i in 0 ..< p.len - 2:
        genericCall.add(p[i])
    result[0] = genericCall

  # Fill arguments
  for i, n, t, d in arguments(prc.params):
    result.add(n)

proc makeAsyncProcBody(prc, iterSym: NimNode, isCapture: bool): NimNode =
  let envSym = ident("env")
  let fillArgs = newNimNode(nnkStmtList)

  # Fill arguments
  var ip = 0
  for i, n, t, d in arguments(prc.params):
    if not isGenericArgType(t):
      fillArgs.add newCall(bindSym"fillArgPtr", envSym, newLit(ip), n)
      inc ip

  result = quote do:
    var it = `iterSym`
    when `isCapture`:
      let `envSym` = cast[ref ContBase](rawEnv(it))
    else:
      let `envSym` = cast[ref getClosureEnvType(`iterSym`)](rawEnv(it))

    GC_ref(`envSym`)
    markAllocatedEnv(cast[ptr ContBase](`envSym`))
    result = cast[typeof(result)](`envSym`)
    setProc(getHeader(`envSym`[]), cast[ProcType](rawProc(it)))
    when not `isCapture`:
      `fillArgs`
    launch(cast[ptr ContBase](`envSym`))

template assignResult[T](v: T) =
  when T is void:
    v
  else:
    result = v

proc fixupLastReturnStmt(body: NimNode): NimNode =
  if body.len > 0:
    body[^1] = newCall(bindSym"assignResult", body[^1])
  result = body

proc makeTypedProcCopy(prc, body, resultType: NimNode): NimNode =
  let body = copyNimTree(body)
  let asyncTypedProcMarker = ident"<yasyncTypedProcMarker>"
  let bd = quote do:
    when `resultType` isnot void:
      {.push warning[ResultShadowed]: off.}
      var result {.used.}: `resultType`
      {.pop.}
    var `asyncTypedProcMarker` {.used, inject.}: int
    `body`

  let params = newTree(nnkFormalParams, prc.params[0])
  for i, n, t, d in arguments(prc.params):
    if not isGenericArgType(t):
      params.add(newIdentDefs(n, t, d))

  result = newTree(nnkLambda,
                   newEmptyNode(),
                   newEmptyNode(),
                   newEmptyNode(),
                   copyNimTree(prc.params),
                   newEmptyNode(),
                   newEmptyNode(),
                   bd)

proc collectSubstate(n, stateObj: NimNode) =
  if n.kind == nnkCall and n[0].kind == nnkSym:
    let data = asyncData.getOrDefault(n[0])
    if data.envType != nil:
      let i = stateObj.len - 1
      let subId = ident("sub" & $i)
      stateObj.add newTree(nnkOfBranch, newLit(i), newIdentDefs(subId, data.envType))

proc processSubstates(n, stateObj: NimNode) =
  for i in 0 ..< n.len:
    processSubstates(n[i], stateObj)

  if n.kind == nnkCall and n[0].kind == nnkSym and $n[0] == "dummyAwaitMarkerMagic":
    collectSubstate(n[1], stateObj)

macro makeSubstates(a: typed): untyped =
  let objStateRecCase = newTree(nnkRecCase, newIdentDefs(ident"sub", ident"uint8"))
  objStateRecCase.add newTree(nnkOfBranch, newLit(0), newIdentDefs(ident"tmpFut", bindSym"FutureBase"))
  processSubstates(a.body, objStateRecCase)
  objStateRecCase.add newTree(nnkElse, newNilLit())
  result = newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), newTree(nnkRecList, objStateRecCase))

proc asyncProc(prc: NimNode, isCapture: bool): NimNode =
  result = prc
  let prcName = prc.name
  let name = if prcName.kind == nnkEmpty: ":anonymous" else: $prcName
  let iterPtrName = "yasync_iterPtr_" & name
  let iterSym = genSym(nskIterator, name & ":iter")

  var resultType = prc.params[0] or ident"void"
  prc.params[0] = newTree(nnkBracketExpr, bindSym"Future", resultType)

  let hSym = ident"<h>"
  let resultSym = ident"result"

  let argDefs = makeArgDefs(prc)
  let body1 = fixupLastReturnStmt(prc.body)
  let body = transformReturnStmt(body1, resultSym)
  let typedProcCopy = makeTypedProcCopy(prc, body, resultType)

  let dummySelfCall = makeDummySelfCall(prc)
  let asyncProcBody = makeAsyncProcBody(prc, iterSym, isCapture)
  let subIdent = ident"<yasyncSubstates>"
  let iterPtrName2 = ident"iterPtrName"
  result.body = quote do:
    const `iterPtrName2` = genIterPtrName(`iterPtrName`)
    type Substates = makeSubstates(`typedProcCopy`)
    iterator `iterSym`() {.closure, exportc: `iterPtrName2`.} =
      when canLiftLocals:
        var `hSym` {.noinit, liftLocals.}: ContHeader
      else:
        var `hSym` {.noinit.}: ContHeader
        keepFromReordering(`hSym`)

      when `resultType` isnot void:
        {.push warning[ResultShadowed]: off.}
        when canLiftLocals:
          var `resultSym` {.used, liftLocals.}: `resultType`
        else:
          var `resultSym` {.used.}: `resultType`
          keepFromReordering(`resultSym`)
        {.pop.}
      when canLiftLocals:
        var `subIdent` {.noinit, used, inject, liftLocals.}: Substates
      else:
        var `subIdent` {.noinit, used, inject.}: Substates
        keepFromReordering(`subIdent`)

      setSubstatesOffset(`hSym`, `resultType`, Substates)

      when not `isCapture`:
        `argDefs`

      `body`

  if prc.kind in {nnkProcDef, nnkMethodDef} and not isCapture:
    result.body.add quote do:
      registerAsyncData(`dummySelfCall`, `iterPtrName2`, `iterSym`)
      proc dummy() {.used.} =
        # Workaround nim bug. Without this proc nim sometimes fails
        # to instantiate waitFor code. This bug is not demonstrated
        # in the tests.
        var e: asyncCallEnvType(`dummySelfCall`)
        discard typeof(read(e)) is void

  result.body.add(asyncProcBody)

proc rawRetType[T](t: typedesc[Cont[T]]): Future[T] = discard

macro asyncRaw*(prc: untyped{nkProcDef}): untyped =
  ## used to define a low-level async procedure
  ## that works with a pre-allocated env. The last parameter
  ## to such procedure must be a pointer to a derivative of `Cont[T]`
  ## type, where `T` is async return type. The actual return type
  ## must be void, regardless of `T`.
  ##
  ## Example 1:
  ## proc sleep(milliseconds: int, env: ptr Cont[void]) {.asyncRaw.} =
  ##   asyncdispatch.addCallback(asyncdispatch.sleepAsync(ms)) do():
  ##     env.complete()
  ## # Works as if it was `proc sleep(ms: int) {.async.}`
  ## waitFor sleep(5)
  ##
  ## Example 2:
  ## proc fetchUrl(url: string, env: ptr Cont[string]) {.asyncRaw.} =
  ##   someHttpClient.onComplete = proc(contents: string) =
  ##     env.complete(contents)
  ##   someHttpClient.startFetch(url)
  ## # Works as if it was `proc fetchUrl(url: string): string {.async.}`
  ## echo waitFor fetch("http://example.com")
  ##
  ## Defining custom Env type (must derived from `Cont[T]`!) may be beneficial
  ## to store contextual data in the environment, avoiding extra allocations
  ##
  ## Example 3:
  ## type AllocationFreeEnv = object of Cont[int]
  ##   someNumber: int
  ##
  ## proc startSomeComputation(context: pointer, callback: proc(calcResult: int, context: pointer) {.cdecl.})
  ##
  ## proc myCallback(computationResult: int, context: pointer) {.cdecl.} =
  ##   echo "Computation complete. Adding the number now..."
  ##   let env = cast[ptr AllocationFreeEnv](context)
  ##   env.complete(env.someNumber + computationResult)
  ##
  ## proc myEfficientComputationPlusSomeNumber(someNumber: int, env: ptr AllocationFreeEnv) {.asyncRaw.} =
  ##   env.someNumber = someNumber
  ##   startSomeComputationApi(env, myCallback)
  ##
  ## # Works as if it was `proc myEfficientComputationPlusSomeNumber(someNumber: int): int {.async.}`
  ## echo waitFor myEfficientComputationPlusSomeNumber(5)


  let innerPrc = copyNimTree(prc)
  let prcName = prc.name.basename
  innerPrc[0] = genSym(nskProc, $prcName) # reset stars in name
  innerPrc.params[0] = newEmptyNode()
  innerPrc[2] = newEmptyNode() # reset generic params
  let prms = prc.params
  var lastArgType = prms[^1][^2]
  prms.del(prms.len - 1)

  let retType = ident"auto"
  if prms[0].kind == nnkEmpty:
    prms[0] = retType

  let innerCall = newCall(innerPrc.name)
  let envSym = ident"env"

  for i, n, t, d in arguments(prc.params):
    innerCall.add(n)
  innerCall.add quote do:
    cast[`lastArgType`](`envSym`)
  result = prc

  let dummySelfCall = makeDummySelfCall(prc)
  let iterPtrCName = ident"iterPtrCName"
  let rawProcPtrName = "yasync_raw_" & $prcName

  innerPrc.addPragma(ident"nimcall")
  innerPrc.addPragma(newTree(nnkExprColonExpr, ident"exportc", iterPtrCName))

  prc.body = quote do:
    type Env = typeof(default(`lastArgType`)[])
    if false: return rawRetType(Env) # Make nim infer return type
    const `iterPtrCName` = genIterPtrName(`rawProcPtrName`)
    `innerPrc`
    registerAsyncRawData(`dummySelfCall`, `iterPtrCName`, Env)
    var `envSym`: ref Env
    `envSym`.new()
    GC_ref(`envSym`)
    markAllocatedEnv(cast[ptr ContBase](`envSym`))
    `innerCall`
    return `envSym`

macro async*(prc: untyped): untyped =
  case prc.kind
  of nnkProcDef, nnkMethodDef, nnkDo: asyncProc(prc, false)
  else:
    echo treeRepr(prc)
    assert(false)
    nil

macro asyncClosureExperimental*(prc: untyped): untyped =
  case prc.kind
  of nnkProcDef, nnkMethodDef, nnkDo: asyncProc(prc, true)
  else:
    echo treeRepr(prc)
    assert(false)
    nil

macro getStateIdxAux(substates: object, state: typedesc): untyped =
  let recCase = getType(substates)[2][0]
  # echo "SUB: ", treeRepr(recCase)
  # echo "STATE: ", getType(state).treeRepr
  for i in 2 ..< recCase.len - 1:
    let s = recCase[i][1]
    let t = getType(s)
    if sameType(t, getType(state)[1]):
      return recCase[i][0]
  assert(false, "yasync internal error")

template getStateIdx[T](substates: object, state: typedesc[AsyncEnv[T]]): untyped =
  getStateIdxAux(substates, T)

macro subAccess(sub: untyped, idx: static[int]): untyped =
  newDotExpr(sub, ident("sub" & $idx))

proc substateAtIndex(sub: var object, i: static[uint8]): auto {.silent.} =
  const i = i.int
  when defined(yasyncDebug):
    return addr subAccess(sub, i)
  else:
    {.push fieldChecks: off.}
    return addr subAccess(sub, i)
    {.pop.}

proc tmpFutSubstate[T](sub: var T): var FutureBase {.silent.} =
  when defined(yasyncDebug):
    return sub.tmpFut
  else:
    {.push fieldChecks: off.}
    return sub.tmpFut
    {.pop.}

proc resetSubstate[T](s: var T, idx: uint8) {.silent.} =
  s = T(sub: idx)

proc setTmpFutSubstate[T](sub: var T, f: FutureBase) {.silent.} =
  sub.resetSubstate(0)
  sub.tmpFutSubstate() = f

macro fillArgs(subAccess: untyped, n: typed): untyped =
  result = newNimNode(nnkStmtList)
  let prc = n[0]
  let typ = getTypeImpl(prc)
  var pi = 0
  for i, _, t, d in arguments(typ.params):
    if not isGenericArgType(t):
      result.add newCall(bindSym"fillArg", subAccess, newLit(pi), n[i + 1])
      inc pi

proc checkVarDeclared[T](a: var T) = discard

macro makeRawCall(call: typed, env: typed): untyped =
  let d = asyncData[call[0]]
  assert(d.procPtrName.startsWith("yasync_raw_"))

  let typ = getTypeImpl(call[0])[0]
  let prc = newProc(ident"inner")
  let prms = newTree(nnkFormalParams, ident"void")
  let innerCall = newCall("inner")

  for i, _, t, d in arguments(typ):
    prms.add(newIdentDefs(ident("arg" & $i ), t))
    innerCall.add(call[i + 1])

  prms.add(newIdentDefs(ident"env", newCall("typeof", env)))
  innerCall.add(env)
  prc.params = prms
  prc.addPragma(ident"nimcall")
  prc.addPragma(newTree(nnkExprColonExpr, ident"importc", newLit(d.procPtrName)))
  result = quote do:
    block:
      `prc`
      `innerCall`

macro awaitSubstateImpl(substates: typed, Env: typedesc, f: ref Cont, thisEnv: untyped): untyped =
  let procPtrName = asyncCallProcPtrNameAux(f)
  let setupEnv = genSym(nskProc, "setupEnv")
  let stateIdx = ident"stateIdx"
  let pEnv = ident"pEnv"
  let subs = ident"subs"

  let wrapperDef = quote do:
    proc `setupEnv`(substates: var object) {.stacktrace: off, inline.} =
      substates.resetSubstate(`stateIdx`)
      let `pEnv` = substateAtIndex(substates, `stateIdx`)

  let wrapperCall = newCall(setupEnv, substates)
  let wrapperParams = wrapperDef.params
  let wrapperBody = wrapperDef.body
  let isRaw = procPtrName.startsWith("yasync_raw_")
  var rawProcDef, rawProcCall, rawProcParams: NimNode
  let callTyp = getTypeImpl(f[0])[0]

  if isRaw:
    let rawProc = genSym(nskProc, "rawProc")
    rawProcCall = newCall(rawProc)
    rawProcDef = quote do:
      proc `rawProc`() {.nimcall, importc: `procPtrName`.}
    rawProcParams = rawProcDef.params
  else:
    wrapperBody.add quote do:
      proc iterPtr(e: pointer) {.nimcall, gcsafe, importc: `procPtrName`.}
      setProc(getHeader(`pEnv`[]), iterPtr)

  for i, _, t, d in arguments(callTyp):
    let argId = ident("arg" & $i)
    wrapperParams.add newIdentDefs(argId, t)
    wrapperCall.add(f[i + 1])
    if isRaw:
      rawProcParams.add newIdentDefs(argId, t)
      rawProcCall.add(argId)
    else:
      wrapperBody.add(newCall(bindSym"fillArgPtr", pEnv, newLit(i), argId))

  if isRaw:
    rawProcParams.add newIdentDefs(ident"env", newCall("typeof", `subs`))
    rawProcCall.add(pEnv)
    wrapperBody.add(rawProcDef)
    wrapperBody.add(rawProcCall)

  result = quote do:
    const `stateIdx` = getStateIdx(`substates`, `Env`)
    template `subs`: untyped =
      substateAtIndex(`substates`, `stateIdx`)
    block:
      `wrapperDef`
      `wrapperCall`

  if isRaw:
    result.add quote do:
      if not `subs`.finished:
        getHeader(`subs`[]).e = `thisEnv`
        yield
      read(`subs`[])
  else:
    result.add quote do:
      if launchf(cast[ptr ContBase](`subs`)):
        getHeader(`subs`[]).e = `thisEnv`
        yield

      checkFinished(cast[ptr ContBase](`subs`))
      readAux(`subs`[])

template await*[T](f: Future[T]): T =
  when compiles(checkVarDeclared(`<yasyncSubstates>`)):
    if false: discard f
    block:
      type Env = asyncCallEnvType(f)
      when Env is void:
        `<yasyncSubstates>`.setTmpFutSubstate(f)
        if not `<yasyncSubstates>`.tmpFutSubstate.finished:
          `<yasyncSubstates>`.tmpFutSubstate.h.e = thisEnv(`<h>`)
          yield
        cast[Future[T]](`<yasyncSubstates>`.tmpFutSubstate).read()
      else:
        awaitSubstateImpl(`<yasyncSubstates>`, Env, f, thisEnv(`<h>`))
  elif compiles(checkVarDeclared(`<yasyncTypedProcMarker>`)):
    dummyAwaitMarkerMagic(f)
  else:
    {.error: "await can only be used inside async function".}

template asyncLaunchWithEnv*(aenv: var AsyncEnv, call: FutureBase{nkCall}) =
  block:
    if false:
      discard call
    const procPtrName = asyncCallProcPtrName(call)
    when procPtrName.startsWith("yasync_raw_"):
      makeRawCall(call, addr aenv.env)
    else:
      proc iterPtr(e: pointer) {.gcsafe, nimcall, importc: procPtrName.}
      getHeader(aenv.env).p = iterPtr
      fillArgs(aenv.env, call)
      launch(cast[ptr ContBase](addr aenv.env))
