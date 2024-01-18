import std/[macros, tables, hashes, strutils]

type
  ProcType = proc(e: pointer) {.gcsafe, nimcall.}
  ContFlags = enum
    fAllocated

  ContHeader = object
    p: ProcType
    e: ptr ContBase
    error: ref Exception
    flags: set[ContFlags]

  ContBase = object of RootObj
    `<state_reserved>`: int
    h: ContHeader

  Cont*[T] = object of ContBase
    when T is void:
      discard
    else:
      result*: T

  FutureBase* = ref ContBase
  Future*[T] = ref Cont[T]

  AsyncEnv*[T] = object
    env*: T

proc finished(f: ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished(f: ptr ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: FutureBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: AsyncEnv): bool {.inline.} = finished(cast[ptr ContBase](addr f))

proc newFuture*(T: typedesc): Future[T] =
  result.new()

proc markAllocatedEnv(e: ptr ContBase) {.inline.} =
  e.h.flags.incl(fAllocated)

proc isAllocatedEnv(e: ptr ContBase): bool {.inline.} =
  e.h.flags.contains(fAllocated)

template setStateFinished[T](a: T) =
  a.`<state_reserved>` = -1

proc resume(p: ptr ContBase) =
  {.push warning[BareExcept]: off.}
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
  {.pop.}

proc launch(p: ptr ContBase) {.inline.} =
  p.h.p(p)

proc launchf(p: ptr ContBase): bool {.inline.} =
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

proc fail(resFut: ptr ContBase, err: ref Exception) =
  resFut.setStateFinished()
  resFut.h.error = err
  resume(resFut)

proc fail*(resFut: ref ContBase, err: ref Exception) {.inline.} =
  fail(cast[ptr ContBase](resFut), err)

proc error*(f: ref ContBase): ref Exception {.inline.} = f.h.error

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
    c.h.flags.incl fAllocated
    GC_ref(c)
    c.h.p = onComplete[T]
    f.h.e = cast[ptr ContBase](c)

proc then*(f: Future[void], cb: proc(error: ref Exception) {.gcsafe.}) =
  assert(f.h.e.isNil, "Future already has a callback")
  if f.finished:
    cb(f.h.error)
  else:
    let c = CBVoid(f: f, cb: cb)
    c.h.flags.incl fAllocated
    GC_ref(c)
    c.h.p = onComplete[void]
    f.h.e = cast[ptr ContBase](c)

proc checkFinished(resFut: ptr ContBase) {.stackTrace: off.} =
  assert(resFut.finished)
  if not resFut.h.error.isNil:
    raise resFut.h.error

proc read[T](resFut: Cont[T]): T =
  checkFinished(addr resFut)
  resFut.result

proc read(resFut: Cont[void]) =
  checkFinished(addr resFut)

proc read*[T](resFut: Future[T]): T =
  checkFinished(cast[ptr ContBase](resFut))
  resFut.result

proc read*(resFut: Future[void]) =
  checkFinished(cast[ptr ContBase](resFut))

proc readAux[T](a: T): auto {.inline.} =
  when compiles(a.result2):
    a.result2
  else:
    discard

proc read*(resFut: AsyncEnv): auto =
  checkFinished(cast[ptr ContBase](addr resFut))
  readAux(resFut.env)

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
      yield (iParam, pp[j], copyNimTree(stripSinkFromArgType(pp[^2])), pp[^1])
      inc iParam

template keepFromReordering[T](v: T) =
  # This proc is used to make nim place iterator variables in its environment
  # in the same order they are defined in the iterator.
  # It was not needed until https://github.com/nim-lang/Nim/pull/22559
  discard v

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
  var idx = idx + 2
  if $rl[2] == "result2": inc idx
  result = newDotExpr(o, rl[idx])

template fillArg[TEnv, TArg](e: var TEnv, idx: int, arg: TArg) =
  argFieldAccess(e, idx) = arg

template fillArgPtr[TEnv, TArg](e: ref TEnv, idx: int, arg: TArg) =
  argFieldAccess(e[], idx) = arg

proc dummyAwaitMarkerMagic[T](f: Future[T]): T {.importc: "yasync_report_error_if_this_symbol_is_missing_on_linkage".}

proc getHeader[T](env: var T): ptr ContHeader {.inline, stackTrace: off.} =
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

macro asyncCallProcPtrName(call: Future): untyped =
  call.expectKind(nnkCall)
  let d = asyncData.getOrDefault(call[0])
  doAssert(d.envType != nil, "yasync internal error")
  newLit(d.procPtrName)

template setProc(h: ptr ContHeader, prc: ProcType) = h.p = prc

var counter {.compileTime.} = 0
proc genIterPtrName(s: string): string {.compileTime.} =
  inc counter
  s & "_" & $counter

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

proc makeAsyncProcRegistration(prc, iterSym: NimNode, isCapture: bool): NimNode =
  let dummySelfCall = makeDummySelfCall(prc)

  let prcName = case prc.name.kind
                of nnkSym, nnkIdent: $prc.name
                of nnkEmpty: "anonymous"
                else: "unknown"
  let iterPtrName = "yasync_getIterPtr_" & prcName

  result = newNimNode(nnkStmtList)
  if prc.kind in {nnkProcDef, nnkMethodDef} and not isCapture:
    result = quote do:
      const iterPtrName = genIterPtrName(`iterPtrName`)
      proc getIterPtr(): ProcType {.stacktrace: off, nimcall, exportc: iterPtrName.} =
        # The emit is a hacky optimization to avoid calling newObj
        # and eventually allow the C optimizer to collapse it
        # entirely.
        when defined(gcDestructors):
          {.emit: """
          struct {
            RootObj Sup;
          } dummy;
          #define nimNewObj(a, b) ((void*)&dummy)
          #define nimCopyMem(a, b, c)
          """.}
        else:
          {.emit: """
          struct {
            struct {
              TNimType* m_type;
            } Sup;
          } dummy;
          #define newObj(a, b) ((void*)&dummy)
          """.}

        result = cast[ProcType](rawProc(`iterSym`))
        when defined(gcDestructors):
          {.emit: """

          #undef nimNewObj
          #undef nimCopyMem
          colontmpD_ = NIM_NIL;
          """.}
        else:
          {.emit: """
          #undef newObj
          """.}

      if false:
        discard getIterPtr()

      registerAsyncData(`dummySelfCall`, iterPtrName, `iterSym`)

      proc dummy() {.used.} =
        # Workaround nim bug. Without this proc nim sometimes fails
        # to instantiate waitFor code. This bug is not demonstrated
        # in the tests.
        var e: asyncCallEnvType(`dummySelfCall`)
        when typeof(read(e)) is void:
          read(e)
        else:
          discard read(e)

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
      var result: `resultType`
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

  let iterSym = genSym(nskIterator, name & ":iter")

  var resultType = prc.params[0] or ident"void"
  prc.params[0] = newTree(nnkBracketExpr, bindSym"Future", resultType)

  let hSym = ident"<h>"
  let resultSym = ident"result"

  let argDefs = makeArgDefs(prc)
  let body1 = fixupLastReturnStmt(prc.body)
  let body = transformReturnStmt(body1, resultSym)
  let typedProcCopy = makeTypedProcCopy(prc, body, resultType)

  let namedProcRegister = makeAsyncProcRegistration(prc, iterSym, isCapture)
  let asyncProcBody = makeAsyncProcBody(prc, iterSym, isCapture)
  let subIdent = ident"<yasyncSubstates>"
  result.body = quote do:
    type Substates = makeSubstates(`typedProcCopy`)
    iterator `iterSym`() {.closure.} =
      var `hSym` {.noinit.}: ContHeader
      keepFromReordering(`hSym`)
      when `resultType` isnot void:
        {.push warning[ResultShadowed]: off.}
        var `resultSym`: `resultType`
        keepFromReordering(`resultSym`)
        {.pop.}
      when not `isCapture`:
        `argDefs`
      var `subIdent` {.used, inject.}: Substates
      `body`
    `namedProcRegister`
    `asyncProcBody`

template rawRetType(t: typedesc): typedesc = Future[typeof(read(default(t)[]))]

macro asyncRaw*(prc: untyped): untyped =
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
  ## proc startSomeComputation(context: pointer, callback: proc(c: pointer) {.cdecl.})
  ##
  ## proc myCallback(computationResult: int, env: ptr AllocationFreeEnv) {.cdecl.} =
  ##   echo "Computation complete. Adding the number now..."
  ##   env.complete(env.someNumber + computationResult)
  ##
  ## proc myEfficientComputationPlusSomeNumber(someNumber: int, env: ptr AllocationFreeEnv) =
  ##   env.someNumber = someNumber
  ##   startSomeComputationApi(env, cast[proc(c: pointer) {.cdecl.}](myCallback))
  ##
  ## # Works as if it was `proc myEfficientComputationPlusSomeNumber(someNumber: int): int {.async.}`
  ## echo waitFor myEfficientComputationPlusSomeNumber(5)


  let innerPrc = copyNimTree(prc)
  let prcName = prc.name.basename
  innerPrc[0] = prcName # reset stars in name
  let prms = prc.params
  var lastArgType = prms[^1][^2]
  prms.del(prms.len - 1)

  let retType = newCall(bindsym"rawRetType", lastArgType)
  prms[0] = retType

  let innerCall = newCall(prcName)
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
    const `iterPtrCName` = genIterPtrName(`rawProcPtrName`)
    `innerPrc`
    registerAsyncRawData(`dummySelfCall`, `iterPtrCName`, typeof(default(`lastArgType`)[]))
    var `envSym`: ref typeof(default(`lastArgType`)[])
    `envSym`.new()
    GC_ref(`envSym`)
    markAllocatedEnv(cast[ptr ContBase](`envSym`))
    `innerCall`
    return cast[typeof(result)](`envSym`)

  # echo repr(result)

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

proc substateAtIndex[R](sub: var object, i: static[int]): var R {.stacktrace: off, linetrace: off, inline.} =
  when defined(yasyncDebug):
    return subAccess(sub, i)
  else:
    {.push fieldChecks: off.}
    return subAccess(sub, i)
    {.pop.}

proc tmpFutSubstate[T](sub: var T): var FutureBase {.stacktrace: off, linetrace: off, inline.} =
  when defined(yasyncDebug):
    return sub.tmpFut
  else:
    {.push fieldChecks: off.}
    return sub.tmpFut
    {.pop.}

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

proc resetSubstate[T](s: var T, idx: uint8) {.inline, stackTrace: off, lineTrace: off.} =
  s = T(sub: idx)

macro makeRawCall(call: typed, env: typed): untyped =
  let d = asyncData[call[0]]
  assert(d.procPtrName.startsWith("yasync_raw_"))

  let typ = getType(call[0])
  let prc = newProc(ident"inner")
  let prms = newTree(nnkFormalParams, ident"void")
  let innerCall = newCall("inner")
  for i in 2 ..< typ.len:
    prms.add(newIdentDefs(ident("arg" & $i ), typ[i]))
    innerCall.add(call[i - 1])

  prms.add(newIdentDefs(ident"env", newCall("typeof", env)))
  innerCall.add(env)
  prc.params = prms
  prc.addPragma(ident"nimcall")
  prc.addPragma(newTree(nnkExprColonExpr, ident"importc", newLit(d.procPtrName)))
  result = quote do:
    block:
      `prc`
      `innerCall`

template await*[T](f: ref Cont[T]): T =
  if false:
    discard f

  when compiles(checkVarDeclared(`<yasyncSubstates>`)):
    block:
      type Env = asyncCallEnvType(f)
      when Env is void:
        `<yasyncSubstates>`.resetSubstate(0)
        `<yasyncSubstates>`.tmpFutSubstate() = f
        if not `<yasyncSubstates>`.tmpFutSubstate.finished:
          `<yasyncSubstates>`.tmpFutSubstate.h.e = thisEnv(`<h>`)
          yield
        cast[Future[T]](`<yasyncSubstates>`.tmpFutSubstate).read()
      else:
        const stateIdx = getStateIdx(`<yasyncSubstates>`, Env).int
        const procPtrName = asyncCallProcPtrName(f)
        `<yasyncSubstates>`.resetSubstate(stateIdx.uint8)
        template subs: untyped =
          substateAtIndex[typeof(Env.env)](`<yasyncSubstates>`, stateIdx)

        getHeader(subs).e = thisEnv(`<h>`)
        when procPtrName.startsWith("yasync_raw_"):
          makeRawCall(f, addr subs)
          if not subs.finished:
            yield
        else:
          proc getIterPtr(): ProcType {.nimcall, importc: procPtrName.}
          getHeader(subs).p = getIterPtr()
          fillArgs(subs, f)

          if launchf(cast[ptr ContBase](addr subs)):
            yield
          checkFinished(cast[ptr ContBase](addr subs))
          readAux(subs)
  elif compiles(checkVarDeclared(`<yasyncTypedProcMarker>`)):
    dummyAwaitMarkerMagic(f)
  else:
    {.error: "await can only be used inside async function".}

template asyncLaunchWithEnv*(aenv: var AsyncEnv, call: FutureBase{nkCall}) =
  block:
    const procPtrName = asyncCallProcPtrName(call)
    when procPtrName.startsWith("yasync_raw_"):
      makeRawCall(call, addr aenv.env)
    else:
      proc getIterPtr(): ProcType {.nimcall, importc: procPtrName.}
      getHeader(aenv.env).p = getIterPtr()
      fillArgs(aenv.env, call)
      launch(cast[ptr ContBase](addr aenv.env))
