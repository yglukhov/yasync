import macros, tables, hashes

type
  ProcType = proc(e: pointer) {.gcsafe, nimcall.}
  ContBase {.inheritable.} = object# {.inheritable.}
    `<state_reserved>`: int
    `<p>`: ProcType
    `<e>2`: ptr ContBase
    `<isAllocated>3`: bool
    error: ref Exception

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
proc finished*(f: ref ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: AsyncEnv): bool {.inline.} = cast[ptr ContBase](addr f).`<state_reserved>` < 0

proc newFuture*(T: typedesc): Future[T] =
  result.new()

proc markAllocatedEnv(e: ptr ContBase) {.inline.} =
  e.`<isAllocated>3` = true

proc isAllocatedEnv(e: ptr ContBase): bool {.inline.} =
  e.`<isAllocated>3`

proc resume(p: ptr ContBase) =
  var p = p
  while not p.isNil:
    if p.finished:
      let p1 = p.`<e>2`
      if isAllocatedEnv(p):
        GC_unref(cast[ref ContBase](p))

      p = p1
    else:
      let f = p.`<p>`
      if f.isNil:
        break
      try:
        f(p)
      except Exception as e:
        # Closure iterators throwing exceptions do not necessarily have finished state
        # so fix it here
        p.`<state_reserved>` = -1
        p.error = e
      if not p.finished:
        break

proc launch(p: ptr ContBase) {.inline.} =
  p.`<p>`(p)

proc launchf(p: ptr ContBase): bool {.inline.} =
  launch(p)
  not finished(p)

proc complete*[T](resFut: ptr Cont[T], v: T) =
  resFut.`<state_reserved>` = -1
  resFut.result = v
  resume(resFut)

proc complete*(resFut: ptr Cont[void]) =
  resFut.`<state_reserved>` = -1
  resume(resFut)

proc complete*[T](resFut: Future[T], v: T) {.inline.} =
  complete(cast[ptr Cont[T]](resFut), v)

proc complete*(resFut: Future[void]) {.inline.} =
  complete(cast[ptr Cont[void]](resFut))

proc fail(resFut: ptr ContBase, err: ref Exception) =
  resFut.`<state_reserved>` = -1
  resFut.error = err
  resume(resFut)

proc fail*(resFut: ref ContBase, err: ref Exception) {.inline.} =
  fail(cast[ptr ContBase](resFut), err)

type
  CB[T] = ref object of ContBase
    cb: proc(v: T, error: ref Exception)
    f: Future[T]

  CBVoid = ref object of ContBase
    cb: proc(error: ref Exception)
    f: Future[void]

proc onComplete[T](p: ptr ContBase) =
  when T is void:
    let p = cast[CBVoid](p)
    p.cb(p.f.error)
  else:
    let p = cast[CB[T]](p)
    p.cb(p.f.result, p.f.error)
  GC_unref(p)

proc then*[T](f: Future[T], cb: proc(v: T, error: ref Exception)) =
  assert(f.`<e>2`.isNil, "Future already has a callback")
  var c = CB[T](f: f, cb: cb)
  GC_ref(c)
  c.`<p>` = cast[ProcType](onComplete[T])
  f.`<e>2` = cast[ptr ContBase](c)

proc then*(f: Future[void], cb: proc(error: ref Exception)) =
  assert(f.`<e>2`.isNil, "Future already has a callback")
  var c = CBVoid(f: f, cb: cb)
  GC_ref(c)
  c.`<p>` = cast[ProcType](onComplete[void])
  f.`<e>2` = cast[ptr ContBase](c)

proc checkFinished(resFut: ptr ContBase) {.stackTrace: off.} =
  assert(resFut.finished)
  if not resFut.error.isNil:
    raise resFut.error

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
  when compiles(a.result5):
    a.result5
  else:
    discard

proc read*(resFut: AsyncEnv): auto =
  checkFinished(cast[ptr ContBase](addr resFut))
  readAux(resFut.env)

template contSubstate(s: untyped): untyped =
  if launchf(cast[ptr ContBase](addr s)):
    yield
  checkFinished(cast[ptr ContBase](addr s))
  readAux(s)

template thisEnv(a: typed): ptr ContBase =
  cast[ptr ContBase](cast[int](addr(a)) - sizeof(int) * 2)

type
  AsyncProcData = object
    procPtrVar: NimNode # Symbol of proc ptr
    iterSym: NimNode
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

proc processArguments(prc: NimNode): NimNode =
  result = newNimNode(nnkVarSection)
  for i, n, t, d in arguments(prc.params):
    result.add(newIdentDefs(newTree(nnkPragmaExpr, n, newTree(nnkPragma, ident"noinit")), t))

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
  var idx = idx + 5
  if $rl[5] == "result5": inc idx
  result = newDotExpr(o, rl[idx])

template fillArg[TEnv, TArg](e: var TEnv, idx: int, arg: TArg) =
  argFieldAccess(e, idx) = arg

template fillArgPtr[TEnv, TArg](e: ref TEnv, idx: int, arg: TArg) =
  argFieldAccess(e[], idx) = arg

proc dummyAwaitMarkerMagic[T](f: Future[T]): T = discard

proc setThisEnvToFuture(f, env: ptr ContBase) {.inline.} =
  f.`<e>2` = env

template realAwait[T](f: Future[T], thisEnv: ptr ContBase, tmpFut: var FutureBase): T =
  tmpFut = f
  if not tmpFut.finished:
    setThisEnvToFuture(cast[ptr ContBase](tmpFut), thisEnv)
    yield
  cast[Future[T]](tmpFut).read()

template realAwait(f: Future[void], thisEnv: ptr ContBase, tmpFut: var FutureBase) =
  tmpFut = f
  if not tmpFut.finished:
    setThisEnvToFuture(cast[ptr ContBase](tmpFut), thisEnv)
    yield
  cast[Future[void]](tmpFut).read()

proc replaceDummyAwait(n, stateObj: NimNode): NimNode =
  let pSym = ident("<p>")
  if n.kind == nnkCall and n[0].kind == nnkSym:
    let data = asyncData.getOrDefault(n[0])
    if data.envType != nil:
      let i = stateObj.len - 1
      let subId = ident("sub" & $i)
      stateObj.add newTree(nnkOfBranch, newLit(i), newIdentDefs(subId, data.envType))
      let res = newNimNode(nnkStmtList)
      let p1Sym = ident("<p>1")
      let eSym = ident("<e>2")
      res.add quote do:
        sub = Substates(sub: `i`)
        sub.`subId`.`eSym` = thisEnv(`pSym`)

      let procPtrVar = data.procPtrVar
      let envAccess = newDotExpr(ident"sub", subId)
      if procPtrVar == nil:
        # Call raw
        # Replace last parameter with addr env
        n[^1] = newCall("addr", envAccess)
        result = newTree(nnkStmtList,
                              n,
                              newTree(nnkYieldStmt, newEmptyNode()),
                              newCall(bindSym"read", envAccess))
        res.add(result)
      else:
        # Call async.
        res.add quote do:
          sub.`subId`.`p1Sym` = `procPtrVar`

        # Fill arguments
        for i in 1 ..< n.len:
          res.add newCall(bindSym"fillArg", envAccess, newLit(i - 1), n[i])

        res.add newCall(bindSym"contSubstate", envAccess)

      result = res

  if result.isNil:
    # Find state corresponding to tmpFut
    var state = -1
    for i, n in stateObj:
      if n.kind == nnkOfBranch and $n[1][0] == "tmpFut":
        state = i - 1
    if state < 0:
      state = stateObj.len - 1
      stateObj.add newTree(nnkOfBranch, newLit(state), newIdentDefs(ident"tmpFut", bindSym"FutureBase"))

    let realAwait = bindSym"realAwait"
    let thisEnv = bindSym"thisEnv"
    result = quote do:
      sub = Substates(sub: `state`)
      `realAwait`(`n`, `thisEnv`(`pSym`), sub.tmpFut)

  assert(not result.isNil, "Internal error")

proc processAsync(n, stateObj: NimNode): NimNode =
  result = n
  for i in 0 ..< result.len:
    result[i] = processAsync(result[i], stateObj)

  if n.kind == nnkCall and n[0].kind == nnkSym and $n[0] == "dummyAwaitMarkerMagic":
    result = replaceDummyAwait(n[1], stateObj)

macro asyncClosure3(c: untyped): untyped =
  result = c
  # echo "CLOS3: ", treerepr result
  var stateObjInsertionPoint = -1
  for i, n in c.body:
    if n.kind == nnkCommentStmt and $n == "<STATE OBJ INSERTION POINT>":
      stateObjInsertionPoint = i
      break
  assert(stateObjInsertionPoint > 0, "internal error")
  let objStateRecCase = newTree(nnkRecCase, newIdentDefs(ident"sub", ident"uint8"))
  c.body = processAsync(c.body, objStateRecCase)
  objStateRecCase.add newTree(nnkElse, newNilLit())
  let insertion = newNimNode(nnkStmtList)
  if objStateRecCase.len > 2:
    # The reccase is not empty meaning there are substates
    let subIdent = ident"sub"
    insertion.add newTree(nnkTypeSection, newTree(nnkTypeDef, ident"Substates", newEmptyNode(), newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), newTree(nnkRecList, objStateRecCase))))
    insertion.add quote do:
      var `subIdent`: Substates
  c.body[stateObjInsertionPoint] = insertion
  # echo repr objStateRecCase
  # echo "CLOS2: ", repr result

macro asyncClosure2(c: typed): untyped =
  newCall(bindSym"asyncClosure3", c)

proc closureEnvType(a: NimNode): NimNode =
  let im = getImplTransformed(a)
  result = getType(im.params[^1])
  result = result[^1]

macro getClosureEnvType(a: typed): untyped =
  result = closureEnvType(a)

macro registerAsyncData(dummyCall: typed, procPtr: typed, iterSym: typed): untyped =
  asyncData[dummyCall[0]] = AsyncProcData(envType: newCall(bindSym"getClosureEnvType", iterSym), iterSym: iterSym, procPtrVar: procPtr)

macro asyncCallEnvType*(call: Future): untyped =
  ## Returns type of async environment for the `call`
  ## This type can be used with `asyncLaunchWithEnv`
  ## Returns `void` if the `call` can not be rewritten to `asyncLaunchWithEnv`
  if call.kind == nnkCall:
    let d = asyncData.getOrDefault(call[0])
    if d.envType != nil:
      return newTree(nnkBracketExpr, bindSym"AsyncEnv", d.envType)
  return ident"void"

proc makeAsyncWrapper(prc, iterSym, procPtrVar, iterDecl: NimNode): NimNode =
  result = prc
  let getClosureEnvType = bindSym"getClosureEnvType"
  let markAllocatedEnv = bindSym"markAllocatedEnv"
  let launchSym = bindSym"launch"
  let p1Sym = ident("<p>1")
  let envSym = ident("env")
  var retType = prc.params[0]
  if retType.kind == nnkEmpty:
    retType = ident"void"
  prc.params[0] = newTree(nnkBracketExpr, bindSym"Future", retType)

  let fillArgs = newNimNode(nnkStmtList)

  let dummySelfCall = newCall(prc.name)

  # Fill arguments
  for i, n, t, d in arguments(prc.params):
    fillArgs.add newCall(bindSym"fillArgPtr", envSym, newLit(i), n)
    dummySelfCall.add(n)

  let getIterPtr = genSym(nskProc, "getIterPtr")

  prc.body = quote do:
    asyncClosure2(`iterDecl`)
    var iterPtr {.global.} = cast[ProcType](rawProc(`iterSym`))
    proc `getIterPtr`(): ProcType {.stackTrace: off.} =
      return iterPtr
    registerAsyncData(`dummySelfCall`, `getIterPtr`(), `iterSym`)

    var `envSym`: ref `getClosureEnvType`(`iterSym`)
    `envSym`.new()
    GC_ref(`envSym`)
    `markAllocatedEnv`(cast[ptr ContBase](`envSym`))
    result = cast[typeof(result)](`envSym`)
    `envSym`.`p1sym` = `getIterPtr`()
    `fillArgs`
    `launchSym`(cast[ptr ContBase](`envSym`))

proc asyncProc(prc: NimNode): NimNode =
  let prcName = prc.name
  let procPtrVar = genSym(nskVar, $prcName & ":procPtr")
  let iterSym = genSym(nskIterator, $prcName & ":iter")

  var resultType = prc.params[0] or ident"void"

  let pSym = ident"<p>"
  let eSym = ident"<e>"
  let isAllocatedSym = ident"<isAllocated>"
  let errorSym = ident"<error>"
  let resultSym = ident"result"

  let argDefs = processArguments(prc)
  let body = transformReturnStmt(prc.body, resultSym)

  let iterDecl = quote do:
    iterator `iterSym`() {.closure.} =
      var `pSym` {.noinit, used.}: ProcType
      var `eSym` {.noinit, used.}: ptr ContBase
      var `isAllocatedSym` {.noinit, used.}: bool
      var `errorSym` {.noinit, used.}: ref Exception
      when `resultType` isnot void:
        {.push warning[ResultShadowed]: off.}
        var `resultSym`: `resultType`
        {.pop.}
      `argDefs`
      ##<STATE OBJ INSERTION POINT>
      `body`

  result = makeAsyncWrapper(prc, iterSym, procPtrVar, iterDecl)

macro asyncLaunchWithEnv*(env: var AsyncEnv, call: typed{nkCall}): untyped =
  call.expectKind(nnkCall)
  let s = call[0]
  s.expectKind(nnkSym)
  let data = asyncData[s]
  let iterPtr = data.procPtrVar
  let fillArgs = newNimNode(nnkStmtList)
  let p1Sym = ident("<p>1")
  let launchSym = bindSym"launch"
  let envAccess = newTree(nnkDotExpr, env, ident"env")

  # Fill arguments
  for i in 1 ..< call.len:
    fillArgs.add newCall(bindSym"fillArg", envAccess, newLit(i - 1), call[i])
  result = quote do:
    block:
      `envAccess`.`p1sym` = `iterPtr`
      `fillArgs`
      `launchSym`(cast[ptr ContBase](addr `envAccess`))

macro asyncRaw2(prc: typed): untyped =
  let s = prc.name
  var lastArgType = prc.params[^1][^2]
  let ptrBase = lastArgType[^1]
  asyncData[s] = AsyncProcData(envType: ptrBase)
  result = prc

macro asyncRaw*(prc: untyped): untyped =
  prc.params[0] = newEmptyNode()
  result = newCall(bindSym"asyncRaw2", prc)

macro async*(prc: untyped): untyped =
  case prc.kind
  of nnkProcDef: asyncProc(prc)
  else: nil

template await*[T](f: ref Cont[T]): T =
  if false:
    discard f
  dummyAwaitMarkerMagic(f)

template await*(f: ref Cont[void]) =
  if false:
    discard f
  dummyAwaitMarkerMagic(f)
