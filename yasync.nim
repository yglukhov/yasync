import macros, tables, hashes

type
  ProcType = proc(e: pointer) {.gcsafe, nimcall.}
  ContBase = object {.inheritable.}
    `<state_reserved>`: int
    `<p>`: ProcType
    `<e>2`: ptr ContBase
    `<isAllocated>3`: bool
    error: ref Exception

  Cont*[T] = object of ContBase
    when T is void:
      discard
    else:
      result: T

  Future*[T] = ref Cont[T]

proc finished(f: ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished(f: ptr ContBase): bool {.inline.} = f.`<state_reserved>` < 0
proc finished*(f: ref ContBase): bool {.inline.} = f.`<state_reserved>` < 0

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
      f(p)
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
  # resume(resFut.e)
  resume(resFut)

proc complete*(resFut: ptr Cont[void]) =
  resFut.`<state_reserved>` = -1
  # resume(resFut.`<e>2`)
  resume(resFut)

proc complete*[T](resFut: Future[T], v: T) {.inline.} =
  complete(cast[ptr Cont[T]](resFut), v)

proc complete*(resFut: Future[void]) {.inline.} =
  complete(cast[ptr Cont[void]](resFut))

proc checkFinished(resFut: ptr ContBase) =
  assert(resFut.finished)

proc read[T](resFut: Cont[T]): T =
  assert(resFut.finished)
  resFut.result

proc read(resFut: Cont[void]) =
  assert(resFut.finished)

proc read*[T](resFut: Future[T]): T =
  assert(resFut.finished)
  resFut.result

proc read*(resFut: Future[void]) =
  assert(resFut.finished)

proc readAux[T](a: T): auto {.inline.} =
  when compiles(a.result4):
    a.result4
  else:
    discard

template contSubstate(s: untyped): untyped =
  if launchf(cast[ptr ContBase](addr s)):
    yield
  checkFinished(cast[ptr ContBase](addr s))
  # when compiles(s.`<asyncresult>3`):
  readAux(s)
  # s.`<asyncresult>3`

template this_env(a: typed): ptr ContBase =
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
    # result.add(newIdentDefs(newTree(nnkPragmaExpr, ident($n), newTree(nnkPragma, ident"noinit")), t))
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
  var idx = idx + 4
  if $rl[4] == "result4": inc idx
  result = newDotExpr(o, rl[idx])

template fillArg[TEnv, TArg](e: var TEnv, idx: int, arg: TArg) =
  argFieldAccess(e, idx) = arg

template fillArgPtr[TEnv, TArg](e: ref TEnv, idx: int, arg: TArg) =
  argFieldAccess(e[], idx) = arg

proc dummyAwaitMarkerMagic[T](f: Future[T]): T = discard

proc setThisEnvToFuture(f, env: ptr ContBase) {.inline.} =
  f.`<e>2` = env

template realAwait[T](f: Future[T], thisEnv: ptr ContBase): T =
  let tmpFut = f
  if not tmpFut.finished:
    setThisEnvToFuture(cast[ptr ContBase](tmpFut), thisEnv)
    yield
  tmpFut.read()

template realAwait(f: Future[void], thisEnv: ptr ContBase) =
  let tmpFut = f
  if not tmpFut.finished:
    setThisEnvToFuture(cast[ptr ContBase](tmpFut), thisEnv)
    yield
  tmpFut.read()

proc replaceDummyAwait(n, stateObj: NimNode): NimNode =
  var optimizeAwait = false
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
    result = newCall(bindSym"realAwait", n, newCall(bindSym"thisEnv", pSym))

  assert(not result.isNil, "Internal error")

proc processAsync(n, stateObj: NimNode): NimNode =
  result = n
  for i in 0 ..< result.len:
    result[i] = processAsync(result[i], stateObj)

  if n.kind == nnkCall and n[0].kind == nnkSym and $n[0] == "dummyAwaitMarkerMagic":
    result = replaceDummyAwait(n[1], stateObj)

macro asyncClosure3(c: untyped): untyped =
  result = c
  # echo "CLOS2: ", treerepr result
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
  let subIdent = ident"sub"
  insertion.add newTree(nnkTypeSection, newTree(nnkTypeDef, ident"Substates", newEmptyNode(), newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), newTree(nnkRecList, objStateRecCase))))
  insertion.add quote do:
    var `subIdent`: Substates
  c.body[stateObjInsertionPoint] = insertion
  # echo repr objStateRecCase
  echo "CLOS2: ", repr result

macro asyncClosure2(c: typed): untyped =
  newCall(bindSym"asyncClosure3", c)

proc closureEnvType(a: NimNode): NimNode =
  let im = getImplTransformed(a)
  result = getType(im.params[^1])
  result = result[^1]

macro getClosureEnvType(a: typed): untyped =
  result = closureEnvType(a)

macro registerAsyncWrapper(prc: typed, iterSym: typed, procPtrVar: typed): untyped =
  result = prc
  asyncData[prc.name] = AsyncProcData(envType: newCall(bindSym"getClosureEnvType", iterSym), iterSym: iterSym, procPtrVar: procPtrVar)

macro asyncCallEnvType*(call: typed): untyped =
  let d = asyncData.getOrDefault(call[0])
  assert(d.envType != nil, "Env type not found")
  return d.envType

proc makeAsyncWrapper(prc, iterSym, procPtrVar: NimNode): NimNode =
  result = newCall(bindSym"registerAsyncWrapper", prc, iterSym, procPtrVar)
  let getClosureEnvType = bindSym"getClosureEnvType"
  let markAllocatedEnv = bindSym"markAllocatedEnv"
  let launchSym = bindSym"launch"
  let p1Sym = ident("<p>1")
  let envSym = ident("env")
  var retType = prc.params[0]
  if retType.kind == nnkEmpty:
    retType = ident"void"
  prc.params[0] = newTree(nnkBracketExpr, bindSym"Future", retType)
  let paramsCount = prc.params.len

  let fillArgs = newNimNode(nnkStmtList)

  # Fill arguments
  for i, n, t, d in arguments(prc.params):
    fillArgs.add newCall(bindSym"fillArgPtr", envSym, newLit(i), n)

  prc.body = quote do:
    # let `envSym` = `allocateAsyncResult`(`envSym`, result)
    var `envSym`: ref `getClosureEnvType`(`iterSym`)
    `envSym`.new()
    GC_ref(`envSym`)
    `markAllocatedEnv`(cast[ptr ContBase](`envSym`))
    result = cast[typeof(result)](`envSym`)
    `envSym`.`p1sym` = `procPtrVar`
    `fillArgs`
    `launchSym`(cast[ptr ContBase](`envSym`))

proc asyncProc(prc: NimNode): NimNode =
  let prcName = prc.name
  var data: AsyncProcData
  let procPtrVar = genSym(nskVar, $prcName & "_procPtr")
  let iterSym = genSym(nskIterator, $prcName & "_iter")
  data.procPtrVar = procPtrVar
  data.iterSym = iterSym

  var resultType = prc.params[0] or ident"void"

  let pSym = ident"<p>"
  let eSym = ident"<e>"
  let isAllocatedSym = ident"<isAllocated>"
  let resultSym = ident"result"

  let argDefs = processArguments(prc)
  let body = transformReturnStmt(prc.body, resultSym)

  let iterDecl = quote do:
    iterator `iterSym`() {.closure.} =
      var `pSym` {.noinit, used.}: ProcType
      var `eSym` {.noinit, used.}: ptr ContBase
      var `isAllocatedSym` {.noinit, used.}: bool
      when `resultType` isnot void:
        var `resultSym`: `resultType`
      `argDefs`
      ##<STATE OBJ INSERTION POINT>
      `body`

  let wrapper = makeAsyncWrapper(prc, iterSym, procPtrVar)

  result = quote do:
    var `procPtrVar`: ProcType

    asyncClosure2(`iterDecl`)

    `procPtrVar` = cast[ProcType](rawProc(`iterSym`))

    `wrapper`

  echo repr result

macro asyncRaw2(prc: typed): untyped =
  let s = prc.name
  var lastArgType = prc.params[^1][^2]
  # let ptrBase = newCall(bindSym"pointerBase", lastArgType)
  let ptrBase = lastArgType[^1]
  asyncData[s] = AsyncProcData(envType: ptrBase)
  result = prc

macro asyncRaw*(prc: untyped): untyped =
  let retType = prc.params[0] or ident"void"
  prc.params[0] = newEmptyNode()
  result = newCall(bindSym"asyncRaw2", prc)

macro async*(prc: untyped): untyped =
  case prc.kind
  of nnkProcDef: asyncProc(prc)
  else: nil

template await*[T](f: ref Cont[T]): T = dummyAwaitMarkerMagic(f)
template await*(f: ref Cont[void]) = dummyAwaitMarkerMagic(f)