import macros, tables, hashes

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

template contSubstate(s: untyped): untyped =
  if launchf(cast[ptr ContBase](addr s)):
    yield
  checkFinished(cast[ptr ContBase](addr s))
  readAux(s)

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

proc keepFromReordering[T](v: T) {.inline.} =
  # This proc is used to make nim place iterator variables in its environment
  # in the same order they are defined in the iterator.
  # It was not needed until https://github.com/nim-lang/Nim/pull/22559
  discard

proc processArguments(prc: NimNode): NimNode =
  let varSection = newNimNode(nnkVarSection)
  result = newNimNode(nnkStmtList)
  result.add(varSection)
  for i, n, t, d in arguments(prc.params):
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

proc dummyAwaitMarkerMagic[T](f: Future[T]): T = discard

template realAwait[T](f: Future[T], thisEnv: ptr ContBase, tmpFut: var FutureBase): auto =
  tmpFut = f
  if not tmpFut.finished:
    tmpFut.h.e = thisEnv
    yield
  cast[Future[T]](tmpFut).read()

proc getHeader[T](env: var T): ptr ContHeader {.inline, stackTrace: off.} =
  cast[ptr ContHeader](cast[int](addr env) + offsetof(ContBase, h))

proc replaceDummyAwait(n, stateObj: NimNode, isCapture: bool): NimNode =
  let hSym = ident("<h>")
  let thisEnv = bindSym"thisEnv"
  if n.kind == nnkCall and n[0].kind == nnkSym and not isCapture:
    let data = asyncData.getOrDefault(n[0])
    if data.envType != nil:
      let i = stateObj.len - 1
      let subId = ident("sub" & $i)
      stateObj.add newTree(nnkOfBranch, newLit(i), newIdentDefs(subId, data.envType))
      let res = newNimNode(nnkStmtList)
      res.add quote do:
        sub = Substates(sub: uint8(`i`))
        getHeader(sub.`subId`).e = `thisEnv`(`hSym`)

      let procPtrName = data.procPtrName
      let envAccess = newDotExpr(ident"sub", subId)
      if procPtrName == "":
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
          block:
            proc getIterPtr(): ProcType {.nimcall, importc: `procPtrName`.}
            getHeader(sub.`subId`).p = getIterPtr()

        # Fill arguments
        for i in 1 ..< n.len:
          res.add newCall(bindSym"fillArg", envAccess, newLit(i - 1), n[i])

        res.add newCall(bindSym"contSubstate", envAccess)

      result = res

  if result.isNil:
    # State corresponding to tmpFut is 0
    let realAwait = bindSym"realAwait"
    let tmpFutId = ident"tmpFut"
    result =
      if isCapture:
        quote do:
          `realAwait`(`n`, `thisEnv`(`hSym`), `tmpFutId`)
      else:
        quote do:
          sub = Substates(sub: 0)
          `realAwait`(`n`, `thisEnv`(`hSym`), sub.tmpFut)

  assert(not result.isNil, "Internal error")

proc processAsync(n, stateObj: NimNode, isCapture: bool): NimNode =
  result = n
  for i in 0 ..< result.len:
    result[i] = processAsync(result[i], stateObj, isCapture)

  if n.kind == nnkCall and n[0].kind == nnkSym and $n[0] == "dummyAwaitMarkerMagic":
    result = replaceDummyAwait(n[1], stateObj, isCapture)

macro asyncClosure3(c: untyped, isCapture: static[bool]): untyped =
  result = c
  # echo "CLOS3: ", treerepr result
  var stateObjInsertionPoint = -1
  for i, n in c.body:
    if n.kind == nnkCommentStmt and $n == "<STATE OBJ INSERTION POINT>":
      stateObjInsertionPoint = i
      break
  assert(stateObjInsertionPoint > 0, "internal error")
  let objStateRecCase = newTree(nnkRecCase, newIdentDefs(ident"sub", ident"uint8"))
  objStateRecCase.add newTree(nnkOfBranch, newLit(0), newIdentDefs(ident"tmpFut", bindSym"FutureBase"))
  c.body = processAsync(c.body, objStateRecCase, isCapture)
  objStateRecCase.add newTree(nnkElse, newNilLit())
  let insertion = newNimNode(nnkStmtList)
  if objStateRecCase.len > 2 and not isCapture:
    # The reccase is not empty meaning there are substates
    let subIdent = ident"sub"
    insertion.add newTree(nnkTypeSection, newTree(nnkTypeDef, ident"Substates", newEmptyNode(), newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), newTree(nnkRecList, objStateRecCase))))
    insertion.add quote do:
      var `subIdent` {.used.}: Substates
  c.body[stateObjInsertionPoint] = insertion
  # echo repr objStateRecCase
  # echo "CLOS2: ", repr result

macro asyncClosure2(c: typed, isCapture: bool): untyped =
  newCall(bindSym"asyncClosure3", c, isCapture)

proc closureEnvType(a: NimNode): NimNode =
  let im = getImplTransformed(a)
  result = getType(im.params[^1])
  result = result[^1]

macro getClosureEnvType(a: typed): untyped =
  result = closureEnvType(a)

macro registerAsyncData(dummyCall: typed, procPtrName: static[string], iterSym: typed): untyped =
  asyncData[dummyCall[0]] = AsyncProcData(envType: newCall(bindSym"getClosureEnvType", iterSym), procPtrName: procPtrName)

macro asyncCallEnvType*(call: Future): untyped =
  ## Returns type of async environment for the `call`
  ## This type can be used with `asyncLaunchWithEnv`
  ## Returns `void` if the `call` can not be rewritten to `asyncLaunchWithEnv`
  if call.kind == nnkCall:
    let d = asyncData.getOrDefault(call[0])
    if d.envType != nil:
      return newTree(nnkBracketExpr, bindSym"AsyncEnv", d.envType)
  return ident"void"

template setProc(h: ptr ContHeader, prc: ProcType) = h.p = prc

var counter {.compileTime.} = 0
proc genIterPtrName(s: string): string {.compileTime.} =
  inc counter
  s & "_" & $counter

proc makeAsyncWrapper(prc, iterSym, iterDecl: NimNode, isCapture: bool): NimNode =
  result = prc
  let getClosureEnvType = bindSym"getClosureEnvType"
  let markAllocatedEnv = bindSym"markAllocatedEnv"
  let launchSym = bindSym"launch"
  let envSym = ident("env")
  let retType = prc.params[0] or ident"void"
  prc.params[0] = newTree(nnkBracketExpr, bindSym"Future", retType)

  let fillArgs = newNimNode(nnkStmtList)

  let dummySelfCall = newCall(prc.name)

  # Fill arguments
  for i, n, t, d in arguments(prc.params):
    fillArgs.add newCall(bindSym"fillArgPtr", envSym, newLit(i), n)
    dummySelfCall.add(n)

  let prcName = case prc.name.kind
                of nnkSym, nnkIdent: $prc.name
                of nnkEmpty: "anonymous"
                else: "unknown"
  let iterPtrName = "yasync_getIterPtr_" & prcName

  var namedProcRegister = newNimNode(nnkStmtList)
  if prc.kind in {nnkProcDef, nnkMethodDef} and not isCapture:
    namedProcRegister = quote do:
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


  prc.body = quote do:
    asyncClosure2(`iterDecl`, `isCapture`)

    `namedProcRegister`

    var it = `iterSym`
    when `isCapture`:
      let `envSym` = cast[ref ContBase](rawEnv(it))
    else:
      let `envSym` = cast[ref `getClosureEnvType`(`iterSym`)](rawEnv(it))

    GC_ref(`envSym`)
    `markAllocatedEnv`(cast[ptr ContBase](`envSym`))
    result = cast[typeof(result)](`envSym`)
    setProc(getHeader(`envSym`[]), cast[ProcType](rawProc(it)))
    when not `isCapture`:
      `fillArgs`
    `launchSym`(cast[ptr ContBase](`envSym`))

template assignResult[T](v: T) =
  when T is void:
    v
  else:
    result = v

proc fixupLastReturnStmt(body: NimNode): NimNode =
  if body.len > 0:
    body[^1] = newCall(bindSym"assignResult", body[^1])
  result = body

proc asyncProc(prc: NimNode, isCapture: bool): NimNode =
  let prcName = prc.name
  let name = if prcName.kind == nnkEmpty: ":anonymous" else: $prcName

  let iterSym = genSym(nskIterator, name & ":iter")

  var resultType = prc.params[0] or ident"void"

  let hSym = ident"<h>"
  let resultSym = ident"result"

  let argDefs = processArguments(prc)

  let body1 = fixupLastReturnStmt(prc.body)
  let body = transformReturnStmt(body1, resultSym)

  let tmpFutId = ident"tmpFut"

  let iterDecl = quote do:
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
      ##<STATE OBJ INSERTION POINT>
      when `isCapture`:
        var `tmpFutId`: FutureBase

      `body`

  result = makeAsyncWrapper(prc, iterSym, iterDecl, isCapture)

macro asyncLaunchWithEnv*(env: var AsyncEnv, call: typed{nkCall}): untyped =
  call.expectKind(nnkCall)
  let s = call[0]
  s.expectKind(nnkSym)
  let data = asyncData[s]
  let iterPtrName = data.procPtrName
  let fillArgs = newNimNode(nnkStmtList)
  let envAccess = newTree(nnkDotExpr, env, ident"env")

  # Fill arguments
  for i in 1 ..< call.len:
    fillArgs.add newCall(bindSym"fillArg", envAccess, newLit(i - 1), call[i])
  result = quote do:
    block:
      block:
        proc getIterPtr(): ProcType {.nimcall, importc: `iterPtrName`.}
        getHeader(`envAccess`).p = getIterPtr()
      `fillArgs`
      launch(cast[ptr ContBase](addr `envAccess`))

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


template await*[T](f: ref Cont[T]): T =
  if false:
    discard f
  dummyAwaitMarkerMagic(f)

template await*(f: ref Cont[void]) =
  if false:
    discard f
  dummyAwaitMarkerMagic(f)
