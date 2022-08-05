# yasync - Yet another async/await for Nim [![Build Status](https://github.com/yglukhov/yasync/workflows/CI/badge.svg?branch=main)](https://github.com/yglukhov/yasync/actions?query=branch%3Amain)

WARNING: Doesn't work because of https://github.com/nim-lang/Nim/issues/19818, but can be tested with [this hacky patch to Nim](https://github.com/yglukhov/Nim/commit/9758922ae702e0d005756111c9fb0badc3ed90d6).

- Semantics is very close to that of Nim's std `async`, `await` and `Future[T]`.
- `await` operation doesn't cause any heap allocations (except calling manually-async functions, async closures, and (mutually) recursive calls).
- `callSoon` is not used and async code is guaranteed to run atomically between `await`s across call boundaries (TODO: maybe add an example)
- Function return type is written as `T`, then `async` transforms it to `Future[T]` implicitly.
- Provides optional compatibility layer for interop with existing `asyncdispatch` (and TODO: chronos) code. [Example](https://github.com/yglukhov/yasync/blob/main/tests/test4.nim). The library itself is completely dispatcher agnostic, and doesn't depend on neither `asyncdispatch`, nor chronos.

This library introduces `async`, `await` and `Future[T]` similar in semantics to Nim's native ones, but implements an optimization to avoid heap allocations. Consider the following sample:

```nim
proc sleep(ms: int): Future[void] = ... # Let's assume this function doesn't allocate

proc foo(a: int): int {.async.} =
  await sleep(a)
  var b = a + 1
  return b

proc bar() {.async.} =
  let a = await foo(5)
  let b = await foo(6)
  echo a + b

waitFor bar()
```

If we pretend that `sleep` doesn't allocate, the whole `waitFor` operation above will not do a single allocation. The optimization happens in `async` functions on `await` calls.

TODO: Describe how it works in details

TODO:
- [x] Recursive calls
- [x] Generics
- [ ] Cancellation
- [ ] Nice stack traces
