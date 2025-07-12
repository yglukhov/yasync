# Package

version       = "0.1.0"
author        = "Yuriy Glukhov"
description   = "Yet another async/await for Nim"
license       = "MIT"


# Dependencies

requires "nim >= 2.2"

dev:
  requires "chronos"

iterator cartesianProduct[T](args: varargs[seq[T]]): seq[T] =
  ## Yields all combinations (Cartesian product) from varargs of seq[string]
  let n = args.len
  assert(n > 0)

  var indices = newSeq[int](n)
  var exhausted = false

  var combo = newSeqOfCap[T](args.len)
  while not exhausted:
    # Build current combination
    combo.setLen(0)
    for i in 0..<n:
      combo.add args[i][indices[i]]
    yield combo

    # Advance indices
    var pos = n - 1
    while true:
      indices[pos] += 1
      if indices[pos] < args[pos].len:
        break
      indices[pos] = 0
      if pos == 0:
        exhausted = true
        break
      dec pos

import std/[oswalkdir, strutils, os]

task test, "Run tests":
  for f in oswalkdir.walkDir("tests"):
    # Run all nim modules starting with "t"
    let sf = f.path.splitFile()
    if sf.ext == ".nim" and sf.name.startsWith("t"):
      for args in cartesianProduct(
          @["c", "cpp"],
          # @["c"],
          @["--mm:refc", "--mm:orc"],
          # @["--mm:orc"],
          @["-d:asyncBackend=asyncdispatch", "-d:asyncBackend=chronos"]):
          # @["-d:asyncBackend=asyncdispatch"]):
        echo "RUNNING TEST: nim " & args.join(" ") & " " & f.path
        exec "nim " & args.join(" ") & " " & f.path
