discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(unaligned):
  1: a
  5: b
  10: c
  r12: d
  r20: e
  7: f
  l64: g
  57: h

block:
  var fbs = newFileBitStream("data/unaligned.hex")
  defer: close(fbs)
  let data = unaligned.get(fbs)
  assert data.a == 1
  assert data.b == 5
  assert data.c == 10
  assert data.d == 12
  assert data.e == 20
  assert data.f == 7
  assert data.g == 64
  assert data.h == 57

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  unaligned.put(sbs, data)
  sbs.seek(0)
  let reparsed = unaligned.get(sbs)
  assert data.a == reparsed.a
  assert data.b == reparsed.b
  assert data.c == reparsed.c
  assert data.d == reparsed.d
  assert data.e == reparsed.e
  assert data.f == reparsed.f
  assert data.g == reparsed.g
  assert data.h == reparsed.h