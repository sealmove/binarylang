discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(inner):
  8: x
struct(innerWithArgs, a: int8, b: Inner):
  8: x = a
  8: y = b.x
struct(parser):
  8: x
  *inner: y
  *innerWithArgs(x, y): z

block:
  var fbs = newFileBitStream("data/complex.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.x == 0x55
  assert data.y.x == 0xAA'i8
  assert data.z.x == 0x55
  assert data.z.y == 0xAA'i8

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.x == reparsed.x
  assert data.y.x == reparsed.y.x
  assert data.z.x == reparsed.z.x
  assert data.z.y == reparsed.z.y