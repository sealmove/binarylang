discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(inner):
  8: bytes[4]
struct(parser):
  s: str = "ABC"
  8: x = 1
  8: y = 2
  8: z = x + y
  *inner: inr

block:
  var fbs = newFileBitStream("data/assertions.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.str == "ABC"
  assert data.x == 1
  assert data.y == 2
  assert data.z == 3
  assert data.inr.bytes == @[0'i8, 1, 2, 3]

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.str == reparsed.str
  assert data.x == reparsed.x
  assert data.y == reparsed.y
  assert data.z == reparsed.z
  assert data.inr.bytes == reparsed.inr.bytes