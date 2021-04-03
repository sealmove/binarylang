discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(aux):
  8: x
struct(parser):
  *aux: x(3)
  8: y

block:
  var fbs = newFileBitStream("data/substreams.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.x.x == 0x12
  assert data.y == 0x78
  
  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.x.x == reparsed.x.x
  assert data.y == reparsed.y