discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(parser, bitEndian = r):
  1: b1
  1: b2
  1: b3
  1: b4
  1: b5
  1: b6
  1: b7
  1: b8
  1: b9
  1: b10
  1: b11
  1: b12
  1: b13
  1: b14
  1: b15
  1: b16

block:
  var fbs = newFileBitStream("data/bitendian.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.b1 == 0
  assert data.b2 == 1
  assert data.b3 == 0
  assert data.b4 == 0
  assert data.b5 == 1
  assert data.b6 == 0
  assert data.b7 == 0
  assert data.b8 == 0
  assert data.b9 == 0
  assert data.b10 == 0
  assert data.b11 == 1
  assert data.b12 == 0
  assert data.b13 == 1
  assert data.b14 == 1
  assert data.b15 == 0
  assert data.b16 == 0

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.b1 == reparsed.b1
  assert data.b2 == reparsed.b2
  assert data.b3 == reparsed.b3
  assert data.b4 == reparsed.b4
  assert data.b5 == reparsed.b5
  assert data.b6 == reparsed.b6
  assert data.b7 == reparsed.b7
  assert data.b8 == reparsed.b8
  assert data.b9 == reparsed.b9
  assert data.b10 == reparsed.b10
  assert data.b11 == reparsed.b11
  assert data.b12 == reparsed.b12
  assert data.b13 == reparsed.b13
  assert data.b14 == reparsed.b14
  assert data.b15 == reparsed.b15
  assert data.b16 == reparsed.b16