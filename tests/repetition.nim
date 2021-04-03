discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(inner, size: int):
  8: x[size]
struct(parser):
  *inner(i+1): complex[3]
  8: size
  4: nibbles[size]
  8: bytes{_ == 2}
  2: duets{2*i > 7}
  3: trios{s.atEnd}

block:
  var fbs = newFileBitStream("data/repetition.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.complex[0].x == @[1'i8]
  assert data.complex[1].x == @[2'i8, 3]
  assert data.complex[2].x == @[4'i8, 5, 6]
  assert data.nibbles == @[0'i8, 1, 2, 3]
  assert data.bytes == @[0'i8, 1, 2]
  assert data.duets == @[0'i8, 1, 2, 3]
  assert data.trios == @[3'i8, 4, 5, 6, 7, 0, 1, 2]
  
  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.complex[0].x == reparsed.complex[0].x
  assert data.complex[1].x == reparsed.complex[1].x
  assert data.complex[2].x == reparsed.complex[2].x
  assert data.size == reparsed.size
  assert data.nibbles == reparsed.nibbles
  assert data.bytes == reparsed.bytes
  assert data.duets == reparsed.duets
  assert data.trios == reparsed.trios