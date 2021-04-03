discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(parser):
  16: beword
  l32: ledword
  f32: befloat
  lf64: ledouble
  s: str(3)
  s: term = "DEF"

block:
  var fbs = newFileBitStream("data/aligned.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.beword == 0x1234
  assert data.ledword == 0x1234_5678
  assert data.befloat == 0x1234_5678'f32
  assert data.ledouble == 0x1234_5678_90AB_CDEF'f64
  assert data.str == "ABC"
  assert data.term == "DEF"

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.beword == reparsed.beword
  assert data.ledword == reparsed.ledword
  assert data.befloat == reparsed.befloat
  assert data.ledouble == reparsed.ledouble
  assert data.str == reparsed.str
  assert data.term == reparsed.term