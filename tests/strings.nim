discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(parser):
  s: a
  s: b(2)
  s: c = "E"
  s: d
  s: _ = "H"
  s: e[2]
  s: {f}
  u8: term = 0xFF
  s: {g[2]}
  s: _ = "END"

block:
  var fbs = newFileBitStream("data/strings.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.a == "AB"
  assert data.b == "CD"
  assert data.c == "E"
  assert data.d == "FG"
  assert data.e == @["IJ", "KL"]
  assert data.f == @["M", "NO", "PQR"]
  assert data.g == @[@["01", "234"], @["5", "678"]]
  
  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.a == reparsed.a
  assert data.b == reparsed.b
  assert data.c == reparsed.c
  assert data.d == reparsed.d
  assert data.e == reparsed.e
  assert data.f == reparsed.f
  assert data.term == reparsed.term
  assert data.g == reparsed.g