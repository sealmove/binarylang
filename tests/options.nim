discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(parser, endian = l, bitEndian = r):
  16: little
  b16: big
  4: first
  4: second
  n4: third
  n4: fourth

block:
  var fbs = newFileBitStream("data/options.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.little == 128
  assert data.big == -32768
  assert data.first == 1
  assert data.second == 2
  assert data.third == 3
  assert data.fourth == 4
  
  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.little == reparsed.little
  assert data.big == reparsed.big
  assert data.first == reparsed.first
  assert data.second == reparsed.second
  assert data.third == reparsed.third
  assert data.fourth == reparsed.fourth