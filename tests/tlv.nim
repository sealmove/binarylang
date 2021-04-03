discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

union(tlv, *byte):
  (0x12): u16: a
  (0x34, 0x56):
    u32: *b
    u16: c
  _: nil
struct(parser):
  u8: code1
  u8: code2
  +tlv(code1): variant1
  +tlv(code2): variant2

block:
  var fbs = newFileBitStream("data/aligned.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.code1 == 0x12
  assert data.code2 == 0x34
  assert data.variant1.a == 0x7856
  assert data.variant2.b == 0x34121234
  assert data.variant2.c == 0x5678

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.code1 == reparsed.code1
  assert data.code2 == reparsed.code2
  assert data.variant1.a == reparsed.variant1.a
  assert data.variant2.b == reparsed.variant2.b