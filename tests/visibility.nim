discard """
  cmd: "nim c -r $file"
"""

import ../binarylang
import tlv

block:
  # struct
  var fbs = newFileBitStream("data/aligned.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  discard data.code1 == 0x12

  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)

  # union
  let data2 = someTlv.get(sbs, 0x12'u8)
  discard data2.a == 0x12