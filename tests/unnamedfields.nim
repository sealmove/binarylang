discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

createParser(parser):
  16: _ = 0x1234
  l32: _
  f32: _
  lf64: _
  s: _(3)
  s: _ = "DEF"

block:
  var fbs = newFileBitStream("tests/data/aligned.hex")
  defer: close(fbs)
  discard parser.get(fbs)