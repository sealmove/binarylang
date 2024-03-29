discard """
  cmd: "nim c -r $file"
"""

import ../binarylang
from ../binarylang/operations import condGet, condPut, validGet, validPut
from strutils import intToStr, parseInt

template addGet(parse, parsed, output, n: untyped) =
  parse
  output = parsed + n
template addPut(encode, encoded, output, n: untyped) =
  output = encoded - n
  encode

struct(parser):
  16: x
  l32 {add(x), valid((_ * (x - 0x1232)) == 0x2468_D158'i32)}: y
  f32 {cond(x != 0x1234)}: no
  f32 {cond(x == 0x1234)}: yes

template toIntGet(parse, parsed, output) =
  parse
  output = parsed.int
template toIntPut(encode, encoded, output) =
  output = encoded.int16
  encode

template toStrGet(parse, parsed, output) =
  parse
  output = parsed.intToStr
template toStrPut(encode, encoded, output) =
  output = encoded.parseInt
  encode

struct(typing):
  16 {toInt[int], add(5), toStr[string]}: x

block:
  var fbs = newFileBitStream("data/aligned.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.x == 0x1234
  assert data.y == 0x1234_68AC
  assert data.no == 0
  assert data.yes == 0x1234_5678'f32

  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.x == reparsed.x
  assert data.y == reparsed.y
  assert data.no == reparsed.no
  assert data.yes == reparsed.yes

block:
  var fbs = newFileBitStream("data/aligned.hex")
  defer: close(fbs)
  let data = typing.get(fbs)
  assert data.x == "4665"
