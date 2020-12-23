import ../binarylang, bitstreams
import unittest

suite "Aligned":
  createParser(p):
    16: beword
    l32: ledword
    f32: befloat
    lf64: ledouble
    s: str(3)
    s: term = "DEF"
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "integers":
    check data.beword == 0x1234
    check data.ledword == 0x1234_5678
  test "floats":
    check data.befloat == 0x1234_5678'f32
    check data.ledouble == 0x1234_5678_90AB_CDEF'f64
  test "strings":
    check data.str == "ABC"
    check data.term == "DEF"
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Unaligned":
  createParser(p):
    1: a
    5: b
    10: c
    r12: d
    r20: e
    7: f
    l64: g
    57: h

  var fbs = newFileBitStream("tests/unaligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "values":
    check data.a == 1
    check data.b == 5
    check data.c == 10
    check data.d == 12
    check data.e == 20
    check data.f == 7
    check data.g == 64
    check data.h == 57
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Complex":
  createParser(inner):
    8: x
  createParser(innerWithArgs, a: int8, b: typeGetter(inner)):
    8: x = a
    8: y = b.x
  createParser(outer):
    8: x
    *inner: y
    *innerWithArgs(x, y): z
  var fbs = newFileBitStream("tests/complex.hex")
  defer: close(fbs)
  var data: typeGetter(outer)
  try: data = outer.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      outer.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == outer.get(sbs)

suite "Assertions":
  createParser(inner):
    8: bytes[4]
  createParser(outer):
    s: str = "ABC"
    8: x = 1
    8: y = 2
    8: z = x + y
    *inner: bytes = (@[0'i8, 1, 2, 3],)
  var fbs = newFileBitStream("tests/assertions.hex")
  defer: close(fbs)
  var data: typeGetter(outer)
  try: data = outer.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      outer.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == outer.get(sbs)

suite "Repetition":
  createParser(inner, size: int):
    8: x[size]
  createParser(p):
    *inner(i+1): complex[3]
    8: size
    4: nibbles[size]
    8: bytes{e == 2}
    2: duets{2*i > 7}
    3: trios{s.atEnd}
  var fbs = newFileBitStream("tests/repetition.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "complex":
    check data.complex == @[(@[1'i8],), (@[2'i8, 3],), (@[4'i8, 5, 6],)]
  test "for":
    check data.nibbles == @[0'i8, 1, 2, 3]
  test "until":
    check data.bytes == @[0'i8, 1, 2]
    check data.duets == @[0'i8, 1, 2, 3]
    check data.trios == @[3'i8, 4, 5, 6, 7, 0, 1, 2]
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Substreams":
  createParser(aux):
    8: x
  createParser(p):
    *aux: x(3)
    8: y
  var fbs = newFileBitStream("tests/substreams.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "test":
    check data.x.x == 0x12
    check data.y == 0x78
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Strings":
  createParser(p):
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
  var fbs = newFileBitStream("tests/strings.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "null-terminated":
    check data.a == "AB"
    check data.e == @["IJ", "KL"]
  test "substream":
    data.b = "CD"
  test "magic":
    check data.d == "FG"
    check data.f == @["M", "NO", "PQR"]
    check data.g == @[@["01", "234"], @["5", "678"]]
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Unnamed fields":
  createParser(p):
    16: _ = 0x1234
    l32: _
    f32: _
    lf64: _
    s: _(3)
    s: _ = "DEF"
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Parser options":
  createParser(p, endian = l, bitEndian = r):
    16: little
    b16: big
    4: first
    4: second
    n4: third
    n4: fourth
  var fbs = newFileBitStream("tests/options.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "endians":
    check data.little == 128
    check data.big == -32768
  test "bit-endians":
    check data.first == 1
    check data.second == 2
    check data.third == 3
    check data.fourth == 4
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Custom":
  type Tlv = ref object
    case code: byte
    of 0x12: a: int16
    of 0x34: b: int32
    else: discard
  proc tlvGet(s: BitStream, code: byte): Tlv =
    result = Tlv(code: code)
    case code
    of 0x12: result.a = s.readS16Be
    of 0x34: result.b = s.readS32Be
    else: discard
  proc tlvPut(s: BitStream, input: Tlv, code: byte) =
    case code
    of 0x12: s.writeBe(input.a)
    of 0x34: s.writeBe(input.b)
    else: discard
  let tlv = (get: tlvGet, put: tlvPut)
  createParser(p):
    u8: code1
    u8: code2
    *tlv(code1): variant1
    *tlv(code2): variant2
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "tlv":
    check data.code1 == 0x12
    check data.code2 == 0x34
    check data.variant1.a == 0x7856
    check data.variant2.b == 0x34121234
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    let data2 = p.get(sbs)
    check data.code1 == data2.code1
    check data.code2 == data2.code2
    check data.variant1.a == data2.variant1.a
    check data.variant2.b == data2.variant2.b

suite "Plugins":
  template chainGet(sym, parse, num: untyped) =
    parse
    sym += num
  template chainPut(sym, encode, num: untyped) =
    sym -= num
    encode
  template chain2Get(sym, num: untyped) =
    sym += num
  template chain2Put(sym, num: untyped) =
    sym -= num
  template condGet(sym, parse, cond: untyped) =
    if cond:
      parse
  template condPut(sym, encode, cond: untyped) =
    if cond:
      encode
  createParser(p):
    16: x
    l32 {chain: x, chain2: x}: y
    f32 {cond: x != 0x1234}: no
    f32 {cond: x == 0x1234}: yes
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "conditional":
    check data.no == 0
    check data.yes == 0x1234_5678'f32
  test "chain":
    check data.x == 0x1234
    check data.y == 0x1234_7AE0
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)