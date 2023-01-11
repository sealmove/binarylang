type
  ParserPlugin* = enum
    ppConverters = "converters"
  ParserOptions* = tuple
    endian: Endianness
    bitEndian: Endianness
    reference: bool
    plugins: set[ParserPlugin]
  ParserOption* = enum
    poEndian
    poBitEndian
    poReference
    poPlugins
  Kind* = enum
    kInt, kUInt, kFloat, kStr, kProduct, kSum
  Type* = object
    case kind*: Kind
    of kProduct, kSum:
      symbol*: NimNode
      args*: seq[NimNode]
    else:
      size*: BiggestInt
    endian*: Endianness
    bitEndian*: Endianness
  Operation* = tuple
    name: string
    typ: NimNode
    args: seq[NimNode]
  Operations* = seq[Operation]
  Repeat* = enum
    rNo
    rFor
    rUntil
  Value* = object
    name*: string
    case repeat*: Repeat
    of rFor, rUntil: repeatExpr*: NimNode
    of rNo: discard
    valueExpr*: NimNode
    sizeExpr*: NimNode
    isMagic*: bool
    isExported*: bool
  Field* = ref object
    typ*: Type
    ops*: Operations
    val*: Value
    symbol*: NimNode
    magic*: Field
  Variation* = object
    case isElseBranch*: bool:
    of false:
      cases*: seq[NimNode]
    of true:
      discard
    case isEmpty*: bool:
    of false:
      fields*: seq[Field]
      st*: seq[string]
    of true:
      discard