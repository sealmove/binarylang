import types, errors
import macros, strutils, strformat

proc getImpl*(typ: Type): NimNode {.compileTime.} =
  case typ.kind
  of kInt, kUInt:
    var s = ""
    if typ.kind == kUInt:
      s &= "u"
    s &= "int"
    if typ.size == 0: discard
    elif typ.size > 32: s &= "64"
    elif typ.size > 16: s &= "32"
    elif typ.size > 8: s &= "16"
    else: s &= "8"
    result = ident(s)
  of kFloat:
    result = ident("float" & $typ.size)
  of kStr:
    result = ident"string"
  of kProduct, kSum:
    let sym = ident(typ.symbol.strVal.capitalizeAscii)
    result = quote do: `sym`

proc decodeType*(t: NimNode, opts: Options, prefix: string): Type
 {.compileTime.} =
  var t = t
  result = Type()
  var
    endian = opts.endian
    bitEndian = opts.bitEndian
  case t.kind
  of nnkIntLit:
    result.kind = kInt
    result.size = t.intVal
    if result.size > 64:
      raise newException(Defect, "Unable to parse values larger than 64 bits")
  of nnkIdent:
    var
      kind = kInt
      letters: set[char]
      size: int
    for i, c in t.strVal:
      case c
      of 'u', 'f', 's':
        if letters * {'u', 'f', 's'} != {}:
          raise newException(Defect, "Type was specified more than once")
        if c == 'u':
          kind = kUInt
        elif c == 'f':
          kind = kFloat
        elif c == 's':
          kind = kStr
      of 'l', 'b':
        if letters * {'l', 'b'} != {}:
          raise newException(Defect, "Endianness was specified more than once")
        if c == 'b':
          endian = bigEndian
        else:
          endian = littleEndian
      of 'n', 'r':
        if letters * {'n', 'r'} != {}:
          raise newException(Defect,
            "Bit endianness was specified more than once")
        if c == 'n':
          bitEndian = bigEndian
        else:
          bitEndian = littleEndian
      else:
        try: size = t.strVal[i..^1].parseInt
        except ValueError:
          raise newException(Defect, &"Format {t.strVal} not supported")
        break
      letters.incl c
    result.kind = kind
    result.size = size
    if letters * {'l', 'b'} != {} and 's' in letters:
      raise newException(Defect, "Endianness for strings is not supported")
    if size > 64:
      raise newException(Defect, "Unable to parse values larger than 64 bits")
    if kind in {kInt, kUInt, kFloat} and size == 0:
      raise newException(Defect, "Unable to parse values with size 0")
    if kind == kFloat and size != 32 and size != 64:
      raise newException(Defect, "Only 32 and 64 bit floats are supported")
    if kind == kStr and size mod 8 != 0:
      raise newException(Defect, "Unaligned strings are not supported")
    if letters * {'l', 'b'} != {} and (size == 8 or size mod 8 != 0):
      raise newException(Defect, "l/b is only valid for multiple-of-8 sizes")
  of nnkCall:
    case prefix
    of "*": result.kind = kProduct
    of "+": result.kind = kSum
    else: syntaxError("Invalid prefix symbol for type. Valid are: '*', '+'")
    if t[0].kind == nnkCall:
      t = t[0]
    result.symbol = t[0]
    var i = 1
    while i < t.len:
      result.args.add(t[i].copyNimTree)
      inc i
  else:
    syntaxError("Invalid type")
  result.endian = endian
  result.bitEndian = bitEndian

proc decodeOps*(node: NimNode): Operations {.compileTime.} =
  for child in node:
    var
      name: string
      typ = newTree(nnkNone)
      args: seq[NimNode]
    case child.kind
    of nnkIdent:
      name = child.strVal
    of nnkBracketExpr:
      name = child[0].strVal
      typ = child[1].copyNimTree
    of nnkCall:
      case child[0].kind
      of nnkIdent:
        name = child[0].strVal
      of nnkBracketExpr:
        name = child[0][0].strVal
        typ = child[0][1].copyNimTree
      else:
        syntaxError("Invalid syntax for operation")
      for i in 1 ..< child.len:
        args.add(child[i].copyNimTree)
    else:
      syntaxError("Invalid syntax for operation")
    result.add (name, typ, args)

proc decodeValue*(node: NimNode, st: var seq[string]): Value {.compileTime.} =
  var node = node
  result = Value()
  while node.kind != nnkIdent:
    if node.kind == nnkAsgn:
      result.valueExpr = node[1]
      node = node[0]
    elif node.kind == nnkCurly:
      if result.valueExpr != nil:
        raise newException(Defect,
          "Magic and assertion can't be used together in the same field")
      result.isMagic = true
      node = node[0]
    elif node.kind == nnkBracketExpr:
      result.repeat = rFor
      result.repeatExpr = node[1]
      node = node[0]
    elif node.kind == nnkCurlyExpr:
      result.repeat = rUntil
      result.repeatExpr = node[1]
      node = node[0]
    elif node.kind == nnkCall:
      result.sizeExpr = node[1]
      node = node[0]
    elif node.kind == nnkPrefix:
      result.isExported = true
      node = node[1]
  if node.strVal != "_":
    result.name = node.strVal
    st.add(result.name)

const defaultOptions: Options = (
  endian: bigEndian,
  bitEndian: bigEndian,
  reference: false)

proc decodeHeader*(input: seq[NimNode]):
 tuple[params: seq[NimNode], opts: Options] {.compileTime.} =
  result.opts = defaultOptions
  var specifiedOpts: set[OptionSet]
  for n in input:
    case n.kind
    of nnkExprColonExpr:
      result.params.add(newIdentDefs(n[0], n[1]))
    of nnkExprEqExpr:
      case n[0].strVal
      of "endian":
        if osEndian in specifiedOpts:
          raise newException(Defect,
            "Option 'endian' was specified more than once")
        case n[1].strVal
        of "b": result.opts.endian = bigEndian
        of "l": result.opts.endian = littleEndian
        of "c": result.opts.endian = cpuEndian
        else:
          raise newException(Defect,
            "Invalid value for endian option (valid values: l, b)")
        specifiedOpts.incl osEndian
      of "bitEndian":
        if osBitEndian in specifiedOpts:
          raise newException(Defect,
            "Option 'bitEndian' was specified more than once")
        case n[1].strVal
        of "n": result.opts.bitEndian = bigEndian
        of "r": result.opts.bitEndian = littleEndian
        else:
          raise newException(Defect,
            "Invalid value for 'bitEndian' option (valid values: n, r)")
        specifiedOpts.incl osBitEndian
      of "reference":
        if osReference in specifiedOpts:
          raise newException(Defect,
            "Option 'reference' was specified more than once")
        case n[1].strVal
        of "y": result.opts.reference = true
        of "n": result.opts.reference = false
        else:
          raise newException(Defect,
            "Invalid value for 'reference' option (valid values: y, n)")
      else:
        raise newException(Defect, &"Unknown option: {$n[0]}")
    else:
      syntaxError("Invalid header syntax")

proc decodeField*(def: NimNode, st: var seq[string], opts: Options):
 Field {.compileTime.} =
  var
    a, b, c: NimNode
    prefix: string
  case def.kind
  of nnkPrefix:
    prefix = def[0].strVal
    c = def[2][0].copyNimTree
    case def[1].kind
    of nnkIdent:
      a = newCall(def[1].copyNimTree)
    of nnkCall:
      a = def[1].copyNimTree
    of nnkCommand:
      case def[1][0].kind
      of nnkIdent:
        a = newCall(def[1][0].copyNimTree)
      of nnkCall:
        a = def[1][0].copyNimTree
      else: syntaxError("Invalid field syntax")
      b = def[1][1].copyNimTree
    else: syntaxError("Invalid field syntax")
  of nnkCall:
    a = def[0].copyNimTree
    c = def[1][0].copyNimTree
  of nnkCommand:
    a = def[0].copyNimTree
    b = def[1].copyNimTree
    c = def[2][0].copyNimTree
  else: syntaxError("Invalid field syntax")
  result = Field(
    typ: decodeType(a, opts, prefix),
    ops: decodeOps(b),
    val: decodeValue(c, st))
  result.symbol =
    ident(result.val.name)

proc decodeVariation*(def: NimNode, st: seq[string], opts: Options):
 Variation {.compileTime.} =
  def.expectKind(nnkCall)
  var
    isElseBranch, isEmpty: bool
    cases: seq[NimNode]
    fields: seq[Field]
  if def[0].kind == nnkIdent:
    if not eqIdent(def[0], "_"):
      syntaxError("Missing parenthesis around branch expression")
    isElseBranch = true
  if def[1].len == 1 and def[1][0].kind == nnkNilLit:
    isEmpty = true
  result = Variation(isEmpty: isEmpty, isElseBranch: isElseBranch)
  if not isElseBranch:
    def[0].expectKind({nnkPar, nnkTupleConstr})
    for c in def[0]:
      cases.add(c.copyNimTree)
    result.cases = cases
  if not isEmpty:
    var symbolTable = st
    for f in def[1]:
      fields.add(decodeField(f, symbolTable, opts))
    result.fields = fields
    result.st = symbolTable