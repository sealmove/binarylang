import astutil
import ../types
from ../dsldecoders import getImpl
import macros

proc getCustomReader(typ: Type; bs: NimNode; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"get"), bs)
  for arg in typ.args:
    result.add(arg.copyNimTree)
  result.prefixFields(st, params, ident"result")

proc createReadStatement(sym, bs: NimNode; f: Field; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newStmtList()
  let
    kind = f.typ.kind
    impl = f.typ.getImpl
    endian = if f.typ.endian == littleEndian: 'l' else: 'b'
    endianNode = newLit(f.typ.endian)
    bitEndian = if f.typ.bitEndian == littleEndian: 'l' else: 'b'
    procUnaligned = ident("readbits" & bitEndian & "e")
  case kind
  of kInt, kUInt:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procUnalignedCall = quote do: `procUnaligned`(`bs`, `sizeNode`, `endianNode`)
    if size in {8, 16, 32, 64}:
      let numKind = if kind == kInt: 's' else: 'u'
      var procAlignedStr = "read" & numKind & $size
      if size != 8: procAlignedStr &= endian & "e"
      let procAligned = ident(procAlignedStr)
      result.add(quote do:
        if isAligned(`bs`):
          `sym` = `procAligned`(`bs`)
        else:
          `sym` = `impl`(`procUnalignedCall`))
    else:
      result.add(quote do:
        if isAligned(`bs`):
          resetBuffer(`bs`)
        `sym` = `impl`(`procUnalignedCall`))
  of kFloat:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procAligned = ident("readf" & $size & endian & "e")
      procUnalignedCall = quote do: `procUnaligned`(`bs`, `sizeNode`, `endianNode`)
      floatCast =
        if size == 64: quote do: cast[float64](`procUnalignedCall`)
        else: quote do: float32(cast[float64](`procUnalignedCall`))
    result.add(quote do:
      if isAligned(`bs`):
        `sym` = `procAligned`(`bs`)
      else:
        `sym` = `floatCast`)
  of kStr:
    let expr = f.val.valueExpr
    result.add(quote do:
      if not isAligned(`bs`):
        raise newException(IOError, "Stream must be aligned to read a string"))
    result.add(
      if expr != nil:
        quote do:
          `sym` = readStr(`bs`, 8 * `expr`.len)
      else:
        quote do:
          `sym` = readStr(`bs`))
  of kProduct, kSum:
    let call = getCustomReader(f.typ, bs, st, params)
    result.add(quote do: `sym` = `call`)

proc createReadField(sym: NimNode; f: Field; bs: NimNode;
                     st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let
    res = ident"result"
    impl = f.typ.getImpl
  var value = f.val.valueExpr
  value.prefixFields(st, params, res)
  if f.val.repeat == rNo:
    result.add createReadStatement(sym, bs, f, st, params)
  else:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, res)
    case f.val.repeat
    of rFor:
      let
        loopIdx = ident"i"
        tmp = quote do: `sym`[`loopIdx`]
      let readStmt = createReadStatement(tmp, bs, f, st, params)
      result.add(quote do:
        `sym` = newSeq[`impl`](`expr`)
        for `loopIdx` in 0 ..< int(`expr`):
          `readStmt`)
    of rUntil:
      let
        tmp = genSym(nskVar)
        loopIdx = ident"i"
      expr.replaceWith(ident"_", tmp)
      expr.replaceWith(ident"s", bs)
      let readStmt = createReadStatement(tmp, bs, f, st, params)
      result.add (quote do:
        block:
          `sym` = newSeq[`impl`]()
          var
            `loopIdx`: int
            `tmp`: `impl`
          while true:
            `readStmt`
            `sym`.add(`tmp`)
            inc `loopIdx`
            if `expr`:
              break)
    else: discard

proc generateRead*(sym: NimNode; f: Field; bs: NimNode;
                  st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let
    res = ident"result"
    field = f.val.name
    isSingleStr = f.typ.kind == kStr and f.val.repeat == rNo and not f.val.isMagic
  var impl = f.typ.getImpl
  if f.val.repeat != rNo:
    impl = quote do: seq[`impl`]
  if f.val.isMagic:
    impl = quote do: seq[`impl`]
  var value = f.val.valueExpr.copyNimTree
  value.prefixFields(st, params, res)
  if f.val.sizeExpr != nil:
    var size = f.val.sizeExpr.copyNimTree
    size.prefixFields(st, params, res)
    let
      ss = genSym(nskVar)
      rf = createReadField(sym, f, ss, st, params)
    result.add(quote do:
      var
        `ss` = createSubstream(`bs`, int(`size`))
      `rf`)
    if value != nil:
      result.add(quote do:
        if `sym` != (`value`):
          raise newException(MagicError, "field '" & $`field` & "' was " &
                            $`sym` & " instead of " & $`value`))
  elif isSingleStr and f.magic != nil:
    let
      tmp = genSym(nskVar)
      pos = genSym(nskLet)
      rf = createReadField(tmp, f.magic, bs, st, params)
    var
      tmpImpl = f.magic.typ.getImpl
      magicVal = f.magic.val.valueExpr
    magicVal.prefixFields(st, params, res)
    if f.magic.val.repeat != rNo:
      tmpImpl = quote do: seq[`tmpImpl`]
    result.add(quote do:
      var
        `tmp`: `tmpImpl`
      while true:
        let `pos` = getPosition(`bs`)
        `rf`
        `bs`.seek(`pos`)
        if `tmp` == `magicVal`:
          break
        `sym`.add(readU8(`bs`).char))
  elif f.val.isMagic:
    let
      elem = genSym(nskVar)
      magic = genSym(nskVar)
      pos = genSym(nskLet)
      readElem = createReadField(elem, f, bs, st, params)
      readMagic = createReadField(magic, f.magic, bs, st, params)
    var elemImpl = f.typ.getImpl
    if f.val.repeat != rNo:
      elemImpl = quote do: seq[`elemImpl`]
    var magicImpl = f.magic.typ.getImpl
    if f.magic.val.repeat != rNo:
      magicImpl = quote do: seq[`magicImpl`]
    var magicVal = f.magic.val.valueExpr
    magicVal.prefixFields(st, params, res)
    result.add(quote do:
      var
        `elem`: `elemImpl`
        `magic`: `magicImpl`
      while true:
        let `pos` = getPosition(`bs`)
        `readMagic`
        `bs`.seek(`pos`)
        if `magic` == `magicVal`:
          break
        `readElem`
        `sym`.add(`elem`))
  else:
    let
      rf = createReadField(sym, f, bs, st, params)
    result.add(quote do:
      `rf`)
    if value != nil:
      result.add(quote do:
        if `sym` != (`value`):
          raise newException(MagicError, "field '" & $`field` & "' was " &
                            $`sym` & " instead of " & $`value`))

proc generateReader*(fields: seq[Field]; fst, pst: seq[string]):
 NimNode {.compileTime.} =
  let
    bs = ident"s"
    res = ident"result"
  result = newStmtList()
  for f in fields:
    var rSym = genSym(nskVar)
    let
      ident = f.symbol.copyNimTree
      field = ident.strVal
    var impl = f.typ.getImpl
    if f.val.repeat != rNo:
      impl = quote do: seq[`impl`]
    if f.val.isMagic:
      impl = quote do: seq[`impl`]
    result.add(quote do:
      var `rSym`: `impl`)
    var
      read = generateRead(rSym, f, bs, fst, pst)
      outputSym, parsed: NimNode
    if f.ops.len > 0:
      # Infer potentially missing types for operations
      for i in 0 ..< f.ops.len:
        if f.ops[i].typ.kind == nnkNone:
          if i == 0:
            f.ops[i].typ = impl.copyNimTree
          else:
            f.ops[i].typ = f.ops[i-1].typ.copyNimTree
      outputSym = genSym(nskVar)
      parsed = genSym(nskVar)
      for i in 0 ..< f.ops.len:
        var typeImpl = f.ops[i].typ
        result.add(quote do:
          var `outputSym`, `parsed`: `typeImpl`)
        var op = newCall(ident(f.ops[i].name & "get"), read, rSym, outputSym)
        for arg in f.ops[i].args:
          var argVal = arg.copyNimTree
          argVal.prefixFields(fst, pst, res)
          argVal.replaceWith(ident"_", rSym)
          op.add(argVal)
        result.add(op)
        rSym = outputSym
        outputSym = genSym(nskVar)
        parsed = genSym(nskVar)
        read = quote do: `parsed` = `outputSym`
    else:
      result.add(read)
    if field != "":
      result.add(quote do:
        result.`ident` = `rSym`)