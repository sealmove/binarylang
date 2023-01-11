import astutil
import ../types
from ../dsldecoders import getImpl
import macros

proc getCustomWriter(typ: Type; sym, bs: NimNode; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"put"), bs)
  case typ.kind
  of kProduct:
    for arg in typ.args:
      result.add(arg.copyNimTree)
    result.prefixFields(st, params, ident"input")
  of kSum:
    result.add(quote do: `sym`.disc)
    for i in 1 ..< typ.args.len:
      result.add(typ.args[i].copyNimTree)
    result.prefixFields(st, params, ident"input")
  else: discard

proc createWriteStatement(f: Field, sym, bs: NimNode; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newStmtList()
  let
    kind = f.typ.kind
    impl = f.typ.getImpl
    endian = if f.typ.endian == littleEndian: 'l' else: 'b'
    endianNode = newLit(f.typ.endian)
    bitEndian = if f.typ.bitEndian == littleEndian: 'l' else: 'b'
  case kind
  of kInt, kUInt, kFloat:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procUnaligned = ident("writebits" & bitEndian & "e")
    if sym == nil:
      result.add(quote do:
        `procUnaligned`(`bs`, `sizeNode`, 0))
    else:
      if size in {8, 16, 32, 64}:
        let procAligned = ident("write" & endian & "e")
        result.add(quote do:
          if isAligned(`bs`):
            `procAligned`(`bs`, `impl`(`sym`))
          else:
            `procUnaligned`(`bs`, `sizeNode`, `sym`, `endianNode`))
      else:
        result.add(quote do: `procUnaligned`(`bs`, `sizeNode`, `sym`, `endianNode`))
  of kStr:
    result.add(quote do:
      if not isAligned(`bs`):
        raise newException(IOError, "Stream must be aligned to write a string"))
    if sym != nil:
      result.add(quote do:
        writeStr(`bs`, `sym`))
    if f.val.valueExpr == nil and (f.magic == nil or f.val.isMagic):
      result.add(quote do:
        writeBe(`bs`, 0'u8))
  of kProduct, kSum:
    let call = getCustomWriter(f.typ, sym, bs, st, params)
    call.insert(2, sym)
    result.add(quote do: `call`)

proc createWriteField(sym: NimNode; f: Field; bs: NimNode;
                      st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let
    ident = f.symbol
    input = ident"input"
    elem = if f.val.isMagic: genSym(nskForVar)
           else: sym
  var value = f.val.valueExpr
  value.prefixFields(st, params, input)
  var writeStmts = newStmtList()
  case f.val.repeat
  of rNo:
    writeStmts.add createWriteStatement(f, elem, bs, st, params)
  of rFor:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, input)
    let
      loopIdx = ident"i"
      loopElem = genSym(nskForVar)
      writeStmt = createWriteStatement(f, loopElem, bs, st, params)
    writeStmts.add(quote do:
      for `loopIdx`, `loopElem` in `elem`:
        `writeStmt`)
  of rUntil:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, input)
    let
      forSym = genSym(nskForVar)
      loopIdx = ident"i"
    expr.replaceWith(ident"_", elem)
    expr.replaceWith(ident"s", bs)
    let writeStmt = createWriteStatement(f, forSym, bs, st, params)
    writeStmts.add(quote do:
      for `loopIdx`, `forSym` in `elem`:
        `writeStmt`)
  if f.val.isMagic:
    result.add(quote do:
      for `elem` in `input`.`ident`:
        `writeStmts`)
  else:
    result.add(writeStmts)

proc generateWrite(sym: NimNode; f: Field; bs: NimNode;
                   st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let input = ident"input"
  if f.val.sizeExpr != nil:
    var size = f.val.sizeExpr.copyNimTree
    size.prefixFields(st, params, input)
    let
      ss = genSym(nskVar)
      wf = createWriteField(sym, f, ss, st, params)
    result.add(quote do:
      var `ss` = newPaddedBitStream(int(`size`))
      `wf`
      `ss`.seek(0)
      `bs`.writeFromSubstream(`ss`, int(`size`)))
  else:
    result.add createWriteField(sym, f, bs, st, params)

proc generateWriter*(fields: seq[Field]; fst, pst: seq[string]):
 NimNode {.compileTime.} =
  result = newStmtList()
  let
    bs = ident"s"
    input = ident"input"
  for f in fields:
    var wSym = genSym(nskVar)
    let
      ident = f.symbol.copyNimTree
      field = ident.strVal
    var impl = f.typ.getImpl
    if f.val.repeat != rNo:
      impl = quote do: seq[`impl`]
    if f.val.isMagic:
      impl = quote do: seq[`impl`]
    let value =
      if field == "":
        if f.val.valueExpr == nil: nil
        else: f.val.valueExpr.copyNimTree
      else: quote do: `input`.`ident`
    result.add(
      if value == nil:
        quote do:
          var `wSym`: `impl`
      else:
        quote do:
          var `wSym` = `value`)
    if f.ops.len > 0:
      # Infer potentially missing types for operations
      for i in 0 ..< f.ops.len:
        if f.ops[i].typ.kind == nnkNone:
          if i == 0:
            f.ops[i].typ = impl.copyNimTree
          else:
            f.ops[i].typ = f.ops[i-1].typ.copyNimTree
      var
        encoded = genSym(nskVar)
        outputSym = genSym(nskVar)
      # Loop through all operations backwards except the first one
      for i in countdown(f.ops.len - 1, 1):
        var
          typeImpl = f.ops[i-1].typ
          write = quote do: `outputSym` = `encoded`
          op = newCall(ident(f.ops[i].name & "put"), write, wSym, encoded)
        result.add(quote do:
          var `encoded`, `outputSym`: `typeImpl`)
        for arg in f.ops[i].args:
          var argVal = arg.copyNimTree
          argVal.prefixFields(fst, pst, input)
          argVal.replaceWith(ident"_", wSym)
          op.add(argVal)
        result.add(op)
        wSym = encoded
        encoded = genSym(nskVar)
        outputSym = genSym(nskVar)
      result.add(quote do:
        var `encoded`,`outputSym`: `impl`)
      var op =
        newCall(
          ident(f.ops[0].name & "put"),
          generateWrite(encoded, f, bs, fst, pst),
          wSym,
          encoded)
      for arg in f.ops[0].args:
        var argVal = arg.copyNimTree
        argVal.prefixFields(fst, pst, input)
        argVal.replaceWith(ident"_", wSym)
        op.add(argVal)
      result.add(op)
    else:
      result.add(
        generateWrite(wSym, f, bs, fst, pst))