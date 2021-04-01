## BinaryLang is a DSL for creating binary parsers/encoders.
##
## It exports two macros:
## - `createParser` which is used to produce a *product parser*
## - `createVariantParser` which is used to produce a *sum parser*
##
## Both of these macros generate a type declaration and a
## `tuple[get: proc, put: proc]`:
## - `get` returns an object with each parsed field
## - `put` writes an object to a stream
##
## Parser options
## ----------------------------------------------------------------------------
## Each specified option must be in the form `option = value`:
## - `endian`: sets the default byte endianness for the whole parser
##    - *default*: big endian
##    - `b`: **big** endian
##    - `l`: **little** endian
## - `bitEndian`: sets the default bit endianness for the whole parser
##    - *default*: left -> right
##    - `n`: left -> right (**normal**)
##    - `r`: left <- right (**reverse**)
##
## Parser parameters
## ----------------------------------------------------------------------------
## Each parameter must be in the form `symbol: type`. The generated `get`/`put`
## procs will then have this additional parameter appended.
##
## DSL
## ----------------------------------------------------------------------------
## Each statement corresponds to 1 field. The general syntax is:
##
## .. code::
##    type {plugin: expr, ...}: name (...)
##
## For the name you use `_` to discard the field, or prepend it with `*` to
## export it.
##
## Type
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## The **kind**, **endianness** and **size** are encoded in a identifier
## made up of:
##
## - 1 optional letter specifying the kind:
##    - *default*: signed integer
##    - `u`: **unsigned** integer
##    - `f`: **float**
##    - `s`: **string**
##    - `*`: complex (see below)
## - 1 optional letter specifying byte endianness:
##    - *default*: big endian
##    - `b`: **big** endian
##    - `l`: **little** endian
## - 1 optional letter specifying bit endianness:
##    - *default*: left -> right
##    - `n`: left -> right (**normal**)
##    - `r`: left <- right (**reverse**)
## - 1 number specifying size in **bits**:
##    - for a string it refers to the size of each individual character and
##      defaults to `8`
##    - for an integer the allowed values are `1 .. 64`
##    - for a float the allowed values are `32` and `64`
##    - for a custom it can't be used (but you can use a substream, see below)
##
## You can order options however you want, but size must come last (e.g.
## `lru16` and `url16` are valid but not `16lru`).
##
## Alignment
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## If any of the following is violated, BinaryLang should generate an
## exception:
## - Byte endianness can only be used with byte-multiple integers
## - Bit endianness must be uniform between **byte boundaries**
## - Spec must finish on a byte boundary
##
## .. code:: nim
##    createParser(parser, bitEndian = n):
##      b9: a # error: cannot apply byte endianness
##      r6: b # error: shares bits with previous byte
##      10: c # error: spec does not finish on a byte boundary
##
## Moreover, unaligned reads for strings are not supported:
##
## .. code:: nim
##    createParser(parser):
##      6: x
##      s: y # invalid, generates an exception
##
## Assertion
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use `= expr` for producing an exception if the parsed value doesn't match
## `expr`:
##
## .. code:: nim
##    s: x = "BinaryLang is awesome"
##    8: y[5] = @[0, 1, 2, 3, 4]
##
## Assertion can also be used in a special manner to terminate the previous
## field if it's a **string** or a **sequence indicated as magic-terminated**.
## This is discussed in later sections.
##
## Complex types
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Instead of the described identifier for specifying type, you can call a
## previously defined parser by using `*` followed by the name of the parser.
## If your parser is parametric you must pass arguments to it with standard
## call syntax.
##
## Example:
##
## .. code:: nim
##    createParser(inner):
##      32: a
##      32: b
##
##    createParser(innerWithArgs, size: int32):
##      32: a
##      32: b[size]
##
##    createParser(outer):
##      *inner: x
##      *innerWithArgs(x.a): y
##
## Repetition
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## There are 3 ways to produce a `seq` of your type:
##
## - `for`: append `[expr]` to the name for repeating `expr` times
## - `until`: append `{expr}` to the name for repeating until `expr` is
##   evaluated to `true`
## - `magic`: enclose name with `{}` and use assertion with your **next** field
##
## .. code:: nim
##    8: a[5] # reads 5 8-bit integers
##    8: b{_ == 103 or i > 9} # reads until it finds the value 103 or
##                            # completes 10th iteration
##    8: {c} # reads 8-bit integers until next field is matches
##    16: _ = 0xABCD
##    u8: {d[5]} # reads byte sequences each of length 5 until next field
##               # matches
##    s: _ = "END"
##
## Also, the following symbols are defined implicitly:
## - `i`: current iteration index
## - `_`: last element read
##
## These can be leveraged even in other expressions than the expression for
## repetition itself; for instance you can use them to parameterize a parser:
##
## .. code:: nim
##    createParser(inner, size: int):
##      8: x[size]
##    createParser(outer):
##      32: amount
##      32: sizes[amount]
##      *inner(sizes[i]): complex[amount]
##
## With the above trick you can get a sequence of variable-length sequences.
##
## Due to current limitations of the underlying bitstream implementation, to
## perform magic, your stream must be aligned and all the reads involved must
## also be aligned. This will be fixed in the future.
##
## Substreams
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Call syntax forces the creation of a substream:
##
## .. code:: nim
##    createParser(aux, size: int):
##      8: x[size]
##    createParser(parser):
##      8: x = 4
##      8: limit = 8
##      *aux(x): fixed(limit)
##
## In the above example, `limit` bytes (8 in this case) will be read from the
## main `BitStream`. Then, a substream will be created out of them, which will
## then be used as the stream for parsing `fixed`. Since `fixed` will only use
## 4 of them, the remaining 4 will effectively be discarded.
##
## Note that unlike in the type, here size is counted in bytes. It is implied
## that you cannot create a substream if your bitstream is unaligned.
##
## This feature is **not implemented for repetition** because it would increase
## complexity with little benefits. The following syntax is **invalid** and
## instead you should use the technique with the auxiliary complex type shown
## above:
##
## .. code:: nim
##    createParser(parser):
##      u8: a[4](6) # does substream refer to each individual element or the
##                  # whole sequence?
##
## Strings
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Strings are special because they don't have a fixed size. Therefore, you
## must provide enough information regarding their termination. This can be
## achieved with one of the following:
## - Use of substream
## - Assertion
## - Magic
##
## .. code:: nim
##    s: a # null/eos-terminated (because next field doesn't use assertion)
##    s: b(5) # reads a string from a substream of 5 bytes until null/eos
##    s: c = "ABC" # reads a string of length 3 that must match "ABC"
##    s: d # reads a string until next field matches
##    s: _ = "MAGIC"
##    s: e[5] # reads 5 null-terminated strings
##    s: {f} # reads null-terminated strings until next field matches
##    8: term = 0xff # terminator of the above sequence
##    s: {g[5]} # sequence of 5-length sequences of null-terminated strings
##    s: _ = "END_NESTED"
##
## Rules:
## - Strings are null/eos-terminated unless assertion is used on the same field
##   **or** on the next field
## - When using repetition, each string element is null-terminated
##
## Custom parser API
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Since a BinaryLang parser is just a `tuple[get: proc, set: proc]`, you can
## write parsers by hand that are compatible with the DSL. Just be sure that
## `get` and `set` have proper signatures, and there is a type with the same
## name as your parser but capitalized:
##
## .. code:: nim
##    type Parser = SomeType
##    proc get(s: BitStream): Parser
##    proc put(s: BitStream, input: Parser)
##    let parser = (get: get, put: put)
##
## If you want your custom parser to be parametric, simply append more
## parameters to your procs. These extra parameters must be identical and in
## the same order in the two procs:
##
## .. code:: nim
##    type Parser = SomeType
##    proc get(s: BitStream, x: int, y: float): Parser
##    proc put(s: BitStream, input: Parser, x: int, y: float)
##    let parser = (get: get, put: put)
##
## Operations
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Operations can be applied to fields with the following syntax:
##
## .. code::
##    type {op: expr}: name
##
## Operations act on data after the parsing and before the encoding
## respectively. The big restriction here is that an operation cannot alter
## the type, since the parsing/encoding type is always fixed.
##
## An operation is nothing more than a pair of templates which follow a
## specific pattern:
## - The names of the templates **must** follow the pattern: `<operation>get`
##   and `<operation>put`
## - They must have exactly 3 untyped parameters (you can name them as you wish):
##    - **parameter #1**: the field you operate on
##    - **parameter #2**: parsing/encoding statements
##    - **parameter #3**: expression provided
## 
## .. code:: nim
##    template increaseGet(field, parse, num: untyped) =
##      parse
##      field += num
##    template increasePut(field, encode, num: untyped) =
##      field -= num
##      encode
##    createParser(myParser):
##      64: x
##      16 {increase: x}: y
##
## Note that in `increaseGet` we parse *before* operating on `field`, while in
## `increasePut` we encode *after* operating on `field`.
##
## You can also apply more than one operations on one field, in which case they
## are chained in the specified order, and there are some special rules:
## - only the **first** operation has 3 parameters as described above
## - the rest **must** not have a parameter for parsing/encoding, since this is
##   only done once
##
## .. code:: nim
##    template condGet(field, parse, cond: untyped) =
##      if cond:
##        parse
##    template condPut(field, encode, cond: untyped) =
##      if cond:
##        encode
##    template increaseGet(field, num: untyped) =
##      field += num
##    template increasePut(field, num: untyped) =
##      field -= num
##    createParser(myParser):
##      8: shouldParse
##      64: x
##      16 {cond: shouldParse.bool, increase: x}: y
##
## Properties
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Properties follow similar syntax to operations, but the name of the property
## must be prefixed with ``@``.
##
## Properties are meant for making the interaction with the parsed data easier.
##
## There are two big differences compared to operations:
## - They don't affect parsing/encoding - they only change the way the user
##   interacts with data
## - They are built-in features rather than user-defined language extensions
##
## The following properties are available:
## - set
## - get
## - hook
##
## `@set` and `@get` allows you to get a different view of your data, hiding
## the underlying implementation. This is particularly useful when the type
## used to parse the data differs from the one you want to use to interact with
## them in Nim.
##
## `@hook` allows you to run more code each time your field is mutated.
##
## Using any of the properties: `@set`, `@get`, `@hook` forces  the actual name
## of the field to be generated with `genSym`, thus hiding it from the user.
## The specified name is then used for generating a pair of *getter*/*setter*
## procs. This way the extra code run on access/mutation is transparent to the
## user.
##
## - in `@get`: `_` refers to the field
## - in `@set` and in `@hook`: `_` refers to the value being assigned
##   to the field
##
## .. code:: nim
##    createParser(parser):
##      s {@get: _.parseInt, @set: $_}: myInt
##
##    var x: Parser
##    echo x.myInt + 42
##    x.myInt = 24
##
## Special notes
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## - Nim expressions may contain:
##    - a previously defined field
##    - a parser parameter
##    - the `_` symbol for *subject* element (its meaning varies)
##    - the `i` symbol for current index in a repetition
##    - the `s` symbol for accessing the bitstream
##
## `i` and `s` might conflict with your variables or fields, so you should
## consider them reserved keywords and not use them for something else.

import macros, tables, strutils, sugar, strformat
import bitstreams
export bitstreams

type
  MagicError* = object of Defect
  SyntaxError* = object of Defect
  Options = tuple
    endian: Endianness
    bitEndian: Endianness
  OptionSet = enum
    osEndian
    osBitEndian
  Kind = enum
    kInt, kUInt, kFloat, kStr, kCustom
  Type = ref object
    case kind: Kind
    of kCustom:
      symbol: NimNode
      args: seq[NimNode]
    else:
      size: BiggestInt
    endian: Endianness
    bitEndian: Endianness
  Transformations = ref object
    ops: seq[tuple[name: string, arg: NimNode]]
    props: Table[string, NimNode]
  Repeat = enum
    rNo
    rFor
    rUntil
  Value = ref object
    name: string
    case repeat: Repeat
    of rFor, rUntil: repeatExpr: NimNode
    of rNo: discard
    valueExpr: NimNode
    sizeExpr: NimNode
    isMagic: bool
    isExported: bool
  Field = ref object
    typ: Type
    trans: Transformations
    val: Value
    symbol: NimNode
    magic: Field
  Variation = ref object
    case isElseBranch: bool:
    of false:
      cases: seq[NimNode]
    of true:
      discard
    case isEmpty: bool:
    of false:
      fields: seq[Field]
      st: seq[string]
    of true:
      discard

const defaultOptions: Options = (
  endian: bigEndian,
  bitEndian: bigEndian)

macro typeGetter*(body: typed): untyped {.deprecated: "use type directly".} =
  body.getTypeImpl[0][1][0][0]

proc syntaxError() = raise newException(SyntaxError, "Syntax error")
proc syntaxError(message: string) = raise newException(SyntaxError, message)

proc getImpl(typ: Type): NimNode {.compileTime.} =
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
  of kCustom:
    let sym = ident(typ.symbol.strVal.capitalizeAscii)
    result = quote do: `sym`

proc prefixFields(node: var NimNode, st, params: seq[string];
                  with: NimNode) {.compileTime.} =
  if node.kind == nnkIdent:
    if node.strVal in st and node.strVal notin params:
      node = newDotExpr(with, node)
  elif node.kind == nnkDotExpr:
    var
      n0 = node[0]
      n1 = node[1]
    prefixFields(n0, st, params, with)
    if n1.kind != nnkIdent:
      prefixFields(n1, st, params, with)
    node = newDotExpr(n0, n1)
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      prefixFields(n, st, params, with)
      node[i] = n.copyNimTree
      inc i

proc getCustomReader(typ: Type; bs: NimNode; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"get"), bs)
  for arg in typ.args:
    result.add(arg.copyNimTree)
  result.prefixFields(st, params, ident"result")

proc getCustomWriter(typ: Type; bs: NimNode; st, params: seq[string]):
 NimNode {.compileTime.} =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"put"), bs)
  for arg in typ.args:
    result.add(arg.copyNimTree)
  result.prefixFields(st, params, ident"input")

proc replaceWith(node: var NimNode; what, with: NimNode) {.compileTime.} =
  if node.kind == nnkIdent:
    if eqIdent(node, what):
      node = with.copyNimTree
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      n.replaceWith(what, with)
      node[i] = n
      inc i

proc decodeType(t: NimNode, opts: Options): Type {.compileTime.} =
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
    result.kind = kCustom
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

proc decodeTransformations(node: NimNode): Transformations {.compileTime.} =
  result = Transformations()
  for child in node:
    if child[0].kind == nnkIdent:
      result.ops.add((child[0].strVal, child[1].copyNimTree))
    else:
      result.props[child[0][1].strVal] = child[1].copyNimTree

proc decodeValue(node: NimNode, st: var seq[string]): Value {.compileTime.} =
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

proc decodeHeader(input: seq[NimNode]):
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
      else:
        raise newException(Defect, &"Unknown option: {$n[0]}")
    else:
      syntaxError("Invalid header syntax")

proc isInterfaced(f: Field): bool {.compileTime.} =
  f.trans.props.len > 0

proc decodeField(def: NimNode, st: var seq[string], opts: Options):
 Field {.compileTime.} =
  var a, b, c: NimNode
  case def.kind
  of nnkPrefix:
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
    typ: decodeType(a, opts),
    trans: decodeTransformations(b),
    val: decodeValue(c, st))
  result.symbol =
    if result.isInterfaced: ident(result.val.name & "Impl")
    else: ident(result.val.name)

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
  of kCustom:
    let call = getCustomReader(f.typ, bs, st, params)
    result.add(quote do: `sym` = `call`)

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
  of kCustom:
    let call = getCustomWriter(f.typ, bs, st, params)
    call.insert(2, sym)
    result.add(quote do: `call`)

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

proc generateRead(sym: NimNode; f: Field; bs: NimNode;
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

proc generateReader(fields: seq[Field]; fst, pst: seq[string]):
 NimNode {.compileTime.} =
  let
    bs = ident"s"
    res = ident"result"
  result = newStmtList()
  for f in fields:
    let
      rSym = genSym(nskVar)
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
    if f.trans.ops.len != 0:
      for i in 0 .. f.trans.ops.len - 1:
        let
          k = f.trans.ops[i].name
          v = f.trans.ops[i].arg
        var val = v.copyNimTree
        val.prefixFields(fst, pst, res)
        val.replaceWith(ident"_", rSym)
        if i == 0:
          result.add(newCall(ident(k & "get"), rSym, read, val))
        else:
          result.add(newCall(ident(k & "get"), rSym, val))
    else:
      result.add(read)
    if field != "":
      result.add(quote do:
        result.`ident` = `rSym`)

proc generateWriter(fields: seq[Field]; fst, pst: seq[string]):
 NimNode {.compileTime.} =
  result = newStmtList()
  let
    bs = ident"s"
    input = ident"input"
  for f in fields:
    let
      wSym = genSym(nskVar)
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
    var
      write = generateWrite(wSym, f, bs, fst, pst)
    if f.trans.ops.len != 0:
      for i in countdown(f.trans.ops.len - 1, 0):
        let
          k = f.trans.ops[i].name
          v = f.trans.ops[i].arg
        var val = v.copyNimTree
        val.prefixFields(fst, pst, input)
        val.replaceWith(ident"_", wSym)
        if i == 0:
          write = generateWrite(wSym, f, bs, fst, pst)
          result.add(newCall(ident(k & "put"), wSym, write, val))
        else:
          result.add(newCall(ident(k & "put"), wSym, val))
    else:
      result.add(write)

proc generateProperties(parserType: NimNode; f: Field;
                        fst, pst: seq[string]): seq[NimNode] {.compileTime.} =
  var getProp, setProp: NimNode
  let
    ident = f.symbol
    objGet = genSym(nskParam)
    targetField = newDotExpr(objGet, ident)
  var expr: NimNode
  if f.trans.props.hasKey("get"):
    expr = f.trans.props["get"].copyNimTree
    expr.replaceWith(ident"_", targetField)
    expr.prefixFields(fst, pst, objGet)
  else:
    expr = targetField
  let getSym = ident(f.val.name)
  getProp = newProc(
    if f.val.isExported: postfix(getSym, "*") else: getSym,
    @[ident"auto",
      newIdentDefs(objGet, parserType)],
    expr)
  result.add(getProp)
  let
    objPut = genSym(nskParam)
    val = ident"x"
  if f.trans.props.hasKey("set"):
    expr = f.trans.props["set"].copyNimTree
    expr.replaceWith(ident"_", val)
    expr.prefixFields(fst, pst, objPut)
  else:
    expr = val
  let setSym = nnkAccQuoted.newTree(
    ident(f.val.name),
    ident"=")
  setProp = newProc(
    if f.val.isExported: postfix(setSym, "*") else: setSym,
    @[newEmptyNode(),
      newIdentDefs(objPut, nnkVarTy.newTree(parserType)),
      newIdentDefs(val, ident"any")],
    newStmtList(
      newAssignment(
        newDotExpr(objPut, ident),
        expr)))
  if f.trans.props.hasKey("hook"):
    expr = f.trans.props["hook"].copyNimTree
    expr.replaceWith(ident"_", val)
    expr.prefixFields(fst, pst, objPut)
    setProp[6].insert(0, expr)
  result.add(setProp)

proc generateConverters(tname, pname: NimNode; params: seq[NimNode];
                        isExported: bool): tuple[to, `from`: NimNode]
 {.compileTime.} =
  var
    toConv = ident("to" & tname.strVal)
    fromConv = ident("from" & tname.strVal)
    procToParams = @[tname, newIdentDefs(ident"x", ident"string")]
    procToBody = newCall(
      newDotExpr(
        pname,
        ident"get"),
      newCall(
        ident"newStringBitStream",
        ident"x"))
    procFromParams = @[ident"string", newIdentDefs(ident"x", tname)]
    putCall = newCall(
      newDotExpr(
        pname,
        ident"put"),
      ident"s",
      ident"x")
  if isExported:
    toConv = postfix(toConv, "*")
    fromConv = postfix(fromConv, "*")
  for p in params:
    procToParams.add(p.copyNimTree)
    procToBody.add(p[0].copyNimTree)
    procFromParams.add(p.copyNimTree)
    putCall.add(p[0].copyNimTree)
  result = (
    newProc(
      ident("to" & tname.strVal),
      procToParams,
      procToBody),
    newProc(
      ident("from" & tname.strVal),
      procFromParams,
      newStmtList(
        (quote do:
          let s {.inject.} = newStringBitStream()),
        putCall,
        (quote do: s.seek(0)),
        (quote do: s.readAll))))

macro createParser*(name: untyped, rest: varargs[untyped]): untyped =
  ## Input:
  ## - `name`: the name of the parser tuple to create (must be lowercase)
  ## - `rest`: **optionally** parser options and parameters
  ## - `rest` (last): a block of the format described above
  ##
  ## Output:
  ## - Object type declaration with name
  ##   `tname` ≡ `capitalizeAscii(name)`
  ## - Reader proc that returns an object of the type `tname`
  ## - Writer proc that accepts an object of type `tname`
  ## - A tuple named `name` with the fields `get` and `put`
  ##
  ## The procs are of the following form:
  ##
  ## .. code-block:: nim
  ##   proc get(s: BitStream): `tname`
  ##   proc put(s: BitStream, input: `tname`)
  result = newStmtList()
  name.expectKind({nnkIdent, nnkPrefix})
  var
    pname: NimNode
    pdef: NimNode
    tname: NimNode
    tdef: NimNode
    isExported: bool
  case name.kind
  of nnkIdent:
    if name.strVal[0].isUpperAscii:
      syntaxError("Parser name must be lowercase")
    pname = name.copyNimTree
    pdef = name.copyNimTree
    tname = ident(name.strVal.capitalizeAscii)
    tdef = ident(name.strVal.capitalizeAscii)
    isExported = false
  of nnkPrefix:
    if name[0].strVal != "*":
      syntaxError("Invalid prefix operator for parser name")
    pname = name[1].copyNimTree
    pdef = postfix(name[1].copyNimTree, "*")
    tname = ident(name[1].strVal.capitalizeAscii)
    tdef = postfix(ident(name[1].strVal.capitalizeAscii), "*")
    isExported = true
  else:
    syntaxError("Invalid syntax for parser name")
  var
    fieldDefs = newTree(nnkRecList)
    fieldsSymbolTable = newSeq[string]()
  let
    bs = ident"s"
    input = ident"input"
    (params, parserOptions) = decodeHeader(rest[0 .. ^2])
    paramsSymbolTable = collect(newSeq):
      for p in params:
        p[0].strVal
  var fields = collect(newSeq):
    for def in rest[^1]:
      decodeField(def, fieldsSymbolTable, parserOptions)
  for i in 0 ..< fields.len - 1:
    if fields[i].val.isMagic or
       (fields[i].typ.kind == kStr and fields[i+1].val.valueExpr != nil):
      if fields[i+1].val.valueExpr == nil:
        raise newException(Defect,
          "Magic was used without assertion at the next field")
      fields[i].magic = fields[i+1]
  var reader = generateReader(fields, fieldsSymbolTable, paramsSymbolTable)
  reader.insert(0, newAssignment(
    ident"result",
    newCall(tname)))
  let writer = generateWriter(fields, fieldsSymbolTable, paramsSymbolTable)
  for f in fields:
    let
      ident = f.symbol
      field = ident.strVal
    var impl = f.typ.getImpl
    if f.val.repeat != rNo:
      impl = quote do: seq[`impl`]
    if f.val.isMagic:
      impl = quote do: seq[`impl`]
    if field != "":
      fieldDefs.add(
        newIdentDefs(
          if f.val.isExported: postfix(f.symbol, "*")
          else: f.symbol,
        impl))
  result.add(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        tdef,
        newEmptyNode(),
        nnkRefTy.newTree(
          nnkObjectTy.newTree(
            newEmptyNode(),
            newEmptyNode(),
            fieldDefs)))))
  for f in fields:
    if f.isInterfaced:
      result.add(
        generateProperties(
          tname,
          f,
          fieldsSymbolTable,
          paramsSymbolTable))
  let
    readerName = genSym(nskProc)
    writerName = genSym(nskProc)
  var
    readerProcForwardDecl = quote do:
      proc `readerName`(`bs`: BitStream): `tname`
    writerProcForwardDecl = quote do:
      proc `writerName`(`bs`: BitStream, `input`: `tname`)
    readerProc = quote do:
      proc `readerName`(`bs`: BitStream): `tname` =
        `reader`
    writerProc = quote do:
      proc `writerName`(`bs`: BitStream, `input`: `tname`) =
        `writer`
  for p in params:
    readerProcForwardDecl[3].add p.copyNimTree
    writerProcForwardDecl[3].add p.copyNimTree
    readerProc[3].add p.copyNimTree
    writerProc[3].add p.copyNimTree
  let (procTo, procFrom) = generateConverters(tname, pname, params, isExported)
  result.add(quote do:
    `readerProcForwardDecl`
    `writerProcForwardDecl`
    let `pdef` = (get: `readerName`, put: `writerName`)
    `readerProc`
    `writerProc`
    `procTo`
    `procFrom`)
  when defined(BinaryLangEcho):
    echo repr result

proc decodeVariation(def: NimNode, st: seq[string], opts: Options):
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
    def[0].expectKind(nnkPar)
    for c in def[0]:
      cases.add(c.copyNimTree)
    result.cases = cases
  if not isEmpty:
    var symbolTable = st
    for f in def[1]:
      fields.add(decodeField(f, symbolTable, opts))
    result.fields = fields
    result.st = symbolTable

macro createVariantParser*(name, disc: untyped; rest: varargs[untyped]): untyped =
  ## Input:
  ## - `name`: the name of the parser tuple to create (must be lowercase)
  ## - `disc`: the definition of the discriminator field (`name: type`)
  ## - `rest`: **optionally** parser options and parameters
  ## - `rest` (last): a block of the format described above
  ##
  ## Output:
  ## - **Variant** object type declaration with discriminator `disc` and name
  ##   `tname` ≡ `capitalizeAscii(name)`
  ## - Reader proc that returns an object of the type `tname`
  ## - Writer proc that accepts an object of type `tname`
  ## - A tuple named `name` with the fields `get` and `put`
  ##
  ## The procs are of the following form:
  ##
  ## .. code-block:: nim
  ##   proc get(s: BitStream): `tname`
  ##   proc put(s: BitStream, input: `tname`)
  ##
  ## The body is similar to that of `createParser` macro, but the fields are
  ## partitioned in branches. Each branch starts with one or more possible
  ## value of the discriminator in parenthesis, seperated by comma.
  ##
  ## For covering the rest of the cases use the `_` symbol (without
  ## parenthesis).
  ##
  ## If you don't want a field for some branch, use `nil` on the right side.
  ##
  ## Example:
  ##
  ## .. code-block:: nim
  ##   createVariantParser(fooBar, disc: int):
  ##     (0): *foo: a
  ##     (1, 3): u32: *b
  ##     (2): nil
  ##     (4):
  ##       u8: c
  ##       *bar: d
  ##     _: u32: e
  result = newStmtList()

  name.expectKind({nnkIdent, nnkPrefix})
  disc.expectKind(nnkExprColonExpr)
  var
    pname: NimNode
    pdef: NimNode
    tname: NimNode
    tdef: NimNode
    isExported: bool
  case name.kind
  of nnkIdent:
    if name.strVal[0].isUpperAscii:
      syntaxError("Parser name must be lowercase")
    pname = name.copyNimTree
    pdef = name.copyNimTree
    tname = ident(name.strVal.capitalizeAscii)
    tdef = ident(name.strVal.capitalizeAscii)
    isExported = false
  of nnkPrefix:
    if name[0].strVal != "*":
      syntaxError("Invalid prefix operator for parser name")
    pname = name[1].copyNimTree
    pdef = postfix(name[1].copyNimTree, "*")
    tname = ident(name[1].strVal.capitalizeAscii)
    tdef = postfix(ident(name[1].strVal.capitalizeAscii), "*")
    isExported = true
  else:
    syntaxError("Invalid syntax for parser name")
  let
    input = ident"input"
    bs = ident"s"
    discType = disc[1]
    (extraParams, parserOptions) = decodeHeader(rest[0 .. ^2])
  var
    discName: NimNode
    objectMeat = newTree(nnkRecCase)
  case disc[0].kind
  of nnkIdent:
    discName = disc[0]
    objectMeat.add(
      newIdentDefs(
        discName,
        discType))
  of nnkPrefix:
    discName = disc[0][1]
    objectMeat.add(
      newIdentDefs(
        postfix(
          discName,
          "*"),
        discType))
  else:
    syntaxError()
  let
    params = newIdentDefs(discName, discType) & extraParams
    paramsSymbolTable = collect(newSeq):
      for p in params:
        p[0].strVal
  var
    variations = collect(newSeq):
      for def in rest[^1]:
        decodeVariation(def, paramsSymbolTable, parserOptions)
  for v in variations:
    if v.isEmpty:
      continue
    for i in 0 ..< v.fields.len - 1:
      if v.fields[i].val.isMagic or
        (v.fields[i].typ.kind == kStr and v.fields[i+1].val.valueExpr != nil):
        if v.fields[i+1].val.valueExpr == nil:
          raise newException(Defect,
            "Magic was used without assertion at the next field")
        v.fields[i].magic = v.fields[i+1]
  for v in variations:
    let left =
      if v.isEmpty:
        newNilLit()
      else:
        var rl = newTree(nnkRecList)
        for f in v.fields:
          if f.val.name != "":
            var impl = f.typ.getImpl
            if f.val.repeat != rNo:
              impl = quote do: seq[`impl`]
            if f.val.isMagic:
              impl = quote do: seq[`impl`]
            rl.add(
              newIdentDefs(
                if f.val.isExported: postfix(f.symbol, "*")
                else: f.symbol,
                impl))
        rl
    if v.isElseBranch:
      objectMeat.add(
        nnkElse.newTree(left))
    else:
      var branch = newTree(nnkOfBranch)
      branch.add(v.cases)
      branch.add(left)
      objectMeat.add(branch)
  result.add(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        tdef,
        newEmptyNode(),
        nnkRefTy.newTree(
          nnkObjectTy.newTree(
            newEmptyNode(),
            newEmptyNode(),
            nnkRecList.newTree(
              objectMeat))))))
  for v in variations:
    if not v.isEmpty:
      for f in v.fields:
        if f.isInterfaced:
          result.add(
            generateProperties(
              tname,
              f,
              v.st,
              paramsSymbolTable))
  let readerName = genSym(nskProc)
  var getCaseStmt = nnkCaseStmt.newTree(discName)
  let readerProcForwardDecl = quote do:
    proc `readerName`(`bs`: BitStream): `tname`
  for v in variations:
    let inner =
      if v.isEmpty:
        nnkDiscardStmt.newTree(newEmptyNode())
      else:
        generateReader(v.fields, v.st, paramsSymbolTable)
    if v.isElseBranch:
      getCaseStmt.add(nnkElse.newTree(inner))
    else:
      var branch = newTree(nnkOfBranch)
      for b in v.cases:
        branch.add(b)
      branch.add(inner)
      getCaseStmt.add(branch)
  let reader = newStmtList(
    newAssignment(
      ident"result",
      nnkObjConstr.newTree(
        tname,
        newColonExpr(
          discName,
          discName))),
    getCaseStmt)
  var readerProc = quote do:
    proc `readerName`(`bs`: BitStream): `tname` =
      `reader`
  let writerName = genSym(nskProc)
  var writerProcForwardDecl = quote do:
    proc `writerName`(`bs`: BitStream, `input`: `tname`)
  var writer = nnkCaseStmt.newTree(discName)
  for v in variations:
    let inner =
      if v.isEmpty:
        nnkDiscardStmt.newTree(newEmptyNode())
      else:
        generateWriter(v.fields, v.st, paramsSymbolTable)
    if v.isElseBranch:
      writer.add(nnkElse.newTree(inner))
    else:
      var branch = newTree(nnkOfBranch)
      for b in v.cases:
        branch.add(b)
      branch.add(inner)
      writer.add(branch)
  var writerProc = quote do:
    proc `writerName`(`bs`: BitStream, `input`: `tname`) =
      `writer`
  for p in params:
    readerProcForwardDecl[3].add p.copyNimTree
    writerProcForwardDecl[3].add p.copyNimTree
    readerProc[3].add p.copyNimTree
    writerProc[3].add p.copyNimTree
  let (procTo, procFrom) = generateConverters(tname, pname, params, isExported)
  result.add(quote do:
    `readerProcForwardDecl`
    `writerProcForwardDecl`
    let `pdef` = (get: `readerName`, put: `writerName`)
    `readerProc`
    `writerProc`
    `procTo`
    `procFrom`)
  when defined(BinaryLangEcho):
    echo repr result