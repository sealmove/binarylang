## BinaryLang is a DSL for creating binary parsers/encoders.
## It exports the macro ``createParser`` which generates a ``tuple[get: proc, put: proc]``.
##
## - ``get`` returns a tuple with each parsed field
## - ``put`` writes a compatible tuple to a stream
##
## BinaryLang is a complete rewrite of `binaryparse <https://github.com/PMunch/binaryparse>`_
## by `PMunch <https://github.com/PMunch>`_ (special thanks).
## The codebase is a lot cleaner and the bit fiddling has been extracted to a separate
## library (`bitstreams <https://github.com/sealmove/bitstreams>`_) which BinaryLang depends on.
## Moreover, a lot of features have been added and there is a plan to implement a plugin system for
## user-defined language extensions.
##
## The macro accepts 3 kind of things:
##
## -  Parser options
## -  Parser parameters
## -  Block with DSL statements
##
## Parser options
## --------------
##
## Each specified option must be in the form ``option = value``:
##
## - ``endian``: sets the default byte endianness for the whole parser
##    - *default*: big endian
##    - ``b``: **big** endian
##    - ``l``: **little** endian
## - ``bitEndian``: sets the default bit endianness for the whole parser
##    - *default*: left -> right
##    - ``n``: left -> right (**normal**)
##    - ``r``: left <- right (**reverse**)
##
## Parser parameters
## -----------------
##
## Each parameter must be in the form ``symbol: type``. The generated
## ``get``/``put`` procs will then have this additional parameter appended.
##
## DSL
## ----
##
## Each statement corresponds to 1 field. The general syntax is:
##
## .. code:: nim
##
##     Type {Operations}: Value
##
## where ``{Operations}`` is optional and refers to a plugin system (see
## below).
##
## Type
## ~~~~
##
## The **kind**, **endianness** and **size** are encoded in a identifier
## made up of:
##
## - 1 optional letter specifying the kind:
##    - *default*: signed integer
##    - ``u``: **unsigned** integer
##    - ``f``: **float**
##    - ``s``: **string**
##    - ``*``: complex (see below)
## - 1 optional letter specifying byte endianness:
##    - *default*: big endian
##    - ``b``: **big** endian
##    - ``l``: **little** endian
## - 1 optional letter specifying bit endianness for unaligned reads:
##    - *default*: left -> right
##    - ``n``: left -> right (**normal**)
##    - ``r``: left <- right (**reverse**)
## - 1 number specifying size in **bits**:
##    - for a string it refers to the size of each individual and defaults to ``8``
##    - for an integer the allowed values are ``1 .. 64``
##    - for a float the allowed values are ``32`` and ``64``
##    - for a custom it can't be used (use the secondary ``size`` operation)
##
## You can order options however you want, but size must come last (e.g. ``lru16`` and ``url16`` are valid but not ``16lru``).
##
## Value
## ~~~~~
##
## This section includes the following features (only name is mandatory):
##
## - name
## - repetition
## - assertion
##
## If you don't *really* want a name, you can discard the symbol by using ``_`` in its place:
##
## .. code:: nim
##
##     createParser(myParser):
##       8: _
##
## Alignment
## ~~~~~~~~~
##
## If any of the following is violated, BinaryLang should generate an exception:
##
## - Byte endianness can only be used with byte-multiple integers
## - Bit endianness must be uniform between **byte boundaries**
## - Spec must finish on a byte boundary
##
## .. code:: nim
##
##    createParser(myParser, bitEndian = n):
##      b9: a # error: cannot apply byte endianness
##      r6: b # error: shares bits with previous byte
##      10: c # error: spec does not finish on a byte boundary
##
## Moreover, unaligned reads for strings are not supported:
##
## .. code:: nim
##
##     createParser(myParser):
##       6: x
##       s: y # invalid, generates an exception
##
## Assertion
## ~~~~~~~~~
##
## Use ``= expr`` for producing an exception if the parsed value doesn't
## match ``expr``:
##
## .. code:: nim
##
##     s: x = "BinaryLang is awesome"
##     8: y[5] = @[0, 1, 2, 3, 4]
##
## Assertion can also be used in a special manner to terminate the previous
## field if it's a **string** or a **sequence indicated as magic-terminated**.
## This is discussed in later sections.
##
## Complex types
## ~~~~~~~~~~~~~
##
## Instead of the described identifier for specifying ``Type``, you can
## call a previously defined parser by using ``*`` followed by the name of
## the parser. If your parser is parametric you must pass arguments to it
## with standard call syntax.
##
## Example:
##
## .. code:: nim
##
##     createParser(inner):
##       32: a
##       32: b
##
##     createParser(innerWithArgs, size: int32):
##       32: a
##       32: b[size]
##
##     createParser(outer):
##       *inner: x
##       *innerWithArgs(x.a): y
##
## Repetition
## ~~~~~~~~~~
##
## There are 3 ways to produce a ``seq`` of your ``Type``:
##
## - ``for``: append ``[expr]`` to the name for repeating ``expr``
##   times
## - ``until``: append ``{expr}`` to the name for repeating until
##   ``expr`` is evaluated to ``true``
## - ``magic``: enclose name with ``{}`` and use assertion with
##   your **next** field
##
## In until repetition you can use the implicitly defined symbol ``e`` to get last element read.
##
## .. code:: nim
##
##     8: a[5] # reads 5 8-bit integers
##     8: b{e == 103 or i > 9} # reads until it finds the value 103 or completes 10th iteration
##     8: {c} # reads 8-bit integers until next field is matches
##     16: _ = 0xABCD
##     u8: {d[5]} # reads byte sequences each of length 5 until next field matches
##     s: _ = "END"
##
## Due to current limitations of the underlying bitstream implementation, to perform magic,
## your stream must be aligned and all the reads involved must also be aligned. This will
## be fixed in the future.
##
## Substreams
## ~~~~~~~~~~
##
## Call syntax forces the creation of a substream:
##
## .. code:: nim
##
##     createParser(aux, size: int):
##       8: x[size]
##     createParser(myParser):
##       8: x = 4
##       8: limit = 8
##       *aux(x): fixed(limit)
##
## In the above example, ``limit`` bytes (8 in this case) will be read from the main ``BitStream``.
## Then, a substream will be created out of them, which will then be used as the stream for parsing ``fixed``.
## Since ``fixed`` will only use 4 of them, the remaining 4 will effectively be discarded.
##
## Note that unlike in ``Type``, here size is counted bytes. It is implied that you cannot create
## a substream if your bitstream is unaligned.
##
## This feature is **not implemented for repetition** because it would increase complexity with little benefits.
## The following syntax is **invalid** and you should use the technique with the auxiliary complex type shown above:
##
## .. code:: nim
##
##     createParser(myParser):
##       u8: a[4](6) # does substream refer to each individual element or the whole sequence?
##
## Strings
## ~~~~~~~
##
## Strings are special because they don't have a fixed size. Therefore, you
## must provide enough information regarding their termination. This can be
## achieved with one of the following:
##
## - Use of substream
## - Assertion
## - Magic
##
## .. code:: nim
##
##     s: a # null/eos-terminated (because next field doesn't use assertion)
##     s: b(5) # reads a string from a substream of 5 bytes until null/eos
##     s: c = "ABC" # reads a string of length 3 that must match "ABC"
##     s: d # reads a string until next field is matched
##     s: _ = "MAGIC"
##     s: e[5] # reads 5 null-terminated strings
##     s: {f} # reads null-terminated strings until next field matches
##     8: term = 0xff # terminator of the above sequence
##     s: {g[5]} # sequence of 5-length sequences of null-terminated strings
##     s: _ = "END_NESTED"
##
## Rules:
##
## - Strings are null/eos-terminated unless assertion is used on the same field
##   **or** the next one
## - When using repetition, each string element is null-terminated
##
## Custom parser API
## ~~~~~~~~~~~~~~~~~
##
## Since a BinaryLang parser is just a ``tuple[get: proc, set: proc]``,
## you can write parsers by hand that are compatible with the DSL. Just be
## sure that ``get`` and ``set`` have a proper signature:
##
## .. code:: nim
##
##     type parserTy = tuple[...]
##     proc get(s: BitStream): parserTy
##     proc put(s: BitStream, input: parserTy)
##     let parser = (get: get, put: put)
##
## If you want your custom parser to be parametric, simply append more
## parameters to your procs. These extra parameters must be identical and
## in the same order in the two procs.
##
## Example:
##
## .. code:: nim
##
##     type parserTy = tuple[...]
##     proc get(s: BitStream, x: int, y: float): parserTy
##     proc put(s: BitStream, input: parserTy, x: int, y: float)
##     let parser = (get: get, put: put)
##
## Operations (plugins)
## ~~~~~~~~~~~~~~~~~~~~
##
## Plugins are **user-defined** keys which define an operation on a field.
## They are parametric, which means they also have a value. The API for
## writing plugins is not designed yet, but the syntax for using them is:
##
## .. code:: nim
##
##     Type {plugin: expr}: Value
##
## Examples of plugins
## ~~~~~~~~~~~~~~~~~~~
##
## - ``pos``: positions the ``stream`` at byte ``value`` before parsing and then
##   resets it to the previous position
## - ``cond``: wraps the field into an ``Option`` type and will only parse it if
##   ``value`` is evaluated to ``true``
##
## You can combine multiple operations which will be applied to the field
## in the specified order:
##
## .. code:: nim
##
##     8: shouldParse
##     64: position
##     16 {cond: shouldParse.bool, pos: position}: x
##
## First ``shouldParse.bool`` will be evaluted. If it's ``false``, parsing
## won't happen; if it's true, then the stream will be positioned at ``pos``
## and ``x`` will be parsed from there. Finally, the stream will be positioned
## where it was originally. The result will be wrapped into an
## ``Option`` and the resulting field will be an ``Option[int16]``.
##
## Special notes
## ~~~~~~~~~~~~~
##
## - Nim expressions may contain:
##    - a previously defined field
##    - a parser parameter
##    - the ``e`` symbol for getting the last element read in a repetition
##    - the ``i`` symbol for current index in a repetition
##    - the ``s`` symbol for accessing the bitstream
##
## These last 3 symbols might conflict with your variables or fields, so you
## shouldn't use them for something else.
##

import macros, tables, strutils, sugar, strformat
import bitstreams

type
  MagicError* = object of Defect
    ## Error raised from the parser procedure when a magic sequence is not
    ## matching the specified value.
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
  Operations = OrderedTable[string, NimNode]
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
  Field = ref object
    typ: Type
    opts: Operations
    val: Value
    magic: Field

const defaultOptions: Options = (
  endian: bigEndian,
  bitEndian: bigEndian)

macro typeGetter*(body: typed): untyped =
  ## Helper macro to get the return type of custom parsers
  body.getTypeImpl[0][1][0][0]

proc syntaxError() = raise newException(Defect, "Invalid syntax")

proc getImpl(typ: Type): NimNode =
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
    var sym = typ.symbol
    result = quote do: typeGetter(`sym`)

proc prefixFields(node: var NimNode, st, params: seq[string], with: NimNode) =
  if node.kind == nnkIdent:
    if node.strVal in st and node.strVal notin params:
      node = newDotExpr(with, node)
  elif node.kind == nnkDotExpr:
    var n = node[1]
    prefixFields(n, st, params, with)
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      prefixFields(n, st, params, with)
      node[i] = n.copyNimTree
      inc i

proc getCustomReader(typ: Type, bs: NimNode, st, params: seq[string]): NimNode =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"get"), bs)
  for arg in typ.args:
    result.add(arg.copyNimTree)
  result.prefixFields(st, params, ident"result")

proc getCustomWriter(typ: Type, bs: NimNode, st, params: seq[string]): NimNode =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"put"), bs)
  for arg in typ.args:
    result.add(arg.copyNimTree)
  result.prefixFields(st, params, ident"input")

proc replaceWith(node: var NimNode; what, with: NimNode) =
  if node.kind == nnkIdent:
    if eqIdent(node, what):
      node = with
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      n.replaceWith(what, with)
      node[i] = n
      inc i

proc decodeType(t: NimNode; seenFields, params: seq[string]; opts: Options): Type =
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
    result.symbol = t[0]
    var i = 1
    while i < t.len:
      result.args.add(t[i].copyNimTree)
      inc i
  else:
    syntaxError()
  result.endian = endian
  result.bitEndian = bitEndian

proc decodeOperations(node: NimNode): Operations =
  result = initOrderedTable[string, NimNode]()
  for child in node:
    result[child[0].strVal] = child[1]

proc decodeValue(node: NimNode, st: var seq[string], params: seq[string]): Value =
  var node = node
  result = Value()
  if node.kind == nnkAsgn:
    result.valueExpr = node[1]
    node = node[0]
  if node.kind == nnkCurly:
    if result.valueExpr != nil:
      raise newException(Defect,
        "Magic and assertion can't be used together in the same field")
    result.isMagic = true
    node = node[0]
  if node.kind == nnkBracketExpr:
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
  if node.kind != nnkIdent:
    syntaxError()
  if node.strVal != "_":
    result.name = node.strVal
    st.add(result.name)

proc decodeHeader(input: seq[NimNode]): tuple[params: seq[NimNode], opts: Options] =
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
      syntaxError()

proc decodeField(def: NimNode, st: var seq[string], params: seq[string],
                 opts: Options): Field =
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
      a = newCall(def[1][0].copyNimTree)
      b = def[1][1].copyNimTree
    else: syntaxError()
  of nnkCall:
    a = def[0].copyNimTree
    c = def[1][0].copyNimTree
  of nnkCommand:
    a = def[0].copyNimTree
    b = def[1].copyNimTree
    c = def[2][0].copyNimTree
  else: syntaxError()
  result = Field(
    typ: decodeType(a, st, params, opts),
    opts: decodeOperations(b),
    val: decodeValue(c, st, params))

proc createReadStatement(sym, bs: NimNode, f: Field, st, params: seq[string]): NimNode {.compileTime.} =
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

proc createWriteStatement(f: Field, sym, bs: NimNode, st, params: seq[string]): NimNode {.compileTime.} =
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
      let tmp = genSym(nskVar)
      result.add(quote do:
        var `tmp` = `sym`)
      if size in {8, 16, 32, 64}:
        let procAligned = ident("write" & endian & "e")
        result.add(quote do:
          if isAligned(`bs`):
            `procAligned`(`bs`, `impl`(`tmp`))
          else:
            `procUnaligned`(`bs`, `sizeNode`, `tmp`, `endianNode`))
      else:
        result.add(quote do: `procUnaligned`(`bs`, `sizeNode`, `tmp`, `endianNode`))
  of kStr:
    result.add(quote do:
      if not isAligned(`bs`):
        raise newException(IOError, "Stream must be aligned to write a string"))
    if sym != nil:
      let tmp = genSym(nskVar)
      result.add(quote do:
        var `tmp` = `sym`
        writeStr(`bs`, `tmp`))
    if f.val.valueExpr == nil and (f.magic == nil or f.val.isMagic):
      result.add(quote do:
        writeBe(`bs`, 0'u8))
  of kCustom:
    let call = getCustomWriter(f.typ, bs, st, params)
    call.insert(2, sym)
    result.add(quote do: `call`)

proc createReadField(sym: NimNode; f: Field; bs: NimNode; st, params: seq[string]): NimNode =
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
      expr.replaceWith(ident"e", tmp)
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

proc createWriteField(f: Field; bs: NimNode; st, params: seq[string]): NimNode =
  result = newStmtList()
  let
    field = f.val.name
    fieldIdent = ident(field)
    input = ident"input"
    tmp = genSym(nskVar)
    impl = f.typ.getImpl
    elem = if f.val.isMagic: genSym(nskForVar)
           else: quote do: `input`.`fieldIdent`
  var value = f.val.valueExpr
  value.prefixFields(st, params, input)
  var writeStmts = newStmtList()
  case f.val.repeat
  of rNo:
    let sym = if field == "": (if value == nil: nil else: value)
              else: quote do: `elem`
    writeStmts.add createWriteStatement(f, sym, bs, st, params)
  of rFor:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, input)
    writeStmts.add(
      if field == "":
        if value == nil:
          quote do:
            var `tmp` = newSeq[`impl`](`expr`)
        else:
          quote do:
            var `tmp` = `value`
      else:
        quote do:
          var `tmp` = `elem`)
    let
      loopIdx = ident"i"
      loopElem = genSym(nskForVar)
      writeStmt = createWriteStatement(f, loopElem, bs, st, params)
    writeStmts.add(quote do:
      for `loopIdx`, `loopElem` in `tmp`:
        `writeStmt`)
  of rUntil:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, input)
    let
      sym = genSym(nskForVar)
      loopIdx = ident"i"
    expr.replaceWith(ident"e", sym)
    expr.replaceWith(ident"s", bs)
    writeStmts.add(
      if field == "":
        if value == nil:
          quote do:
            var `tmp` = newSeq[`impl`](`expr`)
        else:
          quote do:
            var `tmp` = `value`
      else:
        quote do:
          var `tmp` = `elem`)
    let writeStmt = createWriteStatement(f, sym, bs, st, params)
    writeStmts.add(quote do:
      for `loopIdx`, `sym` in `tmp`: `writeStmt`)
  if f.val.isMagic:
    result.add(quote do:
      for `elem` in `input`.`fieldIdent`:
        `writeStmts`)
  else:
    result.add(writeStmts)

proc generateRead(sym: NimNode; f: Field; bs: NimNode, st, params: seq[string]): NimNode =
  result = newStmtList()
  let
    res = ident"result"
    field = f.val.name
    fieldIdent = ident(field)
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
      sym = genSym(nskVar)
      ss = genSym(nskVar)
      rf = createReadField(sym, f, ss, st, params)
    result.add(quote do:
      var
        `ss` = createSubstream(`bs`, int(`size`))
        `sym`: `impl`
      `rf`)
    if value != nil:
      result.add(quote do:
        if `sym` != (`value`):
          raise newException(MagicError, "field '" & $`field` & "' was " &
                            $`sym` & " instead of " & $`value`))
    if field != "":
      result.add(quote do:
        result.`fieldIdent` = `sym`)
  elif isSingleStr and f.magic != nil:
    let
      str = genSym(nskVar)
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
        `str`: string
        `tmp`: `tmpImpl`
      while true:
        let `pos` = getPosition(`bs`)
        `rf`
        `bs`.seek(`pos`)
        if `tmp` == `magicVal`:
          break
        `str`.add(readU8(`bs`).char))
    if field != "":
      result.add(quote do:
        result.`fieldIdent` = `str`)
  elif f.val.isMagic:
    let
      arr = genSym(nskVar)
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
        `arr`: seq[`elemImpl`]
        `elem`: `elemImpl`
        `magic`: `magicImpl`
      while true:
        let `pos` = getPosition(`bs`)
        `readMagic`
        `bs`.seek(`pos`)
        if `magic` == `magicVal`:
          break
        `readElem`
        `arr`.add(`elem`))
    if field != "":
      result.add(quote do:
        result.`fieldIdent` = `arr`)
  else:
    let
      sym = genSym(nskVar)
      rf = createReadField(sym, f, bs, st, params)
    result.add(quote do:
      var `sym`: `impl`
      `rf`)
    if value != nil:
      result.add(quote do:
        if `sym` != (`value`):
          raise newException(MagicError, "field '" & $`field` & "' was " &
                            $`sym` & " instead of " & $`value`))
    if field != "":
      result.add(quote do:
        result.`fieldIdent` = `sym`)

proc generateWrite(f: Field; bs: NimNode, st, params: seq[string]): NimNode =
  result = newStmtList()
  let input = ident"input"
  if f.val.sizeExpr != nil:
    var size = f.val.sizeExpr.copyNimTree
    size.prefixFields(st, st, input)
    let
      ss = genSym(nskVar)
      wf = createWriteField(f, ss, st, params)
    result.add(quote do:
      var `ss` = newPaddedBitStream(int(`size`))
      `wf`
      `ss`.seek(0)
      `bs`.writeFromSubstream(`ss`, int(`size`)))
  else:
    result.add createWriteField(f, bs, st, params)

macro createParser*(name: untyped, rest: varargs[untyped]): untyped =
  ## The main macro in this module. It takes the ``name`` of the tuple to
  ## create along with a block on the format described above and creates a
  ## reader and a writer for it. The output is a tuple with ``name`` that has
  ## two fields ``get`` and ``put``. Get is on the form
  ## ``proc (bs: BitStream): tuple[<fields>]`` and put is
  ## ``proc (bs: BitStream, input: tuple[<fields>])``
  var
    tupleMeat = newTree(nnkTupleTy)
    fieldsSymbolTable = newSeq[string]()
    reader = newStmtList()
    writer = newStmtList()
  let
    bs = ident"s"
    input = ident"input"
    (params, parserOptions) = decodeHeader(rest[0 .. ^2])
    paramsSymbolTable = collect(newSeq):
      for p in params:
        p[0].strVal
  var fields = collect(newSeq):
    for def in rest[^1]:
      decodeField(def, fieldsSymbolTable, paramsSymbolTable, parserOptions)
  for i in 0 ..< fields.len - 1:
    if fields[i].val.isMagic or
       (fields[i].typ.kind == kStr and fields[i+1].val.valueExpr != nil):
      if fields[i+1].val.valueExpr == nil:
        raise newException(Defect,
          "Magic was used without assertion at the next field")
      fields[i].magic = fields[i+1]
  for f in fields:
    let field = f.val.name
    var impl = f.typ.getImpl
    if f.val.repeat != rNo:
      impl = quote do: seq[`impl`]
    if f.val.isMagic:
      impl = quote do: seq[`impl`]
    if field != "":
      tupleMeat.add(newIdentDefs(ident(field), impl))
    let
      sym = genSym(nskVar)
      read = generateRead(sym, f, bs, fieldsSymbolTable, paramsSymbolTable)
      write = generateWrite(f, bs, fieldsSymbolTable, paramsSymbolTable)
    reader.add(read)
    writer.add(write)
  let
    readerName = genSym(nskProc)
    writerName = genSym(nskProc)
  if tupleMeat.len == 0:
    let dummy = genSym(nskField)
    tupleMeat.add(newIdentDefs(dummy, ident"int"))
  result = quote do:
    proc `readerName`(`bs`: BitStream): `tupleMeat` =
      `reader`
    proc `writerName`(`bs`: BitStream, `input`: `tupleMeat`) =
      `writer`
    let `name` = (get: `readerName`, put: `writerName`)
  for p in params:
    result[0][3].add p.copyNimTree
    result[1][3].add p.copyNimTree

  when defined(BinaryLangEcho):
    echo repr result