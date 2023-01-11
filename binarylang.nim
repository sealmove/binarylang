## DSL invocation
## ----------------------------------------------------------------------------
## Two macros are exported:
## - `struct` which is used to produce a *product parser*
## - `union` which is used to produce a *sum parser*
##
## Both of these macros generate a type declaration and a
## `tuple[get: proc, put: proc]`:
## - `get` returns an object with each parsed field
## - `put` writes an object to a stream
## Each statement corresponds to 1 field. The general syntax is:
##
## .. code::
##    type: name (...)
##
## For the name you use `_` to discard the field, or prepend it with `*` to
## export it.
##
## Parser options
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Each specified option must be in the form `option = value`:
## - `endian`: sets the default byte endianness for the whole parser
##    - *default*: big endian
##    - `b`: **big** endian
##    - `l`: **little** endian
##    - `c`: **cpu** endian
## - `bitEndian`: sets the default bit endianness for the whole parser
##    - *default*: left -> right
##    - `n`: left -> right (**normal**)
##    - `r`: left <- right (**reverse**)
## - `reference`: configures whether the associated type will be a `ref` or not
##    - *default*: no
##    - `y`: yes
##    - `n`: no
## - `plugins`: enable additional codegen features (value is a set)
##    - `converters`: generate *from* and *to* procs for converting from/to
##      `string`
##
## .. code-block:: nim
##   struct(data, plugins = {converters}):
##     8: x
##
##   var fileContent = readFile("data/plugins.hex")
##   let data = fileContent.toData
##   assert data.x == 0x41
##
##   let reparsed = data.fromData
##   assert reparsed == "A"
##
## Parser parameters
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Each parameter must be in the form `symbol: type`. The generated `get`/`put`
## procs will then have this additional parameter appended.
##
## The only exception is the discriminator field for **sum** parsers which is
## always named ``disc`` implicitly; and therefore, only the type must be
## provided -instead of an expression-colon-expression-.
##
## Types
## ----------------------------------------------------------------------------
##
## Primitive types
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## The **kind**, **endianness** and **size** are encoded in a identifier
## made up of:
##
## - 1 optional letter specifying the kind:
##    - *default*: signed integer
##    - `u`: unsigned integer
##    - `f`: float
##    - `s`: string
## - 1 optional letter specifying byte endianness:
##    - *default*: big endian
##    - `b`: big endian
##    - `l`: little endian
## - 1 optional letter specifying bit endianness:
##    - *default*: left -> right
##    - `n`: left -> right (normal)
##    - `r`: left <- right (reverse)
## - 1 number specifying size in **bits**:
##    - for a string it refers to the size of each individual character and
##      defaults to `8`
##    - for an integer the allowed values are `1 .. 64`
##    - for a float the allowed values are `32` and `64`
##
## You can order options however you want, but size must come last (e.g.
## `lru16` and `url16` are valid but not `16lru`).
##
## Assertion can also be used in a special manner to terminate the previous
## field if it's a **string** or a **sequence indicated as magic-terminated**.
## This is discussed in later sections.
##
## Product type
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## A parser is of type **product** if it is created with the ``struct`` macro
## or *by hand*, as explained in a later section. To call a product parser you
## must use `*` followed by the name of the parser. If your parser requires
## arguments, you must provide them using standard call syntax.
##
## Example:
##
## .. code:: nim
##    struct(inner):
##      32: a
##      32: b
##
##    struct(innerWithArgs, size: int32):
##      32: a
##      32: b[size]
##
##    struct(outer):
##      *inner: x
##      *innerWithArgs(x.a): y
##
## Sum type
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## A parser is of type **sum** if it is created with the ``union`` macro or
## *by hand*, as explained in a later section. A sum parser has a special
## field called the *discriminator* which determines which branch will be
## activated at run-time -similarly to *object variants*-.
##
## To call a sum parser you must use `+` followed by a call-syntaxed expression.
## The callee is the name of the parser and the first argument is the value of
## the *discriminator* field. If the parser requires additional arguments, they
## also have to be provided. The first argument is treated in a special manner.
## Unlike other arguments, this one is only evaluated during parsing, whereas
## during serialization the value stored in the ``disc`` field is used.
##
## Example:
##
## .. code:: nim
##    union(inner, byte):
##      (0): 8: a
##      (1): 16: b
##      _: nil
##
##    struct(outer):
##      +inner(0): x
##
## Features
## ----------------------------------------------------------------------------
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
##    struct(parser, bitEndian = n):
##      b9: a # error: cannot apply byte endianness
##      r6: b # error: shares bits with previous byte
##      10: c # error: spec does not finish on a byte boundary
##
## Moreover, unaligned reads for strings are not supported:
##
## .. code:: nim
##    struct(parser):
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
##    struct(inner, size: int):
##      8: x[size]
##    struct(outer):
##      32: amount
##      32: sizes[amount]
##      *inner(sizes[i]): aux[amount]
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
##    struct(aux, size: int):
##      8: x[size]
##    struct(parser):
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
## instead you should use the technique with the auxiliary parser shown above:
##
## .. code:: nim
##    struct(parser):
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
## Extensions
## ----------------------------------------------------------------------------
##
## Custom parser API
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Since a BinaryLang parser is just a `tuple[get: proc, put: proc]`, you can
## write parsers by hand that are compatible with the DSL. Just be sure that
## `get` and `put` have proper signatures, and there is a type with the same
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
##    type {op(arg)}: name
##
## Operations act on data after the parsing and before the encoding
## respectively.
##
## An operation is nothing more than a pair of templates which follow a
## specific pattern:
## - The names of the templates **must** follow the pattern: `<operation>get`
##   and `<operation>put`
## - They must have at least 3 untyped parameters (you can name them as you
##   wish):
##    - **parameter #1**: parsing/encoding statements
##    - **parameter #2**: variable *previously* parsed/encoded
##    - **parameter #3**: output
## 
## .. code:: nim
##    template increaseGet(parse, parsed, output, num: untyped) =
##      parse
##      output = parsed + num
##    template increasePut(encode, encoded, output, num: untyped) =
##      output = encoded - num
##      encode
##    struct(myParser):
##      64: x
##      16 {increase(x)}: y
##
## You can apply more than one operations on one field, in which case they
## are chained in the specified order, and only the first operation really
## does any parsing/encoding to the stream. The rest just operate on the
## value produced by the operation directly before them.
##
## `parse` fills in the `parsed` variable. It is a seperate statement because
## it potentially operates on the stream (this happens **always and only for
## the first operation**). Similarly, `encode` passes on the value in
## `output` variable. *Passes* means the value is potentially written to the
## stream.
##
## .. code:: nim
##    template condGet(parse, parsed, output, cond: untyped) =
##      if cond:
##        parse
##        output = parsed
##    template condPut(encode, encoded, output, cond: untyped) =
##      if cond:
##        output = encoded
##        encode
##    template increaseGet(parse, parsed, output, num: untyped) =
##      parse
##      output = parsed + num
##    template increasePut(encode, encoded, output, num: untyped) =
##      output = encoded - num
##      encode
##    struct(myParser):
##      8: shouldParse
##      64: x
##      16 {cond(shouldParse.bool), increase(x)}: y
##
## It is impossible for BinaryLang to infer the type of the altered value,
## that is, if your operation changes it. By default it is assumed that
## the new field value is of the same type as the *previous* one (for the
## first operation, this is the type produced according to the field type
## annotation). Therefore, if your operation alters the type, then you must
## provide the new type in square brackets:
##
## .. code:: nim
##    template asciiNumGet(parse, parsed, output: untyped) =
##      parse
##      output = char(parsed - '0')
##    template asciiNumPut(encode, encoded, output: untyped) =
##      output = int8(encoded + '0')
##      encode
##    struct(myParser):
##      8 {asciiNum[char]}: x
##
## The actual type of the field changes to the type annotated in the last
## operation. if you annotate the type for *some* of the operations, then for
## the ones you did not, the type of the operation directly previous to it is
## assumed.
##
## Special notes
## ----------------------------------------------------------------------------
## - Nim expressions may contain:
##    - a previously defined field
##    - a parser parameter
##    - the `_` symbol for *subject* element (its meaning varies)
##    - the `i` symbol for current index in a repetition
##    - the `s` symbol for accessing the bitstream
##
## `i` and `s` might conflict with your variables or fields, so you should
## consider them reserved keywords and not use them for something else.

import binarylang/private/[types, errors, dsldecoders]
import binarylang/private/codegen/[serialization, deserialization, conversion]
import macros, tables, strutils, sugar
import bitstreams
export bitstreams, MagicError

macro struct*(name: untyped, rest: varargs[untyped]): untyped =
  ## Input:
  ## - `name`: Name of the parser tuple to create (must be lowercase)
  ## - `rest`: **Optional** parser options and parameters
  ## - `rest` (last): Block of the format described above
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
    var impl: NimNode
    if f.ops.len > 0:
      impl = f.ops[^1].typ.copyNimTree
    else:
      impl = f.typ.getImpl
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
  let typeBody = nnkObjectTy.newTree(
    newEmptyNode(),
    newEmptyNode(),
    fieldDefs)
  result.add(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        tdef,
        newEmptyNode(),
        if parserOptions.reference: nnkRefTy.newTree(typeBody)
        else: typeBody)))
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
  result.add(quote do:
    `readerProcForwardDecl`
    `writerProcForwardDecl`
    let `pdef` = (get: `readerName`, put: `writerName`)
    `readerProc`
    `writerProc`)
  if ppConverters in parserOptions.plugins:
    let (procTo, procFrom) = generateConverters(tname, pname, params, isExported)
    result.add(quote do:
      `procTo`
      `procFrom`)

  when defined(BinaryLangEcho):
    echo repr result

macro union*(name, disc: untyped; rest: varargs[untyped]):
 untyped =
  ## Input:
  ## - `name`: The name of the parser tuple to create (must be lowercase)
  ## - `disc`: The definition of the discriminator field (`name: type`)
  ## - `rest`: **Optional** parser options and parameters
  ## - `rest` (last): Block of the format described above
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
  ## The body is similar to that of `struct` macro, but the fields are
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
  ##   union(fooBar, int):
  ##     (0): *foo: a
  ##     (1, 3): u32: *b
  ##     (2): nil
  ##     (4):
  ##       u8: c
  ##       *bar: d
  ##     _: u32: e
  result = newStmtList()

  name.expectKind({nnkIdent, nnkPrefix})
  disc.expectKind({nnkIdent, nnkPrefix})
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
    (extraParams, parserOptions) = decodeHeader(rest[0 .. ^2])
    discName = ident"disc"
  var
    discType: NimNode
    objectMeat = newTree(nnkRecCase)
  case disc.kind
  of nnkIdent:
    discType = disc.copyNimTree
    objectMeat.add(
      newIdentDefs(
        discName,
        discType))
  of nnkPrefix:
    if not eqIdent(disc[0], "*"):
      syntaxError("Invalid prefix for discriminator. Only '*' is allowed.")
    discType = disc[1].copyNimTree
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
            var impl = newTree(nnkNone)
            if f.ops.len > 0:
              for i in countdown(f.ops.len-1, 0):
                if f.ops[^1].typ.kind != nnkNone:
                  impl = f.ops[^1].typ.copyNimTree
                  break
              if impl.kind == nnkNone:
                impl = f.typ.getImpl
            else:
              impl = f.typ.getImpl
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
  let typeBody = nnkObjectTy.newTree(
    newEmptyNode(),
    newEmptyNode(),
    nnkRecList.newTree(
      objectMeat))
  result.add(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        tdef,
        newEmptyNode(),
        if parserOptions.reference: nnkRefTy.newTree(typeBody)
        else: typeBody)))
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
  result.add(quote do:
    `readerProcForwardDecl`
    `writerProcForwardDecl`
    let `pdef` = (get: `readerName`, put: `writerName`)
    `readerProc`
    `writerProc`)
  if ppConverters in parserOptions.plugins:
    let (procTo, procFrom) = generateConverters(tname, pname, params, isExported)
    result.add(quote do:
      `procTo`
      `procFrom`)
  when defined(BinaryLangEcho):
    echo repr result
