import macros

proc generateConverters*(tname, pname: NimNode; params: seq[NimNode];
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
      toConv,
      procToParams,
      procToBody),
    newProc(
      fromConv,
      procFromParams,
      newStmtList(
        (quote do:
          let s {.inject.} = newStringBitStream()),
        putCall,
        (quote do: s.seek(0)),
        (quote do: s.readAll))))