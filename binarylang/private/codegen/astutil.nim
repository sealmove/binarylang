import macros

proc prefixFields*(node: var NimNode, st, params: seq[string];
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

proc replaceWith*(node: var NimNode; what, with: NimNode) {.compileTime.} =
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