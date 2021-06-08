discard """
  cmd: "nim c -r $file"
"""

import ../binarylang, ../binarylang/plugins

struct(parser, reference = y):
  u8: hasChild
  *parser {cond(hasChild.bool)}: child

block:
  var fbs = newFileBitStream("data/recursion.hex")
  defer: close(fbs)
  let data = parser.get(fbs)
  assert data.hasChild == 1
  assert data.child.hasChild == 1
  assert data.child.child.hasChild == 0
  
  # Serialization
  var sbs = newStringBitStream()
  defer: close(sbs)
  parser.put(sbs, data)
  sbs.seek(0)
  let reparsed = parser.get(sbs)
  assert data.hasChild == reparsed.hasChild
  assert data.child.hasChild == reparsed.child.hasChild
  assert data.child.child.hasChild == reparsed.child.child.hasChild
