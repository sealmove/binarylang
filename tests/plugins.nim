discard """
  cmd: "nim c -r $file"
"""

import ../binarylang

struct(data, plugins = {converters}):
  8: x

block:
  var fileContent = readFile("data/plugins.hex")
  let data = fileContent.toData
  assert data.x == 0x41

  # Serialization
  let reparsed = data.fromData
  assert reparsed == "A"