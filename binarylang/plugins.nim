template condGet*(parse, parsed, output, cond: untyped) =
  if cond:
    parse
    output = parsed
template condPut*(encode, encoded, output, cond: untyped) =
  if cond:
    output = encoded
    encode

template validGet*(parse, parsed, output, cond: untyped) =
  parse
  assert cond
  output = parsed
template validPut*(encode, encoded, output, cond: untyped) =
  output = encoded
  assert cond
  encode

template posGet*(parse, parsed, output, pos: untyped) =
  let save = getPosition(s)
  s.setPosition(pos)
  parse
  s.setPosition(save)
  output = parsed
template posPut*(encode, encoded, output, pos: untyped) =
  output = encoded
  let save = getPosition(s)
  s.setPosition(pos)
  encode
  s.setPosition(save)

template condPosGet*(parse, parsed, output, condAndPos: untyped) =
  let
    (cond, pos) = condAndPos
    save = getPosition(s)
  if cond:
    s.setPosition(pos)
    parse
    s.setPosition(save)
    output = parsed
template condPosPut*(encode, encoded, output, condAndPos: untyped) =
  output = encoded
  let
    (cond, pos) = condAndPos
    save = getPosition(s)
  if cond:
    s.setPosition(pos)
    encode
    s.setPosition(save)

## Required imports: sequtils
template toStrGet*(parse, parsed, output: untyped) =
  parse
  output = parsed.mapIt(it.char).join
template toStrPut*(encode, encoded, output: untyped) =
  output = newSeq[byte](encoded.len)
  for i in 0 ..< encoded.len:
    output[i] = byte(encoded[i])
  encode