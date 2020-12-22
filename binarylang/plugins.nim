template condGet*(field, parse, condition: untyped) =
  if condition:
    parse

template condPut*(field, encode, condition: untyped) =
  if condition:
    encode

template validGet*(field, parse, condition: untyped) =
  parse
  assert condition

template validPut*(field, encode, condition: untyped) =
  assert condition
  encode