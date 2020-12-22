template condGet*(field, parse, condition: untyped) =
  if condition:
    parse

template condPut*(field, encode, condition: untyped) =
  if condition:
    encode