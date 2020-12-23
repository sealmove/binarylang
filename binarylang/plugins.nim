# Primary (3 arguments)
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

template condPosGet*(field, parse, condAndPos: untyped) =
  let
    (cond, pos) = condAndPos
    save = getPosition(s)
  if cond:
    s.setPosition(pos)
    parse
    s.setPosition(save)
template condPosPut*(field, encode, condAndPos: untyped) =
  let
    (cond, pos) = condAndPos
    save = getPosition(s)
  if cond:
    s.setPosition(pos)
    encode
    s.setPosition(save)

# Secondary (2 arguments)
template validGet*(field, condition: untyped) =
  assert condition
template validPut*(field, condition: untyped) =
  assert condition