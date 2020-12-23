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

template addGet*(field, parse, n: untyped) =
  parse
  field += n
template addPut*(field, encode, n: untyped) =
  field -= n
  encode

template subGet*(field, parse, n: untyped) =
  parse
  field -= n
template subPut*(field, encode, n: untyped) =
  field += n
  encode

template multGet*(field, parse, n: untyped) =
  parse
  field *= n
template multPut*(field, encode, n: untyped) =
  field = field div n
  encode

template divGet*(field, parse, n: untyped) =
  parse
  field = field div n
template divPut*(field, encode, n: untyped) =
  field *= n
  encode

# Secondary (2 arguments)
template validGet*(field, condition: untyped) =
  assert condition
template validPut*(field, condition: untyped) =
  assert condition