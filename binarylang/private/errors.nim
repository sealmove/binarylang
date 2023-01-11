type
  MagicError* = object of Defect
  SyntaxError* = object of Defect

proc syntaxError*() = raise newException(SyntaxError, "Syntax error")
proc syntaxError*(message: string) = raise newException(SyntaxError, message)