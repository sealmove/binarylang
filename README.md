# BinaryLang
BinaryLang is an extensible Nim DSL for creating binary parsers/encoders in a
symmetric fashion.

It supports syntax for creating simple common parsers (ints, floats, strings),
as well as ways for synthesizing more complex parsers out of simpler ones.
Therefore, it is similar to a parser combinator framework.

Two macros are exported:
- `createParser`: listed parsers are combined sequentially, mapping data to a `tuple`
- `createVariantParser`: listed parsers are partitioned into sets of choices, mapping data to an `object variant`

Moreover, you can mix DSL-generated parsers with custom parsers, as long as the
custom parsers follow the API. There are also ways to extend the DSL through templates. These works as
*plugins*.

## Examples
### Parsing Pascal strings
```nim
createParser(PascalString):
  u8: len
  s: str(len)
```

For larger/real-world examples see my [n4n6 repo](https://github.com/sealmove/n4n6).

To generate the documentation which explains the DSL, run `nim doc2 binarylang.nim`