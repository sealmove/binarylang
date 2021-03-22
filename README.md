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
custom parsers follow the API. There are also ways to extend the DSL through
templates (these work as *plugins*) and ways configure the way you interact
with your data.

## Documentation
- [User guide](https://sealmove.github.io/binarylang/)
- [Tutorial](https://sealmove.github.io/binarylang/tutorial.html)
- [Changelog](https://sealmove.github.io/binarylang/changelog.html)