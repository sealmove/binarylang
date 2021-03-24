# BinaryLang
BinaryLang is an extensible Nim DSL for creating binary parsers/encoders in a
symmetric fashion.

It supports syntax for creating simple common parsers (ints, floats, strings),
as well as ways for synthesizing more complex parsers out of simpler ones.
Therefore, it is similar to a parser combinator framework.

Moreover, you can mix DSL-generated parsers with custom parsers, extend the DSL using
templates, and configure the way you interact with your data.

## Documentation
- [Manual](https://sealmove.github.io/binarylang/)
- [Tutorial](https://sealmove.github.io/binarylang/tutorial.html)
- [Changelog](https://sealmove.github.io/binarylang/changelog.html)
- Tutorials, write-ups, articles
  - [Asuja on HTML](https://ajusa.github.io/binarylang-fun/intro.html)