# BinaryLang
BinaryLang is an extensible Nim DSL for creating binary parsers/encoders in a
symmetric fashion.

It supports syntax for creating simple common parsers (ints, floats, strings),
as well as ways for synthesizing more complex parsers out of simpler ones.
Therefore, it is similar to a parser combinator framework.

Moreover, you can mix DSL-generated parsers with custom ones and extend the DSL
using nim templates.

## Documentation
- [Manual](https://sealmove.github.io/binarylang/)
- [Changelog](https://sealmove.github.io/binarylang/changelog.html)
- Tutorials, write-ups, articles
  - [Ajusa on HTTP](https://ajusa.github.io/binarylang-fun/intro.html)
  - [Ajusa on Web Scraping](https://ajusa.github.io/binarylang-fun/scraping.html)