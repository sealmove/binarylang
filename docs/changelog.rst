v0.2.0 (22 March 2021)
-------------------------------------------------------------------------------
With many breaking changes, this marks the beginning of version control!

- Documentation is updated and refined to reflect the new API.
- `createParser` now declares an object type which is used by the
  parsing/encoding procs. This enables describing recursive parsers.
- The first argument of `createParser`/`createVariantParser` must now
  *mandatorily* be in lowercase and it has an extra meaning: it is used for
  deriving the name of the object type:
  `objname` ≡ `capitalizeAscii(parsername)`.
- `@hook` property is implemented
- `typeGetter` is marked as deprecated.
- Switched from unittest to testament.