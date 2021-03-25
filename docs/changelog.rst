v0.2.0 (22 March 2021)
-------------------------------------------------------------------------------
With many breaking changes, this marks the beginning of version control!

- Documentation is updated and refined to reflect the new API.
- ``createParser`` now declares an object type which is used by the
  parsing/encoding procs. This enables describing recursive parsers.
- The first argument of ``createParser``/``createVariantParser`` must now
  *mandatorily* be in lowercase and it has an extra meaning: it is used for
  deriving the name of the object type:
  ``objname`` ≡ ``capitalizeAscii(parsername)``.
- ``@hook`` property is implemented
- ``typeGetter`` is marked as deprecated.
- Switched from unittest to testament.

v0.3.0 (25 March 2021)
-------------------------------------------------------------------------------
- Bugfixes for ``@put``/``@hook`` properties.
- The underlying field when using properties can now be accessed with the
  identifier ``<field>Impl``.
- Exportation of fields using ``*`` now also works when using properties.
- Parser and parser's type symbols can now be exported by prepending ``*`` to
  the parser's name.
- ``createParser`` and ``createVariantParser`` now also generate converters
  from/to ``string``.

v0.3.1 (25 March 2021)
-------------------------------------------------------------------------------
- Implemented *converters* property. They are now procs with the names:
  - from<parser>
  - to<parser>