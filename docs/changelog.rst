v0.5.1 (8 April 2021)
-------------------------------------------------------------------------------
- Bugfix: BinaryLang now correctly uses the alterned type specified by the last
  operation, in the struct's/union's type declaration.

v0.5.0 (8 April 2021)
-------------------------------------------------------------------------------
- API change: The plugin system is reworked as follows:
    - Call syntax is used instead of expression-bracket-expression, in order to
      support arbitrary number of arguments.
    - The API is homogenic regardless of whether the plugin interfaces with the
      stream or not.
    - Type conversion is supported by explicit annotation.
    - Due to the above, *properties* are rendered redundant, and therefore not
      a thing anymore (completely removed).

v0.4.0 (3 April 2021)
-------------------------------------------------------------------------------
- API change: ``createParser``/``createVariantParser`` were renamed to
  ``struct``/``union`` respectively
- API change: *complex* type is now divided into *product* and *sum* type for
  parsers created with the ``struct``/``union`` macro respectively. Sum parsers
  differ from product parsers in that the first argument is mandatory and is
  treated differently since it refers to the *discriminator*. Specifically, the
  passed argument is calculated only during parsing, whereas during
  serialization the value stored in the ``disc`` field is used.
- API change: The discriminator field of sum parsers is always called ``disc``
  implicitly. The second argument of the ``union`` macro must be a single
  identifier denoting the type of the discriminator -as opposed to an
  expression-colon-expression-.

v0.3.3 (2 April 2021)
-------------------------------------------------------------------------------
- Bugfix: Magic now works properly in ``createVariantParser``
- Converters are now also exported when exporting the corresponding parser

v0.3.2 (2 April 2021)
-------------------------------------------------------------------------------
- Bugfix: symbol table under branches of variant parsers was not updated after
  each field
- Bugfix: anonymous fields in variant parsers were not discarded

v0.3.1 (25 March 2021)
-------------------------------------------------------------------------------
- Implemented *converters* properly. They are now procs with the names:
  - ``from<parser>``
  - ``to<parser>``

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
