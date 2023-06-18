Examples
-------------------------------------------------------------------------------

For larger/real-world examples see my
`n4n6 repo <https://github.com/sealmove/n4n6>`_.

Running the tests
-------------------------------------------------------------------------------

.. code-block:: cmd
  cd tests
  testament pattern "*.nim"

Clean up files produced by testament:

.. code-block:: cmd
  find . -maxdepth 1 -type f ! -name '*.nim' -delete
  rm -r testresults

Releasing a new version
-------------------------------------------------------------------------------
1. Increment the version in your `.nimble` file
2. Add a new entry in `changelog.rst`
3. Commit your changes
4. Tag your release (`git tag <tagname>`)
5. Push the new tag and commits (`git push origin <tagname>`)