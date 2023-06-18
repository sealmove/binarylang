Examples
-------------------------------------------------------------------------------

.. code:: nim
   let x = 5
   struct(pascalString):
     u8: len
     s {@hook: (len = _.len)}: str(len)

For larger/real-world examples see my `n4n6 repo <https://github.com/sealmove/n4n6>`_.