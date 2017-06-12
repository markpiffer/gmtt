# gmtt
The GNU make table toolkit

## Overview
gmtt is a collection of GNU make functions which among other things implement sorting and searching in tabular data. The intention is to manage build configuration data (like memory ranges, interrupt vectors in embedded system bootloaders and so on) in a way that doesn't force you out to the shell. This has the additional benfit that you may get away with easier portability between Unix and Windows.

### Tables
- sort / reverse sort
- select (with a 'where' clause, remotely comparable to SQL)
- map (apply a function on each row)

Tables are just make lists (i.e. strings separated by whitespace) with a first element which denotes the table width (number of colons) by a decimal number: `3 tic tac toe` is a table, while `alpha beta gamma` is not. Of course tables can (and usually will) be written as multi-line defines:
```
define test-tbl =
3
x1|y2  x2|y2  x3|y2
x1|y1  x2|y1  x3|y1
x1|y3  x2|y3  x3|y3
endef
```

### Arithmetic
- add
- subtract
- multiply
- divide
- modulus
- log2
- bitwise or
- bitwise and
- bitwise xor
- bitwise not
- round up to a power of 2 (correct results only for powers of 2 as second operand)
- round down to a power of 2 (correct results only for powers of 2 as second operand)

All operations are supported for decimal, hexadecimal and octal numbers. The arithemtic range is currently ~64 places (don't exploit the maximum) but this limit can be set rather freely. Numbers are not fixed length (there is only an encapsulated internal representation which the user will never see) so they will be fast if small and slow if long. The bases of all operands can be mixed freely but of course the previously mentioned restrictions apply. The output base is decided by the base of the first operand: `$(call add,0x1,1)` will produce `0x2` but `$(call add,1,0x1)` will give `2`. Generally the module relies on well-formed numbers:

* 0x1234 (a hexadecimal)
* -000123 (a negative octal)
* 123 (a decimal)

are all processed correctly, while 

* 0x1234- (will be taken as negative)
* 12 34 (will produce garbage)

are not.

### Miscellaneous functions
- sort with duplicates
- string compares
- explode/implode: take a string apart
