# gmtt
The GNU make table toolkit

## Overview
gmtt is a collection of GNU make functions which among other things implement sorting and searching in tabular data. The intention is to manage build configuration data (like memory ranges, interrupt vectors in embedded system bootloaders and so on) in a way that doesn't force you out to the shell. This has the additional benfit that you may get away with easier portability between Unix and Windows.

### Tables
make has only one complex data type: list. make lists are just sequences of non-empty strings separated by whitespace characters (space, tab, linefeed). Thus tables are implemented as make lists with a first element denoting the table width (number of colons) as a decimal number: `3 tic tac toe` is a table, while `alpha beta gamma` is not. Of course tables can (and usually will) be written as multi-line defines:
```
define test-tbl =
3
x1|y1  x2|y1  x3|y1
x1|y2  x2|y2  x3|y2
x1|y3  x2|y3  x3|y3
endef
```
There are some obvious restrictions on tables: make doesn't recognize string delimiters ("" etc.) so there is no way to have spaces inside one cell:
```
define illegal-table-there-are-no-strings-in-make = 
2
"This table has 4 rows really!"  x2|y1  
endef
```

As there is nothing like an empty string as member of a make list, we are also prohibited of using empty cells in a table. You will have to supply a string that denotes "nothing" and that will not get in your way further down the processing line. Here is an example where it is assumed that C-style comments don't harm 
```
define table-with-empty-cells =
3
x1|y1      x2|y1       x3|y1
x1|y2    /*void*/      x3|y2
/*void*/   x2|y3       x3|y3
endef
```

#### Functions on tables
- sort / reverse sort
- select (with a 'where' clause, remotely comparable to SQL)
- map (apply a function on each row)


### Arithmetic
All operations are supported for decimal, hexadecimal and octal numbers. The arithemtic range is currently ~64 places (don't exploit the maximum) but this limit can be set rather freely. Numbers are not fixed length (there is only an encapsulated internal representation which the user will never see) so they will be fast if small and slow if long. The bases of all operands can be mixed freely but of course the previously mentioned restrictions apply. The output base is decided by the base of the first operand: `$(call add,0x1,1)` will produce `0x2` but `$(call add,1,0x1)` will give `2`. Generally the module relies on well-formed numbers:

* 0x1234 (a hexadecimal)
* -000123 (a negative octal)
* 123 (a decimal)

are all processed correctly, while 

* 0x1234- (will be taken as negative)
* 12 34 (will produce garbage)

are not.

#### Functions on numbers
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
- round up to a power of 2
- round down to a power of 2

### List functions

#### $(call up-to,_list_,_word_)
 Return first part of _list_ up to but excluding the first occurrence of _word_.
 If _word_ is not in _list_, the whole list is returned.
 Examples:
 - `$(call up-to,foo bar baz,baz)` -> `foo bar`
 - `$(call up-to,foo bar baz,foo)` -> ` ` (empty list)


#### $(call index-of,_list_,_word_)
 Return the index of the first occurrence of _word_ if present or the empty list.
 *Indexing starts at 0*, contrary to the make-internal behaviour of numbering lists from 1!
 Note that you can always have an index starting at 1 by prefixing the list at
 call time with the $(\_never-matching) element (or a simple underscore, if you are sure
 that it is never a real element), i.e. `$(call index-of,
 $(_never-matching) foo bar baz,foo)` will give index 1.
 Examples:
 - `$(call index-of,foo bar baz,foo)` -> `0`

#### $(call from-on,_list_,_word_)
 Return the portion of _list_ following the first occurrence of _word_.
 If _word_ is not in _list_, the empty string/list is returned.
 Examples:
 - `$(call from-on,foo bar baz,foo)` -> `bar baz`
 - `$(call from-on,foo bar baz,baz)` -> ` ` (empty list)


#### $(call sort-all,_list_)
Sort the list without dropping duplicates like the internal `$(sort)`.

### Miscellaneous functions
- string compares
- explode/implode: take a string apart

#### $(call explode,_stringlist_,_string_)
 Insert a blank after every occurrence of the strings from _stringlist_ in _string_.
 This function serves mainly to convert a string into a list.
 Examples: 
 - `$(call explode,0 1 2 3 4 5 6 7 8 9,0x1337c0de)` -> `0 x1 3 3 7 c0 de`


