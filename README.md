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

### Function List

#### $(call explode,_stringlist_,_string_)
 Insert a blank after every occurrence of the strings from _stringlist_ in _string_.
 This function serves mainly to convert a string into a list.
 Example: `$(call explode,0 1 2 3 4 5 6 7 8 9,0x1337c0de)` --> `0 x1 3 3 7 c0 de`

#### $(call implode,_string-with-spaces_)
 Remove all spaces from the given string. Note that make is mostly unaware of escape
 characters and therefore takes all spaces verbatim.
 Example: `$(call implode,some\ awkward\ Windows\ path)` --> `some\awkward\Windows\path`

#### $(call str-eq,_string1_,_string2_)
 Compare two strings on equality. Strings are allowed to have blanks.
 Return non-empty if string $1 and $2 are identical, empty string otherwise.
 - `$(call str-eq,yes,no)` --> `` (empty string)
 - `$(call str-eq,yes ,yes)` --> `` (empty string)
 - `$(call str-eq,yes ,yes )`  --> `t`

#### $(call str-le,_string1_,_string2_)
 Compare two strings lexically for _string1_ less-or-equal _string2_.
 Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
 always compares smaller than any other string. The strings may contain spaces
 but leading and trailing spaces are not considered in the comparison and
 multiple interior spaces are substituted by a single one. Spaces compare
 smaller than downcase characters but greater than upcase. In short: you
 should know what you are doing if you have spaces inside your strings.
 Examples:
 - `$(call str-le,aaa,ab))` --> `t` 			     
 - `$(call str-le,   ab aa,aa))` --> ``	(empty string)	     
 - `$(call str-le,aa,aa))` --> `t`			     
 - `$(call str-le,aa,a))` --> ``	(empty string)		     
 - `$(call str-le,a,))` --> `` (empty string)			     
 - `$(call str-le,,a))` --> `t`			     
 - `$(call str-le,MacGyver John,Mac Gyver John))` --> `t`
 - `$(call str-le,macgyver john,mac gyver john))` --> `` (empty string)

#### $(call str-ge,_string1_,_string2_)
 Compare two strings lexically for _string1_ greater-or-equal _string2_.
 Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
 always compares smaller than any other string. The strings may contain spaces
 but leading and trailing spaces are not considered in the comparison and
 multiple interior spaces are substituted by a single one. Spaces compare
 smaller than downcase characters but greater than upcase. In short: you
 should know what you are doing if you have spaces inside your strings.
 Examples:
 - `$(call str-ge,aaa,ab))` --> `` (empty string) 			     
 - `$(call str-ge,   ab aa,aa))` --> `t`
 - `$(call str-ge,aa,aa))` --> `t`			     
 - `$(call str-ge,aa,a))` --> `t`
 - `$(call str-ge,a,))` --> `t`			     
 - `$(call str-ge,,a))` --> `` (empty string)			     
 - `$(call str-ge,MacGyver John,Mac Gyver John))` --> `` (empty string)
 - `$(call str-ge,macgyver john,mac gyver john))` --> `t`

#### $(call str-match,_string1_,_string2_)
 Compare two strings on equality under wildcard substitution. The wildcard
 can appear in both of the strings.
 Return t if _string1_ and _string2_ match where the first % is taken as
 wildcard, return empty string otherwise. 
 - `$(call str-match,Mickey%Mouse,Mickey Mouse))` --> `t`
 - `$(call str-match,Mickey%,MickeyMouse))` --> `t`
 - `$(call str-match,Mickey%,))` --> `` (empty string)
 - `$(call str-match,Mickey %ouse,Mickey Mouse))` --> `t`
 - `$(call str-match,MickeyMouse,MickeyMouse%))` --> `t`
 - `$(call str-match,,%))` --> `t`

#### $(call up-to,_word_,_list_)
 Return first part of _list_ up to but excluding the first occurrence of _word_.
 If _word_ is not in _list_, the whole list is returned.
 Examples:
 - `$(call up-to,baz,foo bar baz)` -> `foo bar`
 - `$(call up-to,foo,foo bar baz)` -> ` ` (empty list)

#### $(call index-of,_word_,_list_)
 Return the index of the first occurrence of _word_ if present or the empty list.
 *Indexing starts at 0*, contrary to the make-internal behaviour of numbering lists from 1!
 Note that you can always have an index starting at 1 by prefixing the list at
 call time with the $(-never-matching) element (or a simple underscore, if you are sure that 
 it is never a real element), i.e. `$(call index-of,foo,$(-never-matching) foo bar baz)` will give index 1.
 Examples:
 - `$(call index-of,foo,foo bar baz)` -> `0`

#### $(call from-on,_word_,_list_)
 Return the portion of _list_ following the first occurrence of _word_.
 If _word_ is not in _list_, the empty string/list is returned.
 Examples:
 - `$(call from-on,foo,foo bar baz)` -> `bar baz`
 - `$(call from-on,baz,foo bar baz)` -> ` ` (empty list)

#### $(call add,_num1_,_num2_)
 Calculate _num1+num2_. Both arguments are signed integers.
 Numeric bases can be mixed, the result is given in the same base as _num1_.
 Examples:
 - `$(call add,-1,0)` --> `-1`
 - `$(call add,0x12,13)` --> `0x1f`

#### $(call sub,_num1_,_num2_)
 Calculate _num1-num2_. Both arguments are signed integers.
 Numeric bases can be mixed, the result is given in the same base as _num1_.
 Examples:
 - `$(call sub,12,-10)` --> `22`
 - `$(call sub,012,-10)` --> `024`

#### $(call mul,_num1_,_num2_)
 Multiply integer _num1_ with integer _num2_. Both arguments can be signed.
 Numeric bases can be mixed, the result is given in the same base as _num1_.
 Examples:
 - `$(call mul,-1,0)` --> `0`
 - `$(call mul,-0x1,0)` --> `0x0`
 - `$(call mul,03,-7)` --> `-025`

#### $(call div,_num1_,_num2_)
 Divide integer _num1_ by integer _num2_. Both arguments can be signed.
 Numeric bases can be mixed, the result is given in the same base as _num1_.
 Examples:
 - `$(call div,9876543210,0x13)` --> `519818063`
 - `$(call div,0x9876543210,16)` --> `0x987654321`

#### $(call select,_col-nrs_,_table_,_where-clause_)
 Select all rows from a _table_ which fulfill the _where-clause_ and pick the subset
 of _col-nrs_ from these rows to form an output list. A gmtt **table** is
 a list with a leading decimal which denotes the number of columns in this 'table'.
 See the documentation for gmtt at [https://github.com/markpiffer/gmtt].
 The _where-clause_ is a function or a 'lambda' expression (i.e. function expression
 written into the parameter place directly) which receives the elements of each row of
 the table in order as parameters `$1`,`$2`,`$3` etc. **Note:** the function call/lambda
 needs to be $-quoted, that is, every '$' that appears as variable reference must be
 doubled '$$'. See the examples below. The clause shall return true (non-empty string)
 or false (empty string) to accept/reject each rows elements into/from the result
 of the select. The selection is limited to the column numbers given in _col-nrs_,
 in their respective order. Do not confuse the _col-nrs_ subset with the parameters 
 given to the 'where' function, the former is just a possibly reordered subset of the
 latter which is formed after the 'where' function accepted the record.
 (`select` mimics a SQL `SELECT model, price FROM cars WHERE color="red"` and 
 returns effectively a list of subsets from all positively selected rows. If you prepend
 the result list with its column count, you have a new gmtt table)
 Example:
 - `test-tbl := 4   foo bar baz 11    foo bar baf 22   faa bar baz 33`
 - `$(call select,3 1 2 3,$(test-tbl),$$(call str-match,$$1,%oo))` --> `baz foo bar baz baf foo bar baf`
 The same can be achieved, if we use a function as where clause:
 - `ends-in-oo = $(call str-match,$1,%oo)`
 - `$(call select,3 1 2 3,$(test-tbl),$$(call ends-in-oo,$$1))` --> `baz foo bar baz baf foo bar baf`
