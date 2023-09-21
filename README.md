# gmtt
The GNU make table toolkit

## Overview
gmtt is a collection of GNU make functions which among other things implement sorting and searching in tabular data. The intention is to manage build configuration data (like platform dependencies, compilation options, memory ranges, interrupt vectors in embedded system bootloaders and so on) in a way that doesn't force you out to the shell and reaps the benefits of simple relational database functionality. This has the additional benfit that you may get away with easier portability between Unix and Windows.

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
You can alternatively use the `spc-mask` function (and its inverse `spc-unmask`) to concatenate strings with spaces into one table entry. But as always in *make*, using spaces in the processing of arguments requires much care and endurance to get it right.

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

## Function List

### String functions

#### $(call explode,_stringlist_,_string_)
 Insert a blank after every occurrence of the strings from _stringlist_ in _string_.
 This function serves mainly to convert a string into a list.
 - `$(call explode,0 1 2 3 4 5 6 7 8 9,0x1337c0de)` --> `0 x1 3 3 7 c0 de`

#### $(call implode,_string-with-spaces_)
 Remove all spaces from the given string. Note that make is mostly unaware of escape
 characters and therefore takes all spaces verbatim.
 - `$(call implode,some\ awkward\ Windows\ path)` --> `some\awkward\Windows\path`

#### $(call n-list,_string_,_number-of-repetitions_)
 Create a list with exactly _number-of-repetitions_ copies of a _string_.
 - `$(call n-list,foo,3)` --> `foo foo foo`

#### $(call bincnt,_binary-literal_)
 Count the _binary-literal_ up by 1, yielding the following binary literal.
 Leading zeros are preserved.
 - `$(call bincnt,010011)` -> `010100`

#### $(call symgen)
 Generate a different string at each call. The last generated symbol is
 accessible via `$(last-symgen)`.
 - `$(call symgen)` --> `sym0`

#### $(call interval,_start_,_range_[,_step_])
 Create a list of integers starting at _start_ and having _range_ elements
 with an increase (or decrease) of _step_ from one to the next. _step_ is optional
 and defaults to 1 if not given.

 Example:
 - `$(call interval,5,5)` --> `5 6 7 8 9`
 - `$(call interval,2,3,100)` --> `2 102 202`

#### $(call lpad,_string_,_final-width_,_padding-character_)
 Left-pad an alphanumeric string with the given character up to the given length.
 If the original string is longer than _final-width_, nothing is padded.
 Expample:
 - `$(call lpad,123,7,0)` --> `0000123`
 - `$(call lpad,123,7,-)` --> `----123`

#### $(call lstrip,_string_,_prefix_)
 Remove a _prefix_ from the given _string_. If the prefix doesn't exist, the
 string is unchanged.
 - `$(call lstrip,0xABCD,0x)` --> `ABCD`

#### $(call str-eq,_string1_,_string2_)
 Compare two strings on equality. Strings are allowed to have blanks.
 Return non-empty if string $1 and $2 are identical, empty string otherwise.
 - `$(call str-eq,yes,no)` --> ` ` (empty string)
 - `$(call str-eq,yes ,yes)` --> ` ` (empty string)
 - `$(call str-eq,yes ,yes )`  --> `t`

#### $(call str-ne,_string1_,_string2_)
 Compare two strings on inequality. The obvious inverse to
 `str-eq`. Strings are allowed to have blanks.  Return empty if
 string $1 and $2 are identical, a non-empty string otherwise.
 - `$(call str-eq,yes,no)` --> `t` 
 - `$(call str-eq,yes ,yes)` --> `t`
 - `$(call str-eq,yes ,yes )`  --> ` ` (empty string)

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
 - `$(call str-le,   ab aa,aa))` --> ` `	(empty string)	     
 - `$(call str-le,aa,aa))` --> `t`			     
 - `$(call str-le,aa,a))` --> ` `	(empty string)		     
 - `$(call str-le,a,))` --> ` ` (empty string)			     
 - `$(call str-le,,a))` --> `t`			     
 - `$(call str-le,MacGyver John,Mac Gyver John))` --> `t`
 - `$(call str-le,macgyver john,mac gyver john))` --> ` ` (empty string)

#### $(call str-ge,_string1_,_string2_)
 Compare two strings lexically for _string1_ greater-or-equal _string2_.
 Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
 always compares smaller than any other string. The strings may contain spaces
 but leading and trailing spaces are not considered in the comparison and
 multiple interior spaces are substituted by a single one. Spaces compare
 smaller than downcase characters but greater than upcase. In short: you
 should know what you are doing if you have spaces inside your strings.
 Examples:
 - `$(call str-ge,aaa,ab))` --> ` ` (empty string) 			     
 - `$(call str-ge,   ab aa,aa))` --> `t`
 - `$(call str-ge,aa,aa))` --> `t`			     
 - `$(call str-ge,aa,a))` --> `t`
 - `$(call str-ge,a,))` --> `t`			     
 - `$(call str-ge,,a))` --> ` ` (empty string)			     
 - `$(call str-ge,MacGyver John,Mac Gyver John))` --> ` ` (empty string)
 - `$(call str-ge,macgyver john,mac gyver john))` --> `t`

#### $(call str-match,_string1_,_string2_)
 Compare two strings on equality under wildcard substitution. The wildcard
 can appear in both of the strings.
 Return t if _string1_ and _string2_ match where the first % is taken as
 wildcard, return empty string otherwise. 
 - `$(call str-match,Mickey%Mouse,Mickey Mouse))` --> `t`
 - `$(call str-match,Mickey%,MickeyMouse))` --> `t`
 - `$(call str-match,Mickey%,))` --> ` ` (empty string)
 - `$(call str-match,Mickey %ouse,Mickey Mouse))` --> `t`
 - `$(call str-match,MickeyMouse,MickeyMouse%))` --> `t`
 - `$(call str-match,,%))` --> `t`

#### $(call glob-match,_string_,_pattern_)
 Try to match the _string_ with the _pattern_, applying glob-syntax.
 Glob-syntax is well known from the shell and
 https://en.wikipedia.org/wiki/Glob_(programming)
 All characters match themselves except:
 -  `*` - zero or more arbitrary chars
 -  `?` - exactly one arbitrary char
 -  `[]` - exactly one character from the set designated inside the brackets:
    - `[abc]` - explicit, matches one of `a`, `b` or `c`
    - `[a-z]` - range, matches one of `a`,`b`...`z`. The
                possibly ranges can be taken from `$(all-chars)`
    - `[]abc]` - first position is the only way to match a `]`
    - `[-abc]` - first or last position is the only way to match a `-`
    - `[!a-z]` - `!` inverts the match, i.e. everything but `a`..`z`
    
 If no match occurred, the  ` ` (empty string) is returned.
 If the string matches, it is returned with all parts corresponding to one of the above
 wildcards separated by space.
 As GNUmake treats everything which is different from the empty string
 as true, this function serves the simple matching test as well as a string
 dissection by wildcard patterns.
 Spaces inside your string are automatically preserved. The elements of the string
 are returned but with all original spaces replaced by an internal character to
 circumvent the rule that GNUmake treats spaces as list element separators.
 Use `spc-unmask` to remove these and restore the spaces. This caveat is necessary
 to allow access to the elements with a constant index, in case you are dissecting
 a string.
 Examples:
 - `$(call glob-match,Linux 2.6.32-431.el6.i686,Linux 2.6.*.i686)` --> `Linux§2.6. 32-431.el6 .i686`
 - `$(call glob-match,down/to/unknown/dir/file.txt,down/*/*/*/*.txt)` --> `down/ to / unknown / dir / file .txt`

#### $(call chop-str,_string_,_group-string_[,_group-string_[,_group-string_[,_group-string_]]])
 Dissect given _string_ into chunks of characters belonging to the
 same _group-string_ and return the result as a separator list. A
 _group-string_ is a tuple of an _identifier_ string and a
 _group-characters_ string with a space in between (e.g.`digit
 0123456789`). Each character in the given _string_ is tested for
 membership in one of the _group-characters_. If adjacent characters
 belong to the same _group-string_ they stay glued together
 otherwise a space is inserted.  The _identifier_ is attached as a
 prefix to the character chunks so that subsequent functions can
 distinguish the groups in the result. If a character in _string_ is
 not found in any of the group strings, it is dropped but still
 separates chunks. This function mainly serves to cut formated
 alphanumeric strings apart.
 *How to handle strings with spaces*:
 if you don't need the spaces after parsing (spaces only separate
 the string parts) then you don't have to do anything, simply call
 the function with the string as parameter.  If you want to preserve
 spaces, then you must call `chop-str-spc` which will leave behind
 the same list of string parts _but with spaces still replaced by an
 internal character_. The space character is handled as if it is
 part of the first _group-string_ so all sequences of characters
 from the first group which are separated by space will become one
 chunk.  This way you can still handle the function result as a list
 after calling.  When stepping through this list you can remove the
 internal character by simply applying `$(call
 spc-unmask,_string-chunk-from-result_)`.
 Examples:
 - `$(call chop-str,Linux 4.13.0-17-generic,A $(-alpha-as-str),1 $(-digit-as-str),. .-+?)` --> ` A¤Linux 1¤4 .¤. 1¤13 .¤. 1¤0 .¤- 1¤17 .¤- A¤generic`
 - `$(call chop-str,Thu Nov 30 18:43:22 CET 2017,alpha $(-alpha-as-str),num $(-digit-as-str),sep :)` --> ` alpha¤Thu alpha¤Nov num¤30 num¤18 sep¤: num¤43 sep¤: num¤22 alpha¤CET num¤2017`
 - `$(call chop-str-spc,Thu Nov 30 18:43:22 CET 2017,alpha $(-alpha-as-str),num $(-digit-as-str),sep :)` --> ` alpha¤Thu§Nov§ num¤30 alpha¤§ num¤18 sep¤: num¤43 sep¤: num¤22 alpha¤§CET§ num¤2017`

#### Predefined character classes for chop-str et al
 - `$(-upper-as-str) := ABCDEFGHIJKLMNOPQRSTUVWXYZ`
 - `$(-lower-as-str) := abcdefghijklmnopqrstuvwxyz`
 - `$(-digit-as-str) := 0123456789`
 - `$(-xdigit-as-str) := 0123456789abcdefABCDEF`
 - `$(-alpha-as-str) := ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`
 - `$(-alnum-as-str) := 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`

#### $(call spc-mask,_string_)
 Replace all occurrences of space in the given string with the
 `$(-spacereplace)` character. This function can be used to convert
 strings with spaces into valid list elements (which must not contain
 spaces). 

#### $(call spc-unmask,_string_)
 Replace all occurrences of the character `$(-spacereplace)` with a
 true space character. The inverse of the `spc-mask` function.

#### $(call drop-prfx,_separator-list_)
 Clear the prefixes from a separator list. A separator list
 is a make list with a prefix name prepended to each list
 element and the internal character `$(-separator)` between them
 e.g. `alpha¤Linux  num¤4  dot¤.  num¤2`. 


#### $(call drop-sufx,_separator-list_)
 Clear the suffixes (usually the data) from a separator list.
 What remains is a list of prefixes only . A separator list
 is a make list with a prefix name prepended to each list
 element and the internal character `$(-separator)` between them
 e.g. `alpha¤Linux  num¤4  dot¤.  num¤2`. 


#### $(call get-prfx-val,_separator-list_,_prefix-1_ [_prefix-2_.._prefix-n_][,_m-th_[,_n-th_]])
 Retrieve the value with the given prefixes from the separator list.
 Optionally select not the first but the _mth_ occurrence of this
 prefix value or a range from the _m-th_ to the _n-th_ (inclusive) occurrence.

#### $(call get-prfx-range,_separator-list_,_opening-prefix_,_closing-prefix_[,_n-th_])
 Retrieve the part of the separator list from _opening-prefix_ to _closing-prefix_ (inclusive).
 Alternatively return the _n-th_ occurrence of the _opening-prefix_/_closing-prefix_ combination.
 This function servers to select e.g. parenthesized parts of a string.


#### $(call get-sufx-range,_separator-list_,_opening-suffix_,_closing-suffix_[,_n-th_])
 Retrieve the part of the separator list from _opening-suffix_ to _closing-suffix_ (inclusive).
 Alternatively return the _n-th_ occurrence of the _opening-suffix_/_closing-suffix_ combination.
 This function servers to select e.g. parenthesized parts of a string.

### List functions

#### $(call uniq-sufx,_list_,_binary-literal_)
 Add a ¤ (Character 164) and a unique binary number to all elements of the _list_.
 The _binary-literal_ must be present and can be any combination of `0`'s and `1`'s.
 - `$(call uniq-sufx,The quick brown fox,0)` --> `The¤0 quick¤1 brown¤10 fox¤11`
 - `$(call uniq-sufx,The quick brown fox,111)` --> `The¤111 quick¤1000 brown¤1001 fox¤1010`

#### $(call sort-all,_list_)
 Sort a list without dropping duplicates. Built-in `$(sort)` will drop them which
 is sometimes not what you want. _Note_: list elements must not contain ¤ (Character 164)
 as this is character is used internally for processing.

#### $(call rev-list,_list_)
 Reverse the order of the elements of a list.
 - `$(call rev-list,The quick brown fox)` --> `fox brown quick The`

#### $(call list2param,_list_)
 Convert the given _list_ to a string where each list element is
 separated by a comma. Multiple spaces get reduced to one comma and
 there is no comma at the start and end of the list.
 - `$(call list2param,The   quick brown   fox)` --> `The,quick,brown,fox`

#### $(call quote-params,[_param1_,[_param2_,[_param3_,[_param4_,[_param5_,[_param6_,[_param7_,[_param8_,[_param9_]]]]]]]]])
 Convert all given parameters into a string with the format of the
 parameter list, i.e. a string with the verbatim parameters
 separated by commas. Use this function as a macro to channel an
 unknown number of function parameters into a string.

 Examples:
- `test = $(info <$(quote-params)>)`
- `$(call test,1,2,3,4,5,6,7,8,9)` -> `<1,2,3,4,5,6,7,8,9>` 
- `$(call test,,2,3,4,5,6,7,8,9)` -> `<,2,3,4,5,6,7,8,9>`  
- `$(call test,1)` -> `<1>`   
- `$(call test,1,2,3,4,5,6,,,9)` -> `<1,2,3,4,5,6,,,9>`  
- `$(call test,,,,,,,,,9)` -> `<,,,,,,,,9>`  
- `$(call test,,,,,,,,,)` -> `<,,,,,,,,>`   
- `$(call test,)` -> `<>`                  

#### $(call exec,_quoted-func_,_params_)
 Evaluate the _quoted-func_ code in place, using the _params_ as parameter list.
 _quoted-func_ is any GNUmake 'code' which could also appear on the rhs of a variable
 definition but in the quoted form: every appearance of '$' is to be quoted with an
 extra '$' (see examples). _params_ is a list of parameters separated by commas
 (see list2params function) which is given as the call parameters _$1_,_$2_,etc.
 to the function expression. 

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

#### $(call up-from,_word_,_list_)
 Return the portion of _list_ following the first occurrence of _word_.
 If _word_ is not in _list_, the empty string/list is returned.
 Examples:
 - `$(call up-from,foo,foo bar baz)` -> `bar baz`
 - `$(call up-from,baz,foo bar baz)` -> ` ` (empty list)

#### $(call down-to,_word_,_list_)
 Return the last part of _list_, searching from the end down to but
 excluding the last occurrence of _word_.
 If _word_ is not in _list_, the whole list is returned. This operation is 
 helpful in dissecting e.g. path expressions (´/´ substituted by ´ ´ or ´/ ´)
 Examples:
 - `$(call down-to,baz,foo baz bar baz)` -> ` ` (empty list)
 - `$(call down-to,foo,foo foo bar baz)` -> `bar baz`

#### $(call down-from,_word_,_list_)
 Return the first part of _list_, searching from the end down to but
 excluding the last occurrence of _word_.
 If _word_ is not in _list_, the empty list is returned. This operation is 
 helpful in dissecting e.g. path expressions (´/´ substituted by ´ ´ or ´/ ´)
 Examples:
 - `$(call down-from,baz,foo baz bar baz)` -> `foo baz bar`
 - `$(call down-from,foo,foo bar baz)` -> ` ` (empty list)

### Arithmetic functions

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

#### $(call cmp,_num1_,_num2_)
 Compare _num1 with num2_ and emit the characters `<`,`>` or `=`. Both arguments
 are signed integers. Numeric bases can be mixed.
 Examples:
 - `$(call cmp,12,-14)` --> `>`
 - `$(call cmp,012,10)` --> `=`

#### $(call int-(le|lt|ge|gt|eq),_num1_,_num2_)
 Compare _num1 with num2_ and emit the characters `<`,`>`,`=` or the empty string.
 The postfix alternatives decide which comparison is executed:
 less-than (lt), less-or-equal (le), greater-than (gt),
 greater-or-equal (ge) and equal (eq). 
 Both arguments are signed integers. Numeric bases can be mixed. The result can
 be used in an $(if ) expression.
 Examples:
 - `$(call int-ge,12,-14)` --> `>`
 - `$(call int-le,12,-14)` --> ` ` (empty string)
 - `$(if $(call int-eq,012,10),equal,not equal)` --> `equal`

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
 
### Table functions

#### $(call sort-tbl,_table_,_key-gen_)
 Sort a _table_ by lines. Key comparison is done by lexical ordering.
 Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
 always compares smaller than any other string. The strings may contain spaces
 but leading and trailing spaces are not considered in the comparison and
 multiple interior spaces are substituted by a single one. Spaces compare
 smaller than downcase characters but greater than upcase. In short: you
 should know what you are doing if you have spaces inside your strings.

 The _key-gen_ is a function expression which shall yield the key
 for the sorting comparison. Note that the key is *always* 
 evaluated as a string - this means that you usually can't use
 numerals directly as key but have to left-pad them with 0's to 
 make them the same length.

#### $(call select,_col-nrs_,_table_,_where-clause_)
 Select all rows from a _table_ which fulfill the _where-clause_ and pick the subset
 of _col-nrs_ from these rows to form an output list. A gmtt **table** is
 a list with a leading decimal which denotes the number of columns in this 'table'.
 See the documentation for gmtt at [https://github.com/markpiffer/gmtt].
 The _where-clause_ is a function or a 'exec' expression (i.e. function expression
 written into the parameter place directly) which receives the elements of each row of
 the table in order as parameters `$1`,`$2`,`$3` etc. **Note:** the function call/exec
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

 Exapmle:
 - `test-tbl := 4   foo bar baz 11    foo bar baf 22   faa bar baz 33`
 - `$(call select,3 1 2 3,$(test-tbl),$$(call str-match,$$1,%oo))` --> `baz foo bar baz baf foo bar baf`
 The same can be achieved, if we use a function as where clause:
 - `ends-in-oo = $(call str-match,$1,%oo)`
 - `$(call select,3 1 2 3,$(test-tbl),$$(call ends-in-oo,$$1))` --> `baz foo bar baz baf foo bar baf`
 
#### $(call join-tbl,_table1_,_table2_[,_NIL-value_])
Join two tables side by side. The resulting table has as many columns
as both input tables combined. The optional NIL value will be 
used to fill empty places in the resulting table if one input table has
less rows than the other. If the NIL value is *not* given, the shorter
of the two tables will be repeated as many times as necessary to fill up
the number of rows to that of the longer table.
The order of the tables is preserved.

Examples:
- `$(call join-tbl,1 one two,2 first 1st second 2nd third 3rd)` --> `3 one first 1st two second 2nd one third 3rd`
- `$(call join-tbl,2 one apple two oranges,1 1st 2nd 3rd,/*NIL*/)` --> `3 one apple 1st two oranges 2nd /*NIL*/ /*NIL*/ 3rd`

### File System Functions

#### $(call wildcard-rec,_list-of-globs_)
 _"wildcard recursive"_ is an extension to the built in `wildcard`
 function.  It turns a list of glob expressions into the list of all
 files (for expressions not ending in `/`) or directories
 (expressions ending in `/`) which are reachable in the file system
 by these globs. The given paths obey usual glob syntax (`*`, `?`
 etc.) with a special extension: there is one `%` or `**` allowed in
 the path expression which starts a recursive search of the file tree
 at this position of the glob. To explain this: the usual glob syntax
 `*` expresses just one directory level down, e.g. in `project/*/foo`
 each file "foo" in any of the subdirectories of `project` is
 found. There is no way to find a "foo" file deeper down the
 hierarchy - you would need to know how many levels deep the file is
 located e.g. `project/*/*/*/foo`. While this syntax is absolutely
 allowed, the expression `project/**/foo` addresses the former and
 all other existing directory levels in the tree no matter how
 deep. More complex patterns are also allowed:

  - `project**/foo` will select all first level directories which start with `project`: also directory `project001` will be considered
  - `project/**foo` will select all files in the tree under `project` which end in `foo`
  - `**.h` will select all C header files in the current file tree

 Example: for the following directory structure:
 
    .(working directory)
    |
    +---barfoo
         |   foo.c
         |
         +---bar
         |       bar
         |
         +---foo
         |       bar.foo
         |       foo.txt
         |
         \---foobar

the exemplary globs on the left will create the outputs to the right:

    **.c --> ./barfoo/foo.c 
    foo*/ --> foo/
    bar*/ --> barfoo/
    **/foo*/ --> ./barfoo/foo/ ./barfoo/foobar/ ./foo/
    **/foo* --> ./barfoo/foo.c ./barfoo/foo/foo.txt
    **/*foo* --> ./barfoo/foo.c ./barfoo/foo/bar.foo ./barfoo/foo/foo.txt
    **/bar --> ./barfoo/bar/bar
    **/bar/ --> ./barfoo/bar/


### Miscellaneous Functions

#### $(call head,_list_)
Functionally identical to `$(firstword )`. Return the head (first element) of a list.

#### $(call tail,_list_)
Return the second and subsequent elements of a list as a new list.

#### $(call clr-comments,_lines-with-hash-comments_)
Return the given string cleared from line comments '#'.
The string may contain spaces and newlines, these are returned untouched.
Example: 
- `config.txt:`
- `first #comment`
- `   # second comment`
- `  third  # comment`
-
- `cfg-file = $(call clr-comments,$(file < config.txt))`

Output:
- `first `
- `    `
- `  third  `


#### $(call while,_quoted-condition_,_quoted-code_[,_quoted-exit-statements_])
Execute a while loop of _quoted-code_ as long as _condition_ is true (not the empty string).
All code statements need to be given as quoted make code (replace `$` with `$$`). 
The code in _quoted-code_ and _quoted-exit-statements_ is `eval`ed in the while loop.
This means that you can use variable assignments like in ordinary code BUT the assignments
will be visible outside of the while loop! The optional _quoted-exit-statement_ is executed *always*
when leaving the while loop, even when the loop body was never executed.
- Example: filter out the `-mllvm` flags from the `CFLAGS` variable and put them in an extra variable
```
CFLAGS := -DFOO -DBAR -mllvm llvmflag1 -mllvm llvmflag2
$(call while, $$(call glob-match,$(space)$$(CFLAGS),*-mllvm *),\
   tmp := $$(call glob-match,$(space)$$(CFLAGS),*-mllvm *)   $(newline)\
   rest := $$(call spc-unmask,$$(word 3,$$(tmp)))            $(newline)\
   llvm_flg := $$(firstword $$(rest))                        $(newline)\
   CFLAGS := $$(firstword $$(tmp)) $$(call tail,$$(rest))    $(newline)\
   $$(info [[[$$(tmp) --- $$(rest) --- $$(CFLAGS) ]]])       $(newline)\
   MLLVM_FLAGS+=$$(llvm_flg),\
MLLVM_FLAGS := $$(strip $$(MLLVM_FLAGS))                     $(newline)\
CFLAGS := $$(call spc-unmask,$$(CFLAGS))\
)
```
- Condition: as long as `glob-match` returns a match of `-mllvm ` in `CFLAGS`. There is a small trick to avoid an empty match on the first `*`: we sneak a `$(space)` character in at the beginning of the string so that the match will never be empty.
- Body (notice the quoting of the newlines - this is necessary for `$(eval)` to correctly interpret the code):
   * Extract the output of `glob-match` into a temporary variable (spaces are replaced in this output, see `glob-match`). The output is a list of three elements: all characters (`*`) up to `-mllvm `, the string `-mllvm ` itself (notice the space at the end) and all characters (`*`) following it.
   * Convert back the third element (rest of `CFLAGS` behind the first `-mllvm ` match) into a string with spaces and put it into `rest`
   * Pluck the argument to the `-mllvm` flag (which is the first element of `rest`) into `llvm_flg`
   * Modify `CFLAGS` to contain everything except `-mllvm` and its argument
   * Print the variables as a debugging aid
   * Append the newly found argument of the `-mllvm` flag to the output variable
- Exit statements:
   * Pretty-print `MLLVM_FLAGS` 
   * Remove the space replacement characters from `CFLAGS` which have accumulated during the loop
- Output:
```
[[[ -DFOO§-DBAR§ -mllvm§ llvmflag1§-mllvm§llvmflag2 --- llvmflag1 -mllvm llvmflag2 --- -DFOO§-DBAR§ -mllvm llvmflag2 ]]]
[[[ -DFOO§-DBAR§§ -mllvm§ llvmflag2 --- llvmflag2 --- -DFOO§-DBAR§§  ]]]
CFLAGS = -DFOO -DBAR
MLLVM_FLAGS = llvmflag1 llvmflag2
```

#### $(call verbose,[_string0_],[_string1_],[_string2_],[_string3_],[_string4_],[_string5_],[_string6_],[_string7_],[_string8_],[_string9_])
 Write strings to stdout depending on warning level. The global variable `VERBOSITY`
 needs to be defined as a string of numerals from 0..9. This will  trigger the output
 of the according strings for all `$(call verbosity ...)` invocations in the makefile.
 Verbosity is additive, *not* hierarchical, i.e. `VERBOSITY=9` will not effect the output
 of all warnings/info-strings from level 0 to 9 but only for level 9. To output a
 combination or selection, set e.g. `VERBOSITY = 01289`.
 Examples:
 - `VERBOSITY = 0`
 - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 0`
 - `VERBOSITY = 2`
 - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 2`
 - `VERBOSITY = 012345`
 - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 0 warninglevel 1 warninglevel 2 info 1 info 2 info 3`
