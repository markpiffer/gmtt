#----------------------------------------------------------------------
#
# gmtt - GNU make table toolkit
#
# This collection of GNU make functions helps with the processing
# of compile-time configuration data which can come as lists, tables,
# numbers etc.
#
# It supports basic arithmetic with octal, decimal and hexadecimal numbers.
# All extensions come in the form of make-functions which need to be called
# with GNU make's $(call ) facility. You can find some examples in gmtt-tests.
#
# Internal functions begin (mostly) with a '-'. They may change without notice.
#
# Copyright (c) 2017 Mark Piffer
#

-gmtt-dbg-args = $(if $(-gmtt-dbg-info),$(info $0($(if $1$2$3$4$5$6,<$1>$(if $2$3$4$5$6,<$2>$(if $3$4$5$6,<$3>$(if $4$5$6,<$4>$(if $5$6,<$5>$(if $6,<$6>)))))))))


empty := #    
false := $(empty)#
space := $(strip) $(strip)#
empty-cell := $$(space)
define newline :=
$(strip)
$(strip)
endef
comma := ,#
hash := \##
percent := %#
colon := :#
equal := =#
lparen := (#
rparen := )#
semicolon := ;#

###### `Character classes`
## The following make variable carry their character class (as list):
## `$([upper])` := `A B C D E F G H I J K L M N O P Q R S T U V W X Y Z`
## `$([lower])` := `a b c d e f g h i j k l m n o p q r s t u v w x y z`
## `$([digit])` := `0 1 2 3 4 5 6 7 8 9`
## `$([xdigit])` := `$([digit]) a b c d e f A B C D E F`
## `$([alpha])` := `$([upper]) $([lower])`
## `$([alnum])` := `$([digit]) $([alpha])`
[upper] := A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
[lower] := a b c d e f g h i j k l m n o p q r s t u v w x y z
[digit] := 0 1 2 3 4 5 6 7 8 9
[xdigit] := $([digit]) a b c d e f A B C D E F
[alpha] := $([upper]) $([lower])
[alnum] := $([digit]) $([alpha])


-separator := ¤# character 164, used in various functions to compose/decompose strings
-never-matching := ¥# character 165, this is used as a list element that should never appear as a real element
-spacereplace := §# takes the place of space characters when needed
-lparenreplace := «# charracter 171, needed in glob-match because "(" and ")" as matching character messes up the parser
-rparenreplace := »# character 187

#----------------------------------------------------------------------
###### `$(all-chars-q)`
## This variable contains all ASCII characters in order,
## separated by space (which makes the variable also a list)
## with make special characters in quoted form. That means
## instead of these characters, the strings:
##  - `$(space)` for the space character
##  - `$(hash)` for the hash (#) character
##  - `$(colon)` for the colon (:) character
##  - `$(equal)` for the equal (=) character
## are written in. The purpose of this list or parts of
## it is to be used in `$(eval)` expressions which would
## otherwise have uncontrollable side effects due to make
## interpreting the characters.
all-chars-q := $$(space) ! " $$(hash) $ % & ' $$(lparen) $$(rparen) * + , - . / 0 1 2 3 4 5 6 7 8 9 $$(colon) ; < $$(equal) > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~ 
# " <- closing quotation mark to end string parsing in some editors :D

#----------------------------------------------------------------------
###### `$(all-chars)`
## This variable contains all ASCII characters in order,
## separated by space (which makes the variable also a list).
## Note that the make special characters space, colon(:), equal (=)
## and hash (#) are _not_ escaped which makes this string
## unsuitable for evaluation by the make parser (mostly `$(eval)`
## calls). Use this string as parameter for string and character
## functions (e.g. `$(explode $(all-chars),abc!"%123)`).
$(eval all-chars := $(all-chars-q))


#----------------------------------------------------------------------
##### String Functions
## The following are functions which work on strings. If a string may
## contain spaces or not is documented individually in each function.

#----------------------------------------------------------------------
###### $(call explode,_stringlist_,_string_)
## Insert a blank after every occurrence of the strings from _stringlist_ in _string_.
## This function serves mainly to convert a string into a list.
## - `$(call explode,0 1 2 3 4 5 6 7 8 9,0x1337c0de)` --> `0 x1 3 3 7 c0 de`
explode = $(if $1,$(subst $(firstword $1),$(firstword $1) ,$(call explode,$(wordlist 2,2147483647,$1),$2)),$2)

#----------------------------------------------------------------------
# Decompose a number into a list. The same as $(call explode,$([0-9]),string)
# but faster.
-xpld-8 = $(subst 0, 0,$(subst 1, 1,$(subst 2, 2,$(subst 3, 3,$(subst 4, 4,$(subst 5, 5,$(subst 6, 6,$(subst 7, 7,$1))))))))
-xpld-10 = $(subst 0, 0,$(subst 1, 1,$(subst 2, 2,$(subst 3, 3,$(subst 4, 4,$(subst 5, 5,$(subst 6, 6,$(subst 7, 7,$(subst 8, 8,$(subst 9, 9,$1))))))))))
-xpld-16 = $(subst 0, 0,$(subst 1, 1,$(subst 2, 2,$(subst 3, 3,$(subst 4, 4,$(subst 5, 5,$(subst 6, 6,$(subst 7, 7,$(subst 8, 8,$(subst 9, 9,$(subst a, a,$(subst b, b,$(subst c, c,$(subst d, d,$(subst e, e,$(subst f, f,$1))))))))))))))))

#----------------------------------------------------------------------
###### $(call implode,_string-with-spaces_)
## Remove all spaces from the given string. Note that make is mostly unaware of escape
## characters and therefore takes all spaces verbatim.
## - `$(call implode,some\ awkward\ Windows\ path)` --> `some\awkward\Windows\path`
implode = $(subst $(space),,$(strip $1))

###### Predefined character classes for chop-str et al
## - `$(-upper-as-str) := ABCDEFGHIJKLMNOPQRSTUVWXYZ`
## - `$(-lower-as-str) := abcdefghijklmnopqrstuvwxyz`
## - `$(-digit-as-str) := 0123456789`
## - `$(-xdigit-as-str) := 0123456789abcdefABCDEF`
## - `$(-alpha-as-str) := ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`
## - `$(-alnum-as-str) := 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`
-upper-as-str := $(call implode,$([upper]))
-lower-as-str := $(call implode,$([lower]))
-digit-as-str := $(call implode,$([digit]))
-xdigit-as-str := $(call implode,$([xdigit]))
-alpha-as-str := $(call implode,$([alpha]))
-alnum-as-str := $(call implode,$([alnum]))

#----------------------------------------------------------------------
###### $(call spc-mask,_string_)
## Replace all occurrences of space in the given string with the
## `$(-spacereplace)` character. This function can be used to convert
## strings with spaces into valid list elements (which must not contain
## spaces). 
spc-mask = $(subst $(space),$(-spacereplace),$1)$(if $2$3$4$5$6$7$8$9$(10)$(11)$(12)$(13)$(14)$(15)$(16)$(17)$(18)$(19)$(20)$(21)$(22)$(23)$(24)$(25)$(26)$(27)$(28)$(29)$(30)$(31)$(32),$(comma)$(call spc-mask,$2,$3,$4,$5,$6,$7,$8,$9,$(10),$(11),$(12),$(13),$(14),$(15),$(16),$(17),$(18),$(19),$(20),$(21),$(22),$(23),$(24),$(25),$(26),$(27),$(28),$(29),$(30),$(31),$(32)))

###### $(call spc-unmask,_string_)
## Replace all occurrences of the character `$(-spacereplace)` with a
## true space character. The inverse of the `spc-mask` function.
spc-unmask = $(subst $(-spacereplace),$(space),$1)$(if $2$3$4$5$6$7$8$9$(10)$(11)$(12)$(13)$(14)$(15)$(16)$(17)$(18)$(19)$(20)$(21)$(22)$(23)$(24)$(25)$(26)$(27)$(28)$(29)$(30)$(31)$(32),$(comma)$(call spc-unmask,$2,$3,$4,$5,$6,$7,$8,$9,$(10),$(11),$(12),$(13),$(14),$(15),$(16),$(17),$(18),$(19),$(20),$(21),$(22),$(23),$(24),$(25),$(26),$(27),$(28),$(29),$(30),$(31),$(32)))

#----------------------------------------------------------------------
# List of match characters for globbing separated by space. All special
# characters quoted and space char replaced by $(-spacereplace)
-match-chars-q := $(subst $$(space),$(-spacereplace),$(all-chars-q))

#----------------------------------------------------------------------
# List of match characters for globbing, separated by space. Space
# replaced by $(-spacereplace)
$(eval -match-chars := $(-match-chars-q))

#----------------------------------------------------------------------
# String of all match characters without spaces.
-match-chars-str := $(call implode,$(-match-chars))

#----------------------------------------------------------------------
# $(call -match-char-range,_start-char_,_end-char_)
# Return list of characters in -match-chars starting *behind*
# start-char up to end-char. Used for glob matching character ranges
-match-char-range = $(wordlist 2,2147483647,$(call explode,$(-match-chars),$(firstword $(subst $2,$2 ,$(lastword $(subst $1, $1,$(-match-chars-str)))))))

#----------------------------------------------------------------------
# Repeat a string for 2^N times. N is given in the form of a list with length N.
# $1 = string to repeat
# $2 = list of length N
# Example: (foo,1 2 3) --> foofoofoofoofoofoofoofoo
--rpt2pN = $(subst $1,$1$1,$(if $(wordlist 2,2,$2),$(call --rpt2pN,$1,$(wordlist 2,2147483647,$2)),$1))

#----------------------------------------------------------------------
# Repeat a string for 2^N times. N is given as a decimal.
# $1 = string to repeat
# $2 = N
# Example: (foo,3) --> foofoofoofoofoofoofoofoo
-rpt2pN = $(call --rpt2pN,$1,$(wordlist 1,$2, _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _))

#----------------------------------------------------------------------
###### $(call n-list,_string_,_number-of-repetitions_)
## Create a list with exactly _number-of-repetitions_ copies of a _string_.
## - `$(call n-list,foo,3)` --> `foo foo foo`
n-list = $(if $(word $2,$1),$(wordlist 1,$2,$1),$(call n-list,$1 $1,$2))

#----------------------------------------------------------------------
###### $(call binary-inc,_binary-literal_)
## Count the _binary-literal_ up by 1, yielding the following binary literal.
## Leading zeros are preserved.
## - `$(call binary-inc,010011)` -> `010100`
binary-inc = $(if $1,$(if $(patsubst %1,,$1),$(patsubst %0,%1,$1),$(call binary-inc,$(patsubst %1,%,$1))0),1)

#----------------------------------------------------------------------
###### $(call decimal-inc,_decimal-literal_)
## Count the _decimal-literal_ up by 1, yielding the following decimal literal.
## Leading zeros are preserved.
## - `$(call decimal-inc,0099)` -> `0100`
decimal-inc = $(if $1,$(if $(patsubst %9,,$1),$(if $(patsubst %8,,$1),$(if $(patsubst %7,,$1),$(if $(patsubst %6,,$1),$(if $(patsubst %5,,$1),$(if $(patsubst %4,,$1),$(if $(patsubst %3,,$1),$(if $(patsubst %2,,$1),$(if $(patsubst %1,,$1),$(patsubst %0,%1,$1),$(patsubst %1,%2,$1)),$(patsubst %2,%3,$1)),$(patsubst %3,%4,$1)),$(patsubst %4,%5,$1)),$(patsubst %5,%6,$1)),$(patsubst %6,%7,$1)),$(patsubst %7,%8,$1)),$(patsubst %8,%9,$1)),$(call decimal-inc,$(patsubst %9,%,$1))0),1)

#----------------------------------------------------------------------
###### $(call symgen)
## Generate a different string at each call. The last generated symbol is
## accessible via `$(last-symgen)`.
## - `$(call symgen)` --> `sym0`
symgen = $(eval last-symgen:=sym$(call binary-inc,$(subst sym,,$(last-symgen))))$(last-symgen)

#----------------------------------------------------------------------
###### $(call interval,_start_,_range_[,_step_])
## Create a list of integers starting at _start_ and having _range_ elements
## with an increase (or decrease) of _step_ from one to the next. _step_ is optional
## and defaults to 1 if not given.
##
## Example:
## - `$(call interval,5,5)` --> `5 6 7 8 9`
## - `$(call interval,2,3,100)` --> `2 102 202`
interval = $(eval -interval := $1)$(foreach i,$(call n-list,_,$2),$(-interval)$(eval -interval := $$(call add,$$(-interval),$(or $3,1))))

#----------------------------------------------------------------------
###### $(call lpad,_string_,_final-width_,_padding-character_)
## Left-pad an alphanumeric string with the given character up to the given length.
## If the original string is longer than _final-width_, nothing is padded.
## Expample:
## - `$(call lpad,123,7,0)` --> `0000123`
## - `$(call lpad,123,7,-)` --> `----123`
lpad = $(call implode,$(wordlist $(words _ $(call explode,$([alnum]),$1)),$2,$(call n-list,$3,$2)))$1

#----------------------------------------------------------------------
###### $(call lstrip,_string_,_prefix_)
## Remove a _prefix_ from the given _string_. If the prefix doesn't exist, the
## string is unchanged.
## - `$(call lstrip,0xABCD,0x)` --> `ABCD`
lstrip = $(patsubst $2%,%,$1)

#----------------------------------------------------------------------
###### $(call str-eq,_string1_,_string2_)
## Compare two strings on equality. Strings are allowed to have blanks.
## Return non-empty if string $1 and $2 are identical, empty string otherwise.
## - `$(call str-eq,yes,no)` --> ` ` (empty string)
## - `$(call str-eq,yes ,yes)` --> ` ` (empty string)
## - `$(call str-eq,yes ,yes )`  --> `t`
str-eq = $(if $(subst x$1,,x$2),,t)

#----------------------------------------------------------------------
###### $(call str-le,_string1_,_string2_)
## Compare two strings lexically for _string1_ less-or-equal _string2_.
## Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
## always compares smaller than any other string. The strings may contain spaces
## but leading and trailing spaces are not considered in the comparison and
## multiple interior spaces are substituted by a single one. Spaces compare
## smaller than downcase characters but greater than upcase. In short: you
## should know what you are doing if you have spaces inside your strings.
## Examples:
## - `$(call str-le,aaa,ab))` --> `t` 			     
## - `$(call str-le,   ab aa,aa))` --> ` `	(empty string)	     
## - `$(call str-le,aa,aa))` --> `t`			     
## - `$(call str-le,aa,a))` --> ` `	(empty string)		     
## - `$(call str-le,a,))` --> ` ` (empty string)			     
## - `$(call str-le,,a))` --> `t`			     
## - `$(call str-le,MacGyver John,Mac Gyver John))` --> `t`
## - `$(call str-le,macgyver john,mac gyver john))` --> ` ` (empty string)
str-le = $(if $(filter _$(subst $(space),_,$(strip $1)),$(firstword $(sort _$(subst $(space),_,$(strip $1)) _$(subst $(space),_,$(strip $2))))),t,$(if $1$2,,t))

#----------------------------------------------------------------------
###### $(call str-ge,_string1_,_string2_)
## Compare two strings lexically for _string1_ greater-or-equal _string2_.
## Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
## always compares smaller than any other string. The strings may contain spaces
## but leading and trailing spaces are not considered in the comparison and
## multiple interior spaces are substituted by a single one. Spaces compare
## smaller than downcase characters but greater than upcase. In short: you
## should know what you are doing if you have spaces inside your strings.
## Examples:
## - `$(call str-ge,aaa,ab))` --> ` ` (empty string) 			     
## - `$(call str-ge,   ab aa,aa))` --> `t`
## - `$(call str-ge,aa,aa))` --> `t`			     
## - `$(call str-ge,aa,a))` --> `t`
## - `$(call str-ge,a,))` --> `t`			     
## - `$(call str-ge,,a))` --> ` ` (empty string)			     
## - `$(call str-ge,MacGyver John,Mac Gyver John))` --> ` ` (empty string)
## - `$(call str-ge,macgyver john,mac gyver john))` --> `t`
str-ge = $(if $(filter _$(subst $(space),_,$(strip $2)),$(firstword $(sort _$(subst $(space),_,$(strip $1)) _$(subst $(space),_,$(strip $2))))),t,$(if $1$2,,t))


#----------------------------------------------------------------------
#word-le = $(filter $1,$(firstword $(sort $1 $2)))

#----------------------------------------------------------------------
#word-ge = $(filter $2,$(firstword $(sort $1 $2)))


#----------------------------------------------------------------------
###### $(call str-match,_string1_,_string2_)
## Compare two strings on equality under wildcard substitution. The wildcard
## can appear in both of the strings.
## Return t if _string1_ and _string2_ match where the first % is taken as
## wildcard, return empty string otherwise. 
## - `$(call str-match,Mickey%Mouse,Mickey Mouse))` --> `t`
## - `$(call str-match,Mickey%,MickeyMouse))` --> `t`
## - `$(call str-match,Mickey%,))` --> ` ` (empty string)
## - `$(call str-match,Mickey %ouse,Mickey Mouse))` --> `t`
## - `$(call str-match,MickeyMouse,MickeyMouse%))` --> `t`
## - `$(call str-match,,%))` --> `t`
str-match = $(if $(and $(patsubst _$(subst $(space),$(-separator),$(strip $1)),,_$(subst $(space),$(-separator),$(strip $2))),$(patsubst _$(subst $(space),$(-separator),$(strip $2)),,_$(subst $(space),$(-separator),$(strip $1)))),,t)

#----------------------------------------------------------------------
# $(call -glob-match-X,_string_,_pattern_,_match-result_)
# Match the character X against the first character of _string_. If
# they match, recursively call -glob-match-Y, with Y being the next
# character from _pattern_ and glue the character to _match-result_.
# On match failure, return false (empty string).
# The particular functions are generated out of the list of characters.
$(foreach c,$(-match-chars-q),$(eval -glob-match-$(c) = $$(if $$1,$$(if $$(findstring $$(firstword $$1),$(c)),$$(call -glob-match-$$(firstword $$2),$$(wordlist 2,2147483647,$$1),$$(wordlist 2,2147483647,$$2),$$(3)$(c))))))
# Rewrite the matching functions for special characters in a glob pattern
# opening and closing parentheses are replaced by special characters bcs the make parser gets confused
-glob-match-$(-lparenreplace) = $(if $1,$(if $(findstring $(firstword $1),$(-lparenreplace)),$(call -glob-match-$(firstword $2),$(wordlist 2,2147483647,$1),$(wordlist 2,2147483647,$2),$(3)$(lparen))))
-glob-match-$(-rparenreplace) = $(if $1,$(if $(findstring $(firstword $1),$(-rparenreplace)),$(call -glob-match-$(firstword $2),$(wordlist 2,2147483647,$1),$(wordlist 2,2147483647,$2),$(3)$(rparen))))
# End of pattern:
-glob-match- = $(if $1,,$3)
# Asterisk - First try if the * matches the empty string and recurse
# with the next character from the _pattern_. If that fails, consume
# one character from _string_ and try again.
-glob-match-* = $(if $1,$(or $(call -glob-match-$(firstword $2),$1,$(wordlist 2,2147483647,$2),$3 ),$(call --glob-match-*,$(wordlist 2,2147483647,$1),$2,$3 $(word 1,$1))),$(if $2,,$3))
--glob-match-* = $(if $1,$(or $(call -glob-match-$(firstword $2),$1,$(wordlist 2,2147483647,$2),$3 ),$(call --glob-match-*,$(wordlist 2,2147483647,$1),$2,$3$(word 1,$1))),$(if $2,,$3))
# Question mark - consume one character from _string_. Fails only if
# _string_ is already empty.
-glob-match-? = $(if $1,$(call -glob-match-$(firstword $2),$(wordlist 2,2147483647,$1),$(wordlist 2,2147483647,$2),$3 $(word 1,$1) ))
# Opening brackets - start a [] sequence for character (range) selection and match it
-glob-match-[ = $(if $1,$(if $(findstring !,$(firstword $2)),$(call -neg-bracket-enter,$1,$(wordlist 2,2147483647,$2),$3),$(call -bracket-enter,$1,$2,$3)))
# $(call bracket-...,_string_,_pattern_,_bracket-chars-so-far_,_match-result_)
# The bracket- and neg-bracket- functions step through a bracket
# expression an accumulate the characters and character ranges
# in _bracket-chars-so-far_ until the closing ']' is encountered.
-bracket-enter = $(call -bracket-cont,$1,$(wordlist 2,2147483647,$2),$(firstword $2),$3)
-neg-bracket-enter = $(call -neg-bracket-cont,$1,$(wordlist 2,2147483647,$2),$(firstword $2),$3)
-bracket-cont = $(if $(findstring ],$(firstword $2)),$(call -bracket-match,$1,$(wordlist 2,2147483647,$2),$3,$4),$(if $(findstring -,$(firstword $2)),$(if $(findstring ],$(word 2,$2)),$(call -bracket-match,$1,$(wordlist 3,2147483647,$2),$3 -,$4),$(call -bracket-cont,$1,$(wordlist 3,2147483647,$2),$3 $(call -match-char-range,$(lastword $3),$(word 2,$2)),$4)),$(call -bracket-cont,$1,$(wordlist 2,2147483647,$2),$3 $(firstword $2),$4)))
-neg-bracket-cont = $(if $(findstring ],$(firstword $2)),$(call -neg-bracket-match,$1,$(wordlist 2,2147483647,$2),$3,$4),$(if $(findstring -,$(firstword $2)),$(if $(findstring ],$(word 2,$2)),$(call -neg-bracket-match,$1,$(wordlist 3,2147483647,$2),$3 -,$4),$(call -neg-bracket-cont,$1,$(wordlist 3,2147483647,$2),$3 $(call -match-char-range,$(lastword $3),$(word 2,$2)),$4)),$(call -neg-bracket-cont,$1,$(wordlist 2,2147483647,$2),$3 $(firstword $2),$4)))
# bracket-match finally tries to match the first character in
# _string_ against the accumulated _bracket-chars-so-far_ and
# if succeeding, continues recursively with -glob-match-Y 
-neg-bracket-match = $(if $(findstring $(firstword $1),$3),,$(call -glob-match-$(firstword $2),$(wordlist 2,2147483647,$1),$(wordlist 2,2147483647,$2),$4 $(firstword $1) ))
-bracket-match = $(if $(findstring $(firstword $1),$3),$(call -glob-match-$(firstword $2),$(wordlist 2,2147483647,$1),$(wordlist 2,2147483647,$2),$4 $(firstword $1) ))
#----------------------------------------------------------------------
###### $(call glob-match,_string_,_pattern_)
## Try to match the _string_ with the _pattern_, applying glob-syntax.
## Glob-syntax is well known from the shell and
## https://en.wikipedia.org/wiki/Glob_(programming)
## All characters match themselves except:
## -  `*` - zero or more arbitrary chars
## -  `?` - exactly one arbitrary char
## -  `[]` - exactly one character from the set designated inside the brackets:
##    - `[abc]` - explicit, matches one of `a`, `b` or `c`
##    - `[a-z]` - range, matches one of `a`,`b`...`z`. The
##                possibly ranges can be taken from `$(all-chars)`
##    - `[]abc]` - first position is the only way to match a `]`
##    - `[-abc]` - first or last position is the only way to match a `-`
##    - `[!a-z]` - `!` inverts the match, i.e. everything but `a`..`z`
## If no match occurred, the  ` ` (empty string) is returned.
## If the string matches, it is returned with all parts corresponding to one of the above
## wildcards separated by space.
## As GNUmake treats everything which is different from the empty string
## as true, this function serves the simple matching test as well as a string
## dissection by wildcard patterns.
## Spaces inside your string are automatically preserved. The elements of the string
## are returned but with all original spaces replaced by an internal character to
## circumvent the rule that GNUmake treats spaces as list element separators.
## Use `spc-unmask` to remove these and restore the spaces. This caveat is necessary
## to allow access to the elements with a constant index, in case you are dissecting
## a string.
## Examples:
## - `$(call glob-match,Linux 2.6.32-431.el6.i686,Linux 2.6.*.i686)` --> `Linux§2.6. 32-431.el6 .i686`
## - `$(call glob-match,down/to/unknown/dir/file.txt,down/*/*/*/*.txt)` --> `down/ to / unknown / dir / file .txt`
glob-match = $(call -glob-match,$(subst $(rparen),$(-rparenreplace),$(subst $(lparen),$(-lparenreplace),$(call explode,$(-match-chars),$(subst $(space),$(-spacereplace),$1)))),$(subst $(rparen),$(-rparenreplace),$(subst $(lparen),$(-lparenreplace),$(call explode,$(-match-chars),$(subst $(space),$(-spacereplace),$2)))))
-glob-match = $(call -glob-match-$(firstword $2),$1,$(wordlist 2,2147483647,$2))


###### `$(call chop-str,_string_,_group-string_[,_group-string_[,_group-string_[,_group-string_]]])`
## Dissect given _string_ into chunks of characters belonging to the
## same _group-string_ and return the result as a separator list. A
## _group-string_ is a tuple of an _identifier_ string and a
## _group-characters_ string with a space in between (e.g.`digit
## 0123456789`). Each character in the given _string_ is tested for
## membership in one of the _group-characters_. If adjacent characters
## belong to the same _group-string_ they stay glued together
## otherwise a space is inserted.  The _identifier_ is attached as a
## prefix to the character chunks so that subsequent functions can
## distinguish the groups in the result. If a character in _string_ is
## not found in any of the group strings, it is dropped but still
## separates chunks. This function mainly serves to cut formated
## alphanumeric strings apart.
## *How to handle strings with spaces*:
## if you don't need the spaces after parsing (spaces only separate
## the string parts) then you don't have to do anything, simply call
## the function with the string as parameter.  If you want to preserve
## spaces, then you must call `chop-str-spc` which will leave behind
## the same list of string parts _but with spaces still replaced by an
## internal character_. The space character is handled as if it is
## part of the first _group-string_ so all sequences of characters
## from the first group which are separated by space will become one
## chunk.  This way you can still handle the function result as a list
## after calling.  When stepping through this list you can remove the
## internal character by simply applying `$(call
## spc-unmask,_string-chunk-from-result_)`.
## Examples:
## - `$(call chop-str,Linux 4.13.0-17-generic,A $(-alpha-as-str),1 $(-digit-as-str),. .-+?)` --> ` AÂ¤Linux 1Â¤4 .Â¤. 1Â¤13 .Â¤. 1Â¤0 .Â¤- 1Â¤17 .Â¤- AÂ¤generic`
## - `$(call chop-str,Thu Nov 30 18:43:22 CET 2017,alpha $(-alpha-as-str),num $(-digit-as-str),sep :)` --> ` alphaÂ¤Thu alphaÂ¤Nov numÂ¤30 numÂ¤18 sepÂ¤: numÂ¤43 sepÂ¤: numÂ¤22 alphaÂ¤CET numÂ¤2017`
## - `$(call chop-str-spc,Thu Nov 30 18:43:22 CET 2017,alpha $(-alpha-as-str),num $(-digit-as-str),sep :)` --> ` alphaÂ¤ThuÂ§NovÂ§ numÂ¤30 alphaÂ¤Â§ numÂ¤18 sepÂ¤: numÂ¤43 sepÂ¤: numÂ¤22 alphaÂ¤Â§CETÂ§ numÂ¤2017`
chop-str = $(call -chop-str,$(call explode,$(all-chars) $(-spacereplace) $(-separator) $(-never-matching),$(subst $(space),$(-never-matching),$1)),,$2,$3,$4,$5)
chop-str-spc = $(call chop-str,$(call spc-mask,$1),$2$(-spacereplace),$3,$4,$5)

# $1 - string to dissect
# $2 - result list with marked & dissected string parts
# $3 - 1st string group
# $4 - 2nd string group (optional)
# $5 - 3rd string group (optional)
# $6 - 4th string group (optional)
-chop-str = $(if $1,$(if $(findstring $(firstword $1),$(lastword $3)),$(call -chop-str-3,$(wordlist 2,2147483647,$1),$2 $(firstword $3)$(-separator)$(firstword $1),$3,$4,$5,$6),$(if $(findstring $(firstword $1),$(lastword $4)),$(call -chop-str-4,$(wordlist 2,2147483647,$1),$2 $(firstword $4)$(-separator)$(firstword $1),$3,$4,$5,$6),$(if $(findstring $(firstword $1),$(lastword $5)),$(call -chop-str-5,$(wordlist 2,2147483647,$1),$2 $(firstword $5)$(-separator)$(firstword $1),$3,$4,$5,$6),$(if $(findstring $(firstword $1),$(lastword $6)),$(call -chop-str-6,$(wordlist 2,2147483647,$1),$2 $(firstword $6)$(-separator)$(firstword $1),$3,$4,$5,$6),$(call -chop-str,$(wordlist 2,2147483647,$1),$2,$3,$4,$5,$6))))),$2)
-chop-str-3 = $(if $(findstring $(firstword $1),$(lastword $3)),$(call -chop-str-3,$(wordlist 2,2147483647,$1),$2$(firstword $1),$3,$4,$5,$6),$(call -chop-str,$1,$2,$3,$4,$5,$6))
-chop-str-4 = $(if $(findstring $(firstword $1),$(lastword $4)),$(call -chop-str-4,$(wordlist 2,2147483647,$1),$2$(firstword $1),$3,$4,$5,$6),$(call -chop-str,$1,$2,$3,$4,$5,$6))
-chop-str-5 = $(if $(findstring $(firstword $1),$(lastword $5)),$(call -chop-str-5,$(wordlist 2,2147483647,$1),$2$(firstword $1),$3,$4,$5,$6),$(call -chop-str,$1,$2,$3,$4,$5,$6))
-chop-str-6 = $(if $(findstring $(firstword $1),$(lastword $6)),$(call -chop-str-6,$(wordlist 2,2147483647,$1),$2$(firstword $1),$3,$4,$5,$6),$(call -chop-str,$1,$2,$3,$4,$5,$6))


###### $(call drop-prfx,_separator-list_)
## Clear the prefixes from a separator list. A separator list
## is a make list with a prefix name prepended to each list
## element and the internal character `$(-separator)` between them
## e.g. `alphaÂ¤Linux  numÂ¤4  dotÂ¤.  numÂ¤2`. 
drop-prfx = $(filter-out %$(-separator),$(subst $(-separator),$(-separator) ,$1))

###### $(call drop-sufx,_separator-list_)
## Clear the suffixes (usually the data) from a separator list.
## What remains is a list of prefixes only . A separator list
## is a make list with a prefix name prepended to each list
## element and the internal character `$(-separator)` between them
## e.g. `alphaÂ¤Linux  numÂ¤4  dotÂ¤.  numÂ¤2`. 
drop-sufx = $(filter-out $(-separator)%,$(subst $(-separator), $(-separator),$1))

###### $(call filter-prfx,_separator-list_,_prefix-1_ [_prefix-2_.._prefix-n_])
filter-prfx = $(filter $(addsuffix $(-separator)%,$2),$1)
filter-out-prfx = $(filter-out $(addsuffix $(-separator)%,$2),$1)
filter-sufx = $(filter $(addprefix %$(-separator),$2),$1)
filter-out-sufx = $(filter-out $(addprefix %$(-separator),$2),$1)

###### $(call get-prfx-val,_separator-list_,_prefix-1_ [_prefix-2_.._prefix-n_][,_m-th_[,_n-th_]])
## Retrieve the value with the given prefixes from the separator list.
## Optionally select not the first but the _mth_ occurrence of this
## prefix value or a range from the _m-th_ to the _n-th_ (inclusive) occurrence.
get-prfx-val = $(wordlist $(call decimal-inc,$3),$(call decimal-inc,$(or $4,$3)),$(call drop-sufx,$(filter $(addprefix %$(-separator),$2),$1)))
get-sufx-val = $(wordlist $(call decimal-inc,$3),$(call decimal-inc,$(or $4,$3)),$(call drop-prfx,$(filter $(addsuffix $(-separator)%,$2),$1)))

###### $(call get-prfx-range,_separator-list_,_opening-prefix_,_closing-prefix_[,_n-th_])
## Retrieve the part of the separator list from _opening-prefix_ to _closing-prefix_ (inclusive).
## Alternatively return the _n-th_ occurrence of the _opening-prefix_/_closing-prefix_ combination.
## This function servers to select e.g. parenthesized parts of a string.
get-prfx-range = $(if $(filter $2$(-separator)%,$(firstword $1)),$(call -get-prfx-range,$(wordlist 2,2147483647,$1),$2,$3,$(or $4,0),$(or $5,0),$(firstword $1)),$(if $1,$(call get-prfx-range,$(wordlist 2,2147483647,$1),$2,$3,$(or $4,0),$(or $5,0))))
-get-prfx-range = $(if $1,$(if $(filter $3$(-separator)%,$(firstword $1)),$(if $(call str-eq,$4,$5),$6 $(firstword $1),$(call get-prfx-range,$1,$2,$3,$4,$(call decimal-inc,$5))),$(call -get-prfx-range,$(wordlist 2,2147483647,$1),$2,$3,$4,$5,$6 $(firstword $1))))

###### $(call get-sufx-range,_separator-list_,_opening-suffix_,_closing-suffix_[,_n-th_])
## Retrieve the part of the separator list from _opening-suffix_ to _closing-suffix_ (inclusive).
## Alternatively return the _n-th_ occurrence of the _opening-suffix_/_closing-suffix_ combination.
## This function servers to select e.g. parenthesized parts of a string.
get-sufx-range = $(if $(filter %$(-separator)$2,$(firstword $1)),$(call -get-sufx-range,$(wordlist 2,2147483647,$1),$2,$3,$(or $4,0),$(or $5,0),$(firstword $1)),$(if $1,$(call get-sufx-range,$(wordlist 2,2147483647,$1),$2,$3,$(or $4,0),$(or $5,0))))
-get-sufx-range = $(if $1,$(if $(filter %$(-separator)$3,$(firstword $1)),$(if $(call str-eq,$4,$5),$6 $(firstword $1),$(call get-sufx-range,$1,$2,$3,$4,$(call decimal-inc,$5))),$(call -get-sufx-range,$(wordlist 2,2147483647,$1),$2,$3,$4,$5,$6 $(firstword $1))))

#----------------------------------------------------------------------
###### $(call uniq-sufx,_list_,_binary-literal_)
## Add a ¤ (Character 164) and a unique binary number to all elements of the _list_.
## The _binary-literal_ must be present and can be any combination of `0`'s and `1`'s.
## - `$(call uniq-sufx,The quick brown fox,0)` --> `The¤0 quick¤1 brown¤10 fox¤11`
## - `$(call uniq-sufx,The quick brown fox,111)` --> `The¤111 quick¤1000 brown¤1001 fox¤1010`
uniq-sufx = $(if $1,$(firstword $1)$(-separator)$2 $(call uniq-sufx,$(wordlist 2,2147483647,$1),$(call binary-inc,$2)))

#----------------------------------------------------------------------
###### $(call sort-all,_list_)
## Sort a list without dropping duplicates. Built-in `$(sort)` will drop them which
## is sometimes not what you want. _Note_: list elements must not contain ¤ (Character 164)
## as this is character is used internally for processing.
sort-all = $(foreach i,$(sort $(call uniq-sufx,$1,0)),$(firstword $(subst $(-separator), ,$(i))))

#----------------------------------------------------------------------
###### $(call rev-list,_list_)
## Reverse the order of the elements of a list.
## - `$(call rev-list,The quick brown fox)` --> `fox brown quick The`
rev-list = $(strip $(call -rev-list,$1,1073741824 1073741825 536870912 536870913 268435456 268435457 134217728 134217729 67108864 67108865 33554432 33554433 16777216 16777217 8388608 8388609 4194304 4194305 2097152 2097153 1048576 1048577 524288 524289 262144 262145 131072 131073 65536 65537 32768 32769 16384 16385 8192 8193 4096 4097 2048 2049 1024 1025 512 513 256 257 128 129 64 65 32 33 16 17))
-rev-list = $(if $1,$(if $2,$(call -rev-list,$(wordlist $(word 2,$2),21819281782,$1),$(wordlist 3,64,$2)) $(call -rev-list,$(wordlist 1,$(firstword $2),$1),$(wordlist 3,64,$2)),$(word 16,$1) $(word 15,$1) $(word 14,$1) $(word 13,$1) $(word 12,$1) $(word 11,$1) $(word 10,$1) $(word 9,$1) $(word 8,$1) $(word 7,$1) $(word 6,$1) $(word 5,$1) $(word 4,$1) $(word 3,$1) $(word 2,$1) $(firstword $1)))


###### $(call shorten,_list_,[[_n_],_m_])
## Delete _n_ elements from the front and _m_ elements from the tail
## of _list_. Both _n_ and _m_ are optional parameters and default to 0.
## Counting starts from 0, that is `$(call shorten,1 2 3,1,1)` evaluates to `2`. 
shorten = $(call rev-list,$(wordlist $(call decimal-inc,$(or $3,0)),2147483647,$(call rev-list,$(wordlist $(call decimal-inc,$(or $2,0)),2147483647,$1))))

#----------------------------------------------------------------------
###### $(call list2param,_list_)
## Convert the given _list_ to a string where each list element is
## separated by a comma. Multiple spaces get reduced to one comma and
## there is no comma at the start and end of the list.
## - `$(call list2param,The   quick brown   fox)` --> `The,quick,brown,fox`
list2param = $(subst $(space),$(comma),$(strip $1))

#----------------------------------------------------------------------
###### $(call exec,_quoted-func_,_params_)
## Evaluate the _quoted-func_ code in place, using the _params_ as parameter list.
## _quoted-func_ is any GNUmake 'code' which could also appear on the rhs of a variable
## definition but in the quoted form: every appearance of '$' is to be quoted with an
## extra '$' (see examples). _params_ is a list of parameters separated by commas
## (see list2params function) which is given as the call parameters _$1_,_$2_,etc.
## to the function expression. 
exec = $(eval -exec=$1)$(eval -exec:=$$(call -exec,$(call list2param,$(subst $(hash),$$(hash),$2))))$(-exec)
lambda2 = $(eval -lambda2=$2)$(eval -lambda2:=$$(call -lambda2$(subst $(space),,$(wordlist 1,$1,$(lambda-param)))))$(-lambda2)

lambda-param := ,$$3 ,$$4  ,$$5 ,$$6 ,$$7 ,$$8 ,$$9 ,$$10 ,$$11 ,$$12 ,$$13 ,$$14 ,$$15 ,$$16 ,$$17 ,$$18 ,$$19 ,$$20 ,$$21 ,$$22 ,$$23 ,$$24 ,$$25 ,$$26 ,$$27 ,$$28 ,$$29 ,$$30 ,$$31 ,$$32 ,$$33



#----------------------------------------------------------------------
###### $(call up-to,_word_,_list_)
## Return first part of _list_ up to but excluding the first occurrence of _word_.
## If _word_ is not in _list_, the whole list is returned.
## Examples:
## - `$(call up-to,baz,foo bar baz)` -> `foo bar`
## - `$(call up-to,foo,foo bar baz)` -> ` ` (empty list)
up-to = $(if $(findstring $1,$(firstword $2)),,$(strip $(subst $(-separator), ,$(firstword $(subst $(-separator)$1$(-separator), ,$(subst $(space),$(-separator), $2 ))))))

#----------------------------------------------------------------------
###### $(call index-of,_word_,_list_)
## Return the index of the first occurrence of _word_ if present or the empty list.
## *Indexing starts at 0*, contrary to the make-internal behaviour of numbering lists from 1!
## Note that you can always have an index starting at 1 by prefixing the list at
## call time with the $(-never-matching) element (or a simple underscore, if you are sure that 
## it is never a real element), i.e. `$(call index-of,foo,$(-never-matching) foo bar baz)` will give index 1.
## Examples:
## - `$(call index-of,foo,foo bar baz)` -> `0`
index-of = $(if $(findstring $1,$2),$(words $(call up-to,$1,$2)))

#----------------------------------------------------------------------
###### $(call up-from,_word_,_list_)
## Return the portion of _list_ following the first occurrence of _word_.
## If _word_ is not in _list_, the empty string/list is returned.
## Examples:
## - `$(call up-from,foo,foo bar baz)` -> `bar baz`
## - `$(call up-from,baz,foo bar baz)` -> ` ` (empty list)
up-from = $(strip $(wordlist $(or $(call index-of,$1,$(-never-matching) $(-never-matching) $2),2147483647),2147483647,$2))


#----------------------------------------------------------------------
###### $(call down-to,_word_,_list_)
## Return the last part of _list_, searching from the end down to but
## excluding the last occurrence of _word_.
## If _word_ is not in _list_, the whole list is returned. This operation is 
## helpful in dissecting e.g. path expressions (´/´ substituted by ´ ´ or ´/ ´)
## Examples:
## - `$(call down-to,baz,foo baz bar baz)` -> ` ` (empty list)
## - `$(call down-to,foo,foo foo bar baz)` -> `bar baz`
down-to = $(call rev-list,$(call up-to,$1,$(call rev-list,$2)))

#----------------------------------------------------------------------
###### $(call down-from,_word_,_list_)
## Return the first part of _list_, searching from the end down to but
## excluding the last occurrence of _word_.
## If _word_ is not in _list_, the empty list is returned. This operation is 
## helpful in dissecting e.g. path expressions (´/´ substituted by ´ ´ or ´/ ´)
## Examples:
## - `$(call down-from,baz,foo baz bar baz)` -> `foo baz bar`
## - `$(call down-from,foo,foo bar baz)` -> ` ` (empty list)
down-from = $(call rev-list,$(call up-from,$1,$(call rev-list,$2)))

#----------------------------------------------------------------------
-num-limit := 64

#----------------------------------------------------------------------
# Some constants for numeric functions
-all-0 := $(call n-list,0,$(-num-limit))
-all-f := $(call n-list,f,$(-num-limit))

#----------------------------------------------------------------------
# Merge two unsigned numbers with possibly different length. Numbers come
# in as lists with most significant digit first, therefore the shorter one
# must be prepended with 0's.
# $1 = 1st number as list
# $2 = 2nd number as list
-shift-merge = $(if $(word $(words $1),$2),$(join $(wordlist $(words _ $1),$(words $2),$(-all-0)) $1,$2),$(join $1,$(wordlist $(words _ $2),$(words $1),$(-all-0)) $2))


#----------------------------------------------------------------------
# Merge two sign-extended numbers. This function is only implemented for
# hexadecimal numbers.
#
# $1 = 1st number as list
# $2 = 2nd number as list
# $3 = 1st sign (- or empty)
# $4 = 2nd sign (- or empty)
-sgnxt-merge = $(if $(word $(words $1),$2),$(join $(wordlist $(words _ $1),$(words $2),$(-all-$(subst 0-,f,0$3))) $1,$2),$(join $1,$(wordlist $(words _ $2),$(words $1),$(-all-$(subst 0-,f,0$4))) $2))



#----------------------------------------------------------------------
# The following elisp code generates the tables for addition, subtraction
# and multiplication for the selected base. Currently the bases 8,10 and
# 16 are in use in gmtt.
# (dolist (base (list 8 10 16))
#    (let* ((i base)
#        (j 0)
#        (add-pairs "")
#        (carry-pairs "")
#        (sub-pairs "")
#        (borrow-pairs "")
#        (mul-pairs "")
#        (mulcarry-pairs ""))
#       (while (< 0 i)
#          (setq i (1- i)
#                add-pairs (concat add-pairs (format " %x:=%x" i i))
#                carry-pairs (concat carry-pairs (format " %x:=" i))
#                sub-pairs (concat sub-pairs (format " %x:=%x" i i))
#                borrow-pairs (concat borrow-pairs (format " %x:=" i))
#                mul-pairs (concat mul-pairs (format " %x:=0" i))
#                mulcarry-pairs (concat mulcarry-pairs (format " %x:=" i)))
#          (setq j base)
#          (while (< 0 j)
#            (setq j (1- j)
#                  add-pairs (concat add-pairs (format " %x%x:=%x" i j (% (+ i j ) base)))
#                  carry-pairs (concat carry-pairs (format " %x%x:=%s" i j (if (<= base (+ i j)) "y" "")))
#                  sub-pairs (concat sub-pairs (format " %x%x:=%x" i j (% (- (+ base i) j ) base)))
#                  borrow-pairs (concat borrow-pairs (format " %x%x:=%s" i j (if (< i j) "z" "")))
#                  mul-pairs (concat mul-pairs (format " %x%x:=%x" i j (% (* i j) base)))
#                  mulcarry-pairs (concat mulcarry-pairs (format " %x%x:=%s" i j (if (> base (* i j)) "" (format "%x" (/ (* i j) base))))))))
#       (insert "-add" (int-to-string base) "-pairs :=" add-pairs "\n"
#               "-carry" (int-to-string base) "-pairs :=" carry-pairs "\n"
#               "$(foreach i,$(-add" (int-to-string base) "-pairs),$(eval -ad" (int-to-string base) "-$(i)))\n"
#               "$(foreach i,$(-carry" (int-to-string base) "-pairs),$(eval -cy" (int-to-string base) "-$(i)))\n"
#                "-sub" (int-to-string base) "-pairs :=" sub-pairs "\n"
#               "-borrow" (int-to-string base) "-pairs :=" borrow-pairs "\n"
#               "$(foreach i,$(-sub" (int-to-string base) "-pairs),$(eval -sb" (int-to-string base) "-$(i)))\n"
#               "$(foreach i,$(-borrow" (int-to-string base) "-pairs),$(eval -bw" (int-to-string base) "-$(i)))\n"
#               "-mul" (int-to-string base) "-pairs :=" mul-pairs "\n"
#               "-mulcarry" (int-to-string base) "-pairs :=" mulcarry-pairs "\n"
#               "$(foreach i,$(-mul" (int-to-string base) "-pairs),$(eval -ml" (int-to-string base) "-$(i)))\n"
#               "$(foreach i,$(-mulcarry" (int-to-string base) "-pairs),$(eval -mc" (int-to-string base) "-$(i)))\n"
#               "-add" (int-to-string base) " = $(call --add" (int-to-string base) ",$(foreach pair,$1,$(-cy" (int-to-string base)
#                     "-$(pair))$(-ad" (int-to-string base) "-$(pair))))\n"
#               "--add" (int-to-string base) " = $(if $(findstring y,$1),$(call --add" (int-to-string base)
#                     ",$(foreach pair,$(subst $(space)y,1 ,$(subst " (format "%x %x" (1- base) (1- base)) " y,y0 0 , $1)),$(-cy"
#                     (int-to-string base) "-$(pair))$(-ad" (int-to-string base) "-$(pair)))),$1)\n"
#               "-sub" (int-to-string base) " = $(call --sub" (int-to-string base) ",$(foreach pair,$1,$(-bw" (int-to-string base)
#                     "-$(pair))$(-sb" (int-to-string base) "-$(pair))))\n"
#               "--sub" (int-to-string base) " = $(if $(findstring z,$1),$(call --sub" (int-to-string base)
#                     ",$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z" (format "%x %x" (1- base) (1- base)) " , $1)),$(-bw"
#                     (int-to-string base) "-$(pair))$(-sb" (int-to-string base) "-$(pair)))),$1)\n"
#               "-mul" (int-to-string base) " = $(if $(word $(words $1),$2),$(call --mul" (int-to-string base)
#                     ",$1,0 $2),$(call --mul" (int-to-string base) ",$2,0 $1))\n"
#               "--mul" (int-to-string base) " = $(if $1,$(call -add" (int-to-string base) ",$(join $(call --mul"
#                     (int-to-string base) "-$(firstword $1),$2),0 $(call --mul" (int-to-string base) ",$(wordlist 2,$(-num-limit),$1),$2))))\n"
#               "--mul" (int-to-string base) "-0 = $(patsubst %,0,$1)\n"
#               "--mul" (int-to-string base) "-1 = $1\n")
#               (setq i 2)
#               (while (< i base)
#                   (insert (format "--mul%d-%x = $(call -add%d,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc%d-%x$(i)) $(-ml%d-%x$(i)))))\n" base i base base i base i))
#                   (setq i (1+ i)))))

-add8-pairs := 7:=7 77:=6 76:=5 75:=4 74:=3 73:=2 72:=1 71:=0 70:=7 6:=6 67:=5 66:=4 65:=3 64:=2 63:=1 62:=0 61:=7 60:=6 5:=5 57:=4 56:=3 55:=2 54:=1 53:=0 52:=7 51:=6 50:=5 4:=4 47:=3 46:=2 45:=1 44:=0 43:=7 42:=6 41:=5 40:=4 3:=3 37:=2 36:=1 35:=0 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 27:=1 26:=0 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 17:=0 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
-carry8-pairs := 7:= 77:=y 76:=y 75:=y 74:=y 73:=y 72:=y 71:=y 70:= 6:= 67:=y 66:=y 65:=y 64:=y 63:=y 62:=y 61:= 60:= 5:= 57:=y 56:=y 55:=y 54:=y 53:=y 52:= 51:= 50:= 4:= 47:=y 46:=y 45:=y 44:=y 43:= 42:= 41:= 40:= 3:= 37:=y 36:=y 35:=y 34:= 33:= 32:= 31:= 30:= 2:= 27:=y 26:=y 25:= 24:= 23:= 22:= 21:= 20:= 1:= 17:=y 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-add8-pairs),$(eval -ad8-$(i)))
$(foreach i,$(-carry8-pairs),$(eval -cy8-$(i)))
-sub8-pairs := 7:=7 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 67:=7 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 57:=6 56:=7 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 47:=5 46:=6 45:=7 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 37:=4 36:=5 35:=6 34:=7 33:=0 32:=1 31:=2 30:=3 2:=2 27:=3 26:=4 25:=5 24:=6 23:=7 22:=0 21:=1 20:=2 1:=1 17:=2 16:=3 15:=4 14:=5 13:=6 12:=7 11:=0 10:=1 0:=0 07:=1 06:=2 05:=3 04:=4 03:=5 02:=6 01:=7 00:=0
-borrow8-pairs := 7:= 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(-sub8-pairs),$(eval -sb8-$(i)))
$(foreach i,$(-borrow8-pairs),$(eval -bw8-$(i)))
-mul8-pairs := 7:=0 77:=1 76:=2 75:=3 74:=4 73:=5 72:=6 71:=7 70:=0 6:=0 67:=2 66:=4 65:=6 64:=0 63:=2 62:=4 61:=6 60:=0 5:=0 57:=3 56:=6 55:=1 54:=4 53:=7 52:=2 51:=5 50:=0 4:=0 47:=4 46:=0 45:=4 44:=0 43:=4 42:=0 41:=4 40:=0 3:=0 37:=5 36:=2 35:=7 34:=4 33:=1 32:=6 31:=3 30:=0 2:=0 27:=6 26:=4 25:=2 24:=0 23:=6 22:=4 21:=2 20:=0 1:=0 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
-mulcarry8-pairs := 7:= 77:=6 76:=5 75:=4 74:=3 73:=2 72:=1 71:= 70:= 6:= 67:=5 66:=4 65:=3 64:=3 63:=2 62:=1 61:= 60:= 5:= 57:=4 56:=3 55:=3 54:=2 53:=1 52:=1 51:= 50:= 4:= 47:=3 46:=3 45:=2 44:=2 43:=1 42:=1 41:= 40:= 3:= 37:=2 36:=2 35:=1 34:=1 33:=1 32:= 31:= 30:= 2:= 27:=1 26:=1 25:=1 24:=1 23:= 22:= 21:= 20:= 1:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-mul8-pairs),$(eval -ml8-$(i)))
$(foreach i,$(-mulcarry8-pairs),$(eval -mc8-$(i)))
-add8 = $(call --add8,$(foreach pair,$1,$(-cy8-$(pair))$(-ad8-$(pair))))
--add8 = $(if $(findstring y,$1),$(call --add8,$(foreach pair,$(subst $(space)y,1 ,$(subst 7 7 y,y0 0 , $1)),$(-cy8-$(pair))$(-ad8-$(pair)))),$1)
-sub8 = $(call --sub8,$(foreach pair,$1,$(-bw8-$(pair))$(-sb8-$(pair))))
--sub8 = $(if $(findstring z,$1),$(call --sub8,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z7 7 , $1)),$(-bw8-$(pair))$(-sb8-$(pair)))),$1)
-mul8 = $(if $(word $(words $1),$2),$(call --mul8,$1,0 $2),$(call --mul8,$2,0 $1))
--mul8 = $(if $1,$(call -add8,$(join $(call --mul8-$(firstword $1),$2),0 $(call --mul8,$(wordlist 2,$(-num-limit),$1),$2))))
--mul8-0 = $(patsubst %,0,$1)
--mul8-1 = $1
--mul8-2 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-2$(i)) $(-ml8-2$(i)))))
--mul8-3 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-3$(i)) $(-ml8-3$(i)))))
--mul8-4 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-4$(i)) $(-ml8-4$(i)))))
--mul8-5 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-5$(i)) $(-ml8-5$(i)))))
--mul8-6 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-6$(i)) $(-ml8-6$(i)))))
--mul8-7 = $(call -add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc8-7$(i)) $(-ml8-7$(i)))))
-add10-pairs := 9:=9 99:=8 98:=7 97:=6 96:=5 95:=4 94:=3 93:=2 92:=1 91:=0 90:=9 8:=8 89:=7 88:=6 87:=5 86:=4 85:=3 84:=2 83:=1 82:=0 81:=9 80:=8 7:=7 79:=6 78:=5 77:=4 76:=3 75:=2 74:=1 73:=0 72:=9 71:=8 70:=7 6:=6 69:=5 68:=4 67:=3 66:=2 65:=1 64:=0 63:=9 62:=8 61:=7 60:=6 5:=5 59:=4 58:=3 57:=2 56:=1 55:=0 54:=9 53:=8 52:=7 51:=6 50:=5 4:=4 49:=3 48:=2 47:=1 46:=0 45:=9 44:=8 43:=7 42:=6 41:=5 40:=4 3:=3 39:=2 38:=1 37:=0 36:=9 35:=8 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 29:=1 28:=0 27:=9 26:=8 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 19:=0 18:=9 17:=8 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 09:=9 08:=8 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
-carry10-pairs := 9:= 99:=y 98:=y 97:=y 96:=y 95:=y 94:=y 93:=y 92:=y 91:=y 90:= 8:= 89:=y 88:=y 87:=y 86:=y 85:=y 84:=y 83:=y 82:=y 81:= 80:= 7:= 79:=y 78:=y 77:=y 76:=y 75:=y 74:=y 73:=y 72:= 71:= 70:= 6:= 69:=y 68:=y 67:=y 66:=y 65:=y 64:=y 63:= 62:= 61:= 60:= 5:= 59:=y 58:=y 57:=y 56:=y 55:=y 54:= 53:= 52:= 51:= 50:= 4:= 49:=y 48:=y 47:=y 46:=y 45:= 44:= 43:= 42:= 41:= 40:= 3:= 39:=y 38:=y 37:=y 36:= 35:= 34:= 33:= 32:= 31:= 30:= 2:= 29:=y 28:=y 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 19:=y 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-add10-pairs),$(eval -ad10-$(i)))
$(foreach i,$(-carry10-pairs),$(eval -cy10-$(i)))
-sub10-pairs := 9:=9 99:=0 98:=1 97:=2 96:=3 95:=4 94:=5 93:=6 92:=7 91:=8 90:=9 8:=8 89:=9 88:=0 87:=1 86:=2 85:=3 84:=4 83:=5 82:=6 81:=7 80:=8 7:=7 79:=8 78:=9 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 69:=7 68:=8 67:=9 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 59:=6 58:=7 57:=8 56:=9 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 49:=5 48:=6 47:=7 46:=8 45:=9 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 39:=4 38:=5 37:=6 36:=7 35:=8 34:=9 33:=0 32:=1 31:=2 30:=3 2:=2 29:=3 28:=4 27:=5 26:=6 25:=7 24:=8 23:=9 22:=0 21:=1 20:=2 1:=1 19:=2 18:=3 17:=4 16:=5 15:=6 14:=7 13:=8 12:=9 11:=0 10:=1 0:=0 09:=1 08:=2 07:=3 06:=4 05:=5 04:=6 03:=7 02:=8 01:=9 00:=0
-borrow10-pairs := 9:= 99:= 98:= 97:= 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 89:=z 88:= 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 79:=z 78:=z 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 69:=z 68:=z 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 59:=z 58:=z 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 49:=z 48:=z 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 39:=z 38:=z 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 29:=z 28:=z 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 19:=z 18:=z 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 09:=z 08:=z 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(-sub10-pairs),$(eval -sb10-$(i)))
$(foreach i,$(-borrow10-pairs),$(eval -bw10-$(i)))
-mul10-pairs := 9:=0 99:=1 98:=2 97:=3 96:=4 95:=5 94:=6 93:=7 92:=8 91:=9 90:=0 8:=0 89:=2 88:=4 87:=6 86:=8 85:=0 84:=2 83:=4 82:=6 81:=8 80:=0 7:=0 79:=3 78:=6 77:=9 76:=2 75:=5 74:=8 73:=1 72:=4 71:=7 70:=0 6:=0 69:=4 68:=8 67:=2 66:=6 65:=0 64:=4 63:=8 62:=2 61:=6 60:=0 5:=0 59:=5 58:=0 57:=5 56:=0 55:=5 54:=0 53:=5 52:=0 51:=5 50:=0 4:=0 49:=6 48:=2 47:=8 46:=4 45:=0 44:=6 43:=2 42:=8 41:=4 40:=0 3:=0 39:=7 38:=4 37:=1 36:=8 35:=5 34:=2 33:=9 32:=6 31:=3 30:=0 2:=0 29:=8 28:=6 27:=4 26:=2 25:=0 24:=8 23:=6 22:=4 21:=2 20:=0 1:=0 19:=9 18:=8 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 09:=0 08:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
-mulcarry10-pairs := 9:= 99:=8 98:=7 97:=6 96:=5 95:=4 94:=3 93:=2 92:=1 91:= 90:= 8:= 89:=7 88:=6 87:=5 86:=4 85:=4 84:=3 83:=2 82:=1 81:= 80:= 7:= 79:=6 78:=5 77:=4 76:=4 75:=3 74:=2 73:=2 72:=1 71:= 70:= 6:= 69:=5 68:=4 67:=4 66:=3 65:=3 64:=2 63:=1 62:=1 61:= 60:= 5:= 59:=4 58:=4 57:=3 56:=3 55:=2 54:=2 53:=1 52:=1 51:= 50:= 4:= 49:=3 48:=3 47:=2 46:=2 45:=2 44:=1 43:=1 42:= 41:= 40:= 3:= 39:=2 38:=2 37:=2 36:=1 35:=1 34:=1 33:= 32:= 31:= 30:= 2:= 29:=1 28:=1 27:=1 26:=1 25:=1 24:= 23:= 22:= 21:= 20:= 1:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-mul10-pairs),$(eval -ml10-$(i)))
$(foreach i,$(-mulcarry10-pairs),$(eval -mc10-$(i)))
-add10 = $(call --add10,$(foreach pair,$1,$(-cy10-$(pair))$(-ad10-$(pair))))
--add10 = $(if $(findstring y,$1),$(call --add10,$(foreach pair,$(subst $(space)y,1 ,$(subst 9 9 y,y0 0 , $1)),$(-cy10-$(pair))$(-ad10-$(pair)))),$1)
-sub10 = $(call --sub10,$(foreach pair,$1,$(-bw10-$(pair))$(-sb10-$(pair))))
--sub10 = $(if $(findstring z,$1),$(call --sub10,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z9 9 , $1)),$(-bw10-$(pair))$(-sb10-$(pair)))),$1)
-mul10 = $(if $(word $(words $1),$2),$(call --mul10,$1,0 $2),$(call --mul10,$2,0 $1))
--mul10 = $(if $1,$(call -add10,$(join $(call --mul10-$(firstword $1),$2),0 $(call --mul10,$(wordlist 2,$(-num-limit),$1),$2))))
--mul10-0 = $(patsubst %,0,$1)
--mul10-1 = $1
--mul10-2 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-2$(i)) $(-ml10-2$(i)))))
--mul10-3 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-3$(i)) $(-ml10-3$(i)))))
--mul10-4 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-4$(i)) $(-ml10-4$(i)))))
--mul10-5 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-5$(i)) $(-ml10-5$(i)))))
--mul10-6 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-6$(i)) $(-ml10-6$(i)))))
--mul10-7 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-7$(i)) $(-ml10-7$(i)))))
--mul10-8 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-8$(i)) $(-ml10-8$(i)))))
--mul10-9 = $(call -add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc10-9$(i)) $(-ml10-9$(i)))))
-add16-pairs := f:=f ff:=e fe:=d fd:=c fc:=b fb:=a fa:=9 f9:=8 f8:=7 f7:=6 f6:=5 f5:=4 f4:=3 f3:=2 f2:=1 f1:=0 f0:=f e:=e ef:=d ee:=c ed:=b ec:=a eb:=9 ea:=8 e9:=7 e8:=6 e7:=5 e6:=4 e5:=3 e4:=2 e3:=1 e2:=0 e1:=f e0:=e d:=d df:=c de:=b dd:=a dc:=9 db:=8 da:=7 d9:=6 d8:=5 d7:=4 d6:=3 d5:=2 d4:=1 d3:=0 d2:=f d1:=e d0:=d c:=c cf:=b ce:=a cd:=9 cc:=8 cb:=7 ca:=6 c9:=5 c8:=4 c7:=3 c6:=2 c5:=1 c4:=0 c3:=f c2:=e c1:=d c0:=c b:=b bf:=a be:=9 bd:=8 bc:=7 bb:=6 ba:=5 b9:=4 b8:=3 b7:=2 b6:=1 b5:=0 b4:=f b3:=e b2:=d b1:=c b0:=b a:=a af:=9 ae:=8 ad:=7 ac:=6 ab:=5 aa:=4 a9:=3 a8:=2 a7:=1 a6:=0 a5:=f a4:=e a3:=d a2:=c a1:=b a0:=a 9:=9 9f:=8 9e:=7 9d:=6 9c:=5 9b:=4 9a:=3 99:=2 98:=1 97:=0 96:=f 95:=e 94:=d 93:=c 92:=b 91:=a 90:=9 8:=8 8f:=7 8e:=6 8d:=5 8c:=4 8b:=3 8a:=2 89:=1 88:=0 87:=f 86:=e 85:=d 84:=c 83:=b 82:=a 81:=9 80:=8 7:=7 7f:=6 7e:=5 7d:=4 7c:=3 7b:=2 7a:=1 79:=0 78:=f 77:=e 76:=d 75:=c 74:=b 73:=a 72:=9 71:=8 70:=7 6:=6 6f:=5 6e:=4 6d:=3 6c:=2 6b:=1 6a:=0 69:=f 68:=e 67:=d 66:=c 65:=b 64:=a 63:=9 62:=8 61:=7 60:=6 5:=5 5f:=4 5e:=3 5d:=2 5c:=1 5b:=0 5a:=f 59:=e 58:=d 57:=c 56:=b 55:=a 54:=9 53:=8 52:=7 51:=6 50:=5 4:=4 4f:=3 4e:=2 4d:=1 4c:=0 4b:=f 4a:=e 49:=d 48:=c 47:=b 46:=a 45:=9 44:=8 43:=7 42:=6 41:=5 40:=4 3:=3 3f:=2 3e:=1 3d:=0 3c:=f 3b:=e 3a:=d 39:=c 38:=b 37:=a 36:=9 35:=8 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 2f:=1 2e:=0 2d:=f 2c:=e 2b:=d 2a:=c 29:=b 28:=a 27:=9 26:=8 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 1f:=0 1e:=f 1d:=e 1c:=d 1b:=c 1a:=b 19:=a 18:=9 17:=8 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 0f:=f 0e:=e 0d:=d 0c:=c 0b:=b 0a:=a 09:=9 08:=8 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
-carry16-pairs := f:= ff:=y fe:=y fd:=y fc:=y fb:=y fa:=y f9:=y f8:=y f7:=y f6:=y f5:=y f4:=y f3:=y f2:=y f1:=y f0:= e:= ef:=y ee:=y ed:=y ec:=y eb:=y ea:=y e9:=y e8:=y e7:=y e6:=y e5:=y e4:=y e3:=y e2:=y e1:= e0:= d:= df:=y de:=y dd:=y dc:=y db:=y da:=y d9:=y d8:=y d7:=y d6:=y d5:=y d4:=y d3:=y d2:= d1:= d0:= c:= cf:=y ce:=y cd:=y cc:=y cb:=y ca:=y c9:=y c8:=y c7:=y c6:=y c5:=y c4:=y c3:= c2:= c1:= c0:= b:= bf:=y be:=y bd:=y bc:=y bb:=y ba:=y b9:=y b8:=y b7:=y b6:=y b5:=y b4:= b3:= b2:= b1:= b0:= a:= af:=y ae:=y ad:=y ac:=y ab:=y aa:=y a9:=y a8:=y a7:=y a6:=y a5:= a4:= a3:= a2:= a1:= a0:= 9:= 9f:=y 9e:=y 9d:=y 9c:=y 9b:=y 9a:=y 99:=y 98:=y 97:=y 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 8f:=y 8e:=y 8d:=y 8c:=y 8b:=y 8a:=y 89:=y 88:=y 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 7f:=y 7e:=y 7d:=y 7c:=y 7b:=y 7a:=y 79:=y 78:= 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 6f:=y 6e:=y 6d:=y 6c:=y 6b:=y 6a:=y 69:= 68:= 67:= 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 5f:=y 5e:=y 5d:=y 5c:=y 5b:=y 5a:= 59:= 58:= 57:= 56:= 55:= 54:= 53:= 52:= 51:= 50:= 4:= 4f:=y 4e:=y 4d:=y 4c:=y 4b:= 4a:= 49:= 48:= 47:= 46:= 45:= 44:= 43:= 42:= 41:= 40:= 3:= 3f:=y 3e:=y 3d:=y 3c:= 3b:= 3a:= 39:= 38:= 37:= 36:= 35:= 34:= 33:= 32:= 31:= 30:= 2:= 2f:=y 2e:=y 2d:= 2c:= 2b:= 2a:= 29:= 28:= 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 1f:=y 1e:= 1d:= 1c:= 1b:= 1a:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 0f:= 0e:= 0d:= 0c:= 0b:= 0a:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-add16-pairs),$(eval -ad16-$(i)))
$(foreach i,$(-carry16-pairs),$(eval -cy16-$(i)))
-sub16-pairs := f:=f ff:=0 fe:=1 fd:=2 fc:=3 fb:=4 fa:=5 f9:=6 f8:=7 f7:=8 f6:=9 f5:=a f4:=b f3:=c f2:=d f1:=e f0:=f e:=e ef:=f ee:=0 ed:=1 ec:=2 eb:=3 ea:=4 e9:=5 e8:=6 e7:=7 e6:=8 e5:=9 e4:=a e3:=b e2:=c e1:=d e0:=e d:=d df:=e de:=f dd:=0 dc:=1 db:=2 da:=3 d9:=4 d8:=5 d7:=6 d6:=7 d5:=8 d4:=9 d3:=a d2:=b d1:=c d0:=d c:=c cf:=d ce:=e cd:=f cc:=0 cb:=1 ca:=2 c9:=3 c8:=4 c7:=5 c6:=6 c5:=7 c4:=8 c3:=9 c2:=a c1:=b c0:=c b:=b bf:=c be:=d bd:=e bc:=f bb:=0 ba:=1 b9:=2 b8:=3 b7:=4 b6:=5 b5:=6 b4:=7 b3:=8 b2:=9 b1:=a b0:=b a:=a af:=b ae:=c ad:=d ac:=e ab:=f aa:=0 a9:=1 a8:=2 a7:=3 a6:=4 a5:=5 a4:=6 a3:=7 a2:=8 a1:=9 a0:=a 9:=9 9f:=a 9e:=b 9d:=c 9c:=d 9b:=e 9a:=f 99:=0 98:=1 97:=2 96:=3 95:=4 94:=5 93:=6 92:=7 91:=8 90:=9 8:=8 8f:=9 8e:=a 8d:=b 8c:=c 8b:=d 8a:=e 89:=f 88:=0 87:=1 86:=2 85:=3 84:=4 83:=5 82:=6 81:=7 80:=8 7:=7 7f:=8 7e:=9 7d:=a 7c:=b 7b:=c 7a:=d 79:=e 78:=f 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 6f:=7 6e:=8 6d:=9 6c:=a 6b:=b 6a:=c 69:=d 68:=e 67:=f 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 5f:=6 5e:=7 5d:=8 5c:=9 5b:=a 5a:=b 59:=c 58:=d 57:=e 56:=f 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 4f:=5 4e:=6 4d:=7 4c:=8 4b:=9 4a:=a 49:=b 48:=c 47:=d 46:=e 45:=f 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 3f:=4 3e:=5 3d:=6 3c:=7 3b:=8 3a:=9 39:=a 38:=b 37:=c 36:=d 35:=e 34:=f 33:=0 32:=1 31:=2 30:=3 2:=2 2f:=3 2e:=4 2d:=5 2c:=6 2b:=7 2a:=8 29:=9 28:=a 27:=b 26:=c 25:=d 24:=e 23:=f 22:=0 21:=1 20:=2 1:=1 1f:=2 1e:=3 1d:=4 1c:=5 1b:=6 1a:=7 19:=8 18:=9 17:=a 16:=b 15:=c 14:=d 13:=e 12:=f 11:=0 10:=1 0:=0 0f:=1 0e:=2 0d:=3 0c:=4 0b:=5 0a:=6 09:=7 08:=8 07:=9 06:=a 05:=b 04:=c 03:=d 02:=e 01:=f 00:=0
-borrow16-pairs := f:= ff:= fe:= fd:= fc:= fb:= fa:= f9:= f8:= f7:= f6:= f5:= f4:= f3:= f2:= f1:= f0:= e:= ef:=z ee:= ed:= ec:= eb:= ea:= e9:= e8:= e7:= e6:= e5:= e4:= e3:= e2:= e1:= e0:= d:= df:=z de:=z dd:= dc:= db:= da:= d9:= d8:= d7:= d6:= d5:= d4:= d3:= d2:= d1:= d0:= c:= cf:=z ce:=z cd:=z cc:= cb:= ca:= c9:= c8:= c7:= c6:= c5:= c4:= c3:= c2:= c1:= c0:= b:= bf:=z be:=z bd:=z bc:=z bb:= ba:= b9:= b8:= b7:= b6:= b5:= b4:= b3:= b2:= b1:= b0:= a:= af:=z ae:=z ad:=z ac:=z ab:=z aa:= a9:= a8:= a7:= a6:= a5:= a4:= a3:= a2:= a1:= a0:= 9:= 9f:=z 9e:=z 9d:=z 9c:=z 9b:=z 9a:=z 99:= 98:= 97:= 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 8f:=z 8e:=z 8d:=z 8c:=z 8b:=z 8a:=z 89:=z 88:= 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 7f:=z 7e:=z 7d:=z 7c:=z 7b:=z 7a:=z 79:=z 78:=z 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 6f:=z 6e:=z 6d:=z 6c:=z 6b:=z 6a:=z 69:=z 68:=z 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 5f:=z 5e:=z 5d:=z 5c:=z 5b:=z 5a:=z 59:=z 58:=z 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 4f:=z 4e:=z 4d:=z 4c:=z 4b:=z 4a:=z 49:=z 48:=z 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 3f:=z 3e:=z 3d:=z 3c:=z 3b:=z 3a:=z 39:=z 38:=z 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 2f:=z 2e:=z 2d:=z 2c:=z 2b:=z 2a:=z 29:=z 28:=z 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 1f:=z 1e:=z 1d:=z 1c:=z 1b:=z 1a:=z 19:=z 18:=z 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 0f:=z 0e:=z 0d:=z 0c:=z 0b:=z 0a:=z 09:=z 08:=z 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(-sub16-pairs),$(eval -sb16-$(i)))
$(foreach i,$(-borrow16-pairs),$(eval -bw16-$(i)))
-mul16-pairs := f:=0 ff:=1 fe:=2 fd:=3 fc:=4 fb:=5 fa:=6 f9:=7 f8:=8 f7:=9 f6:=a f5:=b f4:=c f3:=d f2:=e f1:=f f0:=0 e:=0 ef:=2 ee:=4 ed:=6 ec:=8 eb:=a ea:=c e9:=e e8:=0 e7:=2 e6:=4 e5:=6 e4:=8 e3:=a e2:=c e1:=e e0:=0 d:=0 df:=3 de:=6 dd:=9 dc:=c db:=f da:=2 d9:=5 d8:=8 d7:=b d6:=e d5:=1 d4:=4 d3:=7 d2:=a d1:=d d0:=0 c:=0 cf:=4 ce:=8 cd:=c cc:=0 cb:=4 ca:=8 c9:=c c8:=0 c7:=4 c6:=8 c5:=c c4:=0 c3:=4 c2:=8 c1:=c c0:=0 b:=0 bf:=5 be:=a bd:=f bc:=4 bb:=9 ba:=e b9:=3 b8:=8 b7:=d b6:=2 b5:=7 b4:=c b3:=1 b2:=6 b1:=b b0:=0 a:=0 af:=6 ae:=c ad:=2 ac:=8 ab:=e aa:=4 a9:=a a8:=0 a7:=6 a6:=c a5:=2 a4:=8 a3:=e a2:=4 a1:=a a0:=0 9:=0 9f:=7 9e:=e 9d:=5 9c:=c 9b:=3 9a:=a 99:=1 98:=8 97:=f 96:=6 95:=d 94:=4 93:=b 92:=2 91:=9 90:=0 8:=0 8f:=8 8e:=0 8d:=8 8c:=0 8b:=8 8a:=0 89:=8 88:=0 87:=8 86:=0 85:=8 84:=0 83:=8 82:=0 81:=8 80:=0 7:=0 7f:=9 7e:=2 7d:=b 7c:=4 7b:=d 7a:=6 79:=f 78:=8 77:=1 76:=a 75:=3 74:=c 73:=5 72:=e 71:=7 70:=0 6:=0 6f:=a 6e:=4 6d:=e 6c:=8 6b:=2 6a:=c 69:=6 68:=0 67:=a 66:=4 65:=e 64:=8 63:=2 62:=c 61:=6 60:=0 5:=0 5f:=b 5e:=6 5d:=1 5c:=c 5b:=7 5a:=2 59:=d 58:=8 57:=3 56:=e 55:=9 54:=4 53:=f 52:=a 51:=5 50:=0 4:=0 4f:=c 4e:=8 4d:=4 4c:=0 4b:=c 4a:=8 49:=4 48:=0 47:=c 46:=8 45:=4 44:=0 43:=c 42:=8 41:=4 40:=0 3:=0 3f:=d 3e:=a 3d:=7 3c:=4 3b:=1 3a:=e 39:=b 38:=8 37:=5 36:=2 35:=f 34:=c 33:=9 32:=6 31:=3 30:=0 2:=0 2f:=e 2e:=c 2d:=a 2c:=8 2b:=6 2a:=4 29:=2 28:=0 27:=e 26:=c 25:=a 24:=8 23:=6 22:=4 21:=2 20:=0 1:=0 1f:=f 1e:=e 1d:=d 1c:=c 1b:=b 1a:=a 19:=9 18:=8 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 0f:=0 0e:=0 0d:=0 0c:=0 0b:=0 0a:=0 09:=0 08:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
-mulcarry16-pairs := f:= ff:=e fe:=d fd:=c fc:=b fb:=a fa:=9 f9:=8 f8:=7 f7:=6 f6:=5 f5:=4 f4:=3 f3:=2 f2:=1 f1:= f0:= e:= ef:=d ee:=c ed:=b ec:=a eb:=9 ea:=8 e9:=7 e8:=7 e7:=6 e6:=5 e5:=4 e4:=3 e3:=2 e2:=1 e1:= e0:= d:= df:=c de:=b dd:=a dc:=9 db:=8 da:=8 d9:=7 d8:=6 d7:=5 d6:=4 d5:=4 d4:=3 d3:=2 d2:=1 d1:= d0:= c:= cf:=b ce:=a cd:=9 cc:=9 cb:=8 ca:=7 c9:=6 c8:=6 c7:=5 c6:=4 c5:=3 c4:=3 c3:=2 c2:=1 c1:= c0:= b:= bf:=a be:=9 bd:=8 bc:=8 bb:=7 ba:=6 b9:=6 b8:=5 b7:=4 b6:=4 b5:=3 b4:=2 b3:=2 b2:=1 b1:= b0:= a:= af:=9 ae:=8 ad:=8 ac:=7 ab:=6 aa:=6 a9:=5 a8:=5 a7:=4 a6:=3 a5:=3 a4:=2 a3:=1 a2:=1 a1:= a0:= 9:= 9f:=8 9e:=7 9d:=7 9c:=6 9b:=6 9a:=5 99:=5 98:=4 97:=3 96:=3 95:=2 94:=2 93:=1 92:=1 91:= 90:= 8:= 8f:=7 8e:=7 8d:=6 8c:=6 8b:=5 8a:=5 89:=4 88:=4 87:=3 86:=3 85:=2 84:=2 83:=1 82:=1 81:= 80:= 7:= 7f:=6 7e:=6 7d:=5 7c:=5 7b:=4 7a:=4 79:=3 78:=3 77:=3 76:=2 75:=2 74:=1 73:=1 72:= 71:= 70:= 6:= 6f:=5 6e:=5 6d:=4 6c:=4 6b:=4 6a:=3 69:=3 68:=3 67:=2 66:=2 65:=1 64:=1 63:=1 62:= 61:= 60:= 5:= 5f:=4 5e:=4 5d:=4 5c:=3 5b:=3 5a:=3 59:=2 58:=2 57:=2 56:=1 55:=1 54:=1 53:= 52:= 51:= 50:= 4:= 4f:=3 4e:=3 4d:=3 4c:=3 4b:=2 4a:=2 49:=2 48:=2 47:=1 46:=1 45:=1 44:=1 43:= 42:= 41:= 40:= 3:= 3f:=2 3e:=2 3d:=2 3c:=2 3b:=2 3a:=1 39:=1 38:=1 37:=1 36:=1 35:= 34:= 33:= 32:= 31:= 30:= 2:= 2f:=1 2e:=1 2d:=1 2c:=1 2b:=1 2a:=1 29:=1 28:=1 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 1f:= 1e:= 1d:= 1c:= 1b:= 1a:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 0f:= 0e:= 0d:= 0c:= 0b:= 0a:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(-mul16-pairs),$(eval -ml16-$(i)))
$(foreach i,$(-mulcarry16-pairs),$(eval -mc16-$(i)))
-add16 = $(call --add16,$(foreach pair,$1,$(-cy16-$(pair))$(-ad16-$(pair))))
--add16 = $(if $(findstring y,$1),$(call --add16,$(foreach pair,$(subst $(space)y,1 ,$(subst f f y,y0 0 , $1)),$(-cy16-$(pair))$(-ad16-$(pair)))),$1)
-sub16 = $(call --sub16,$(foreach pair,$1,$(-bw16-$(pair))$(-sb16-$(pair))))
--sub16 = $(if $(findstring z,$1),$(call --sub16,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,zf f , $1)),$(-bw16-$(pair))$(-sb16-$(pair)))),$1)
-mul16 = $(if $(word $(words $1),$2),$(call --mul16,$1,0 $2),$(call --mul16,$2,0 $1))
--mul16 = $(if $1,$(call -add16,$(join $(call --mul16-$(firstword $1),$2),0 $(call --mul16,$(wordlist 2,$(-num-limit),$1),$2))))
--mul16-0 = $(patsubst %,0,$1)
--mul16-1 = $1
--mul16-2 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-2$(i)) $(-ml16-2$(i)))))
--mul16-3 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-3$(i)) $(-ml16-3$(i)))))
--mul16-4 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-4$(i)) $(-ml16-4$(i)))))
--mul16-5 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-5$(i)) $(-ml16-5$(i)))))
--mul16-6 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-6$(i)) $(-ml16-6$(i)))))
--mul16-7 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-7$(i)) $(-ml16-7$(i)))))
--mul16-8 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-8$(i)) $(-ml16-8$(i)))))
--mul16-9 = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-9$(i)) $(-ml16-9$(i)))))
--mul16-a = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-a$(i)) $(-ml16-a$(i)))))
--mul16-b = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-b$(i)) $(-ml16-b$(i)))))
--mul16-c = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-c$(i)) $(-ml16-c$(i)))))
--mul16-d = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-d$(i)) $(-ml16-d$(i)))))
--mul16-e = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-e$(i)) $(-ml16-e$(i)))))
--mul16-f = $(call -add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(-mc16-f$(i)) $(-ml16-f$(i)))))


# Remove all leading 0's
# $1 = numeric literal with leading 0's
-nrmlz = $(if $1,$(if $(patsubst 00%,,$1),$(if $(patsubst 0%,,$1),$1,$(call -nrmlz,$(patsubst 0%,%,$1))),$(call -nrmlz,$(patsubst 00%,%,$1))),0)


# Compare two unsigned numbers (given as lists)
# $1 = first number as list
# $2 = second number as list
-ucmp = $(if $(word $(words $2),$1),$(if $(word $(words $1),$2),$(call --ucmp,$(firstword $(filter-out 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff,$(join $1,$2)))),>),<)
--ucmp = $(if $1,$(if $(-bw16-$1),<,>),=)



# Split a numeric literal into a base id (8,10 or 16) and the digits without base indicator
# and also strip the sign
# Example: -analyze(- 0x0123 ) -> 16 0123
#          -analyze(+123 ) -> 10 123
#          -analyze(-000123) -> 8 00123
-analyze = $(if $(filter 0,$1),10 0,$(subst .,10 ,$(patsubst .0%,8 %,$(patsubst .0x%,16 %,$(patsubst .0X%,16 %,.$(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst -,,$(subst +,,$(subst $(space),,$1))))))))))))))


# Convert a number in list form to a "normal" number with prefix and optional sign
# $1 = number as list
# $2 = '-' or empty string
-fmt16 = $(if $(filter-out 0,$1),$2)0x$(call -nrmlz,$(subst $(space),,$1))
-fmt10 = $(if $(filter-out 0,$1),$2)$(call -nrmlz,$(subst $(space),,$1))
-fmt8 = $(if $(filter-out 0,$1),$2)0$(call -nrmlz,$(subst $(space),,$1))



# <empty string> := $1 > $2
# b := $1 < $2
# eq := $1 = $2
-bw10- := eq
-udivcmp = $(-bw10-$(firstword $(filter-out 0 00 11 22 33 44 55 66 77 88 99,$(join $1,$2))))


-10to10 = $1
-8to8 = $1
-16to16 = $1

-16to8 = $(if $1,$(call -16to8,$(wordlist 2,$(-num-limit),$1),$(call -add8,$(call -shift-merge,$(-16to8$(firstword $1)),$(call --mul8-4,$(call --mul8-4,$2))))),$2)
-16to80 := 0
-16to81 := 1
-16to82 := 2
-16to83 := 3
-16to84 := 4
-16to85 := 5
-16to86 := 6
-16to87 := 7
-16to88 := 1 0
-16to89 := 1 1
-16to8a := 1 2
-16to8b := 1 3
-16to8c := 1 4
-16to8d := 1 5
-16to8e := 1 6
-16to8f := 1 7

-16to10 = $(if $1,$(call -16to10,$(wordlist 2,$(-num-limit),$1),$(call -add10,$(call -shift-merge,$(-16to10$(firstword $1)),$(call --mul10-2,$(call --mul10-8,$2))))),$2)
-16to100 := 0
-16to101 := 1	 
-16to102 := 2	 
-16to103 := 3	 
-16to104 := 4	 
-16to105 := 5	 
-16to106 := 6	 
-16to107 := 7	 
-16to108 := 8	 
-16to109 := 9	 
-16to10a := 1 0
-16to10b := 1 1
-16to10c := 1 2
-16to10d := 1 3
-16to10e := 1 4
-16to10f := 1 5

-8to10 = $(if $1,$(call -8to10,$(wordlist 2,$(-num-limit),$1),$(call -add10,$(call -shift-merge,$(firstword $1),$(call --mul10-8,$2)))),$2)

-8to16 = $(if $1,$(call -8to16,$(wordlist 2,$(-num-limit),$1),$(call -add16,$(call -shift-merge,$(firstword $1),$(call --mul16-8,$2)))),$2)

-10to16 = $(if $1,$(call -10to16,$(wordlist 2,$(-num-limit),$1),$(call -add16,$(call -shift-merge,$(firstword $1),$(call --mul16-a,$2)))),$2)

-10to8 = $(if $1,$(call -10to8,$(wordlist 2,$(-num-limit),$1),$(call -add8,$(call -shift-merge,$(-10to8$(firstword $1)),$(call --mul8-2,$(call --mul8-5,$2))))),$2)
-10to80 := 0
-10to81 := 1	 
-10to82 := 2	 
-10to83 := 3	 
-10to84 := 4	 
-10to85 := 5	 
-10to86 := 6	 
-10to87 := 7	 
-10to88 := 1 0	 
-10to89 := 1 1	 


# Decision tree for division:
#
#  1 ----------------+
# a-b                |
#        +---------- 5 ----------------+
#        |          a-b-4b             |
#   +--- 3 ----+               +------ 7 -------+
#   |  a-b-2b  |               |   a-b-4b-2b    |
#   2          4               6                8 --------+
# a-b-b     a-b-2b-b       a-b-4b-b        a-b-4b-2b-b    |
#                                                         9
#                                                    a-b-4b-2b-b-b


# $1 = a
# $2 = b
# $3 = current power of 10 of b as list of 0's
-div10-1 = $(call --div10-1$(-udivcmp),$1,$2,$3)
--div10-1 = $(call -div10-5,$(call -sub10,$(join $1,$2)),$(call --mul10-4,$2),$2,$3)
--div10-1z = 0 $(if $3,$(call -div10-1,$1,0 $2,$(wordlist 2,$(-num-limit),$3)))
--div10-1eq = 1 $3


# $1 = a-b
# $2 = 4*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-5 = $(call --div10-5$(-udivcmp),$1,$2,$3,$4)
--div10-5 = $(call -div10-7,$(call -sub10,$(join $1,$2)),$(call --mul10-2,$3),$3,$4)
--div10-5z = $(call -div10-3,$1,$(call --mul10-2,$3),$3,$4)
--div10-5eq = 5 $4

# $1 = a-b
# $2 = 2*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-3 = $(call --div10-3$(-udivcmp),$1,$2,$3,$4)
--div10-3 = $(call -div10-4,$(call -sub10,$(join $1,$2)),$3,$3,$4)
--div10-3z = $(call -div10-2,$1,$3,$3,$4)
--div10-3eq = 3 $4

# $1 = a-b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-2 = $(call --div10-2$(-udivcmp),$1,$2,$3,$4)
--div10-2 = 2 $(if $4,$(call -div10-1,$(call -sub10,$(join $1,$2)),0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-2z = 1 $(if $4,$(call -div10-1,$1,0 $3,$(wordlist 2,$(-num-limit),$4)))
--div10-2eq = 2 $4

# $1 = a-b-2b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-4 = $(call --div10-4$(-udivcmp),$1,$2,$3,$4)
--div10-4 = 4 $(if $4,$(call -div10-1,$(call -sub10,$(join $1,$2)),0 $3,$(wordlist 2,$(-num-limit),$4)))
--div10-4z = 3 $(if $4,$(call -div10-1,$1,0 $3,$(wordlist 2,$(-num-limit),$4)))
--div10-4eq = 4 $4

# $1 = a-b-4b
# $2 = 2*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-7 = $(call --div10-7$(-udivcmp),$1,$2,$3,$4)
--div10-7 = $(call -div10-8,$(call -sub10,$(join $1,$2)),$3,$3,$4)
--div10-7z = $(call -div10-6,$1,$3,$3,$4)
--div10-7eq = 7 $4


# $1 = a-b-4b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-6 = $(call --div10-6$(-udivcmp),$1,$2,$3,$4)
--div10-6 = 6 $(if $4,$(call -div10-1,$(call -sub10,$(join $1,$2)),0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-6z = 5 $(if $4,$(call -div10-1,$1,0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-6eq = 6 $4

# $1 = a-b-4b-2b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-8 = $(call --div10-8$(-udivcmp),$1,$2,$3,$4)
--div10-8 = $(call -div10-9,$(call -sub10,$(join $1,$2)),$3,$3,$4)
--div10-8z = 7 $(if $4,$(call -div10-1,$1,0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-8eq = 8 $4

# $1 = a-b-4b-2b-b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
-div10-9 = $(call --div10-9$(-udivcmp),$1,$2,$3,$4)
--div10-9 = 9 $(if $4,$(call -div10-1,$(call -sub10,$(join $1,$2)),0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-9z = 8 $(if $4,$(call -div10-1,$1,0 $2,$(wordlist 2,$(-num-limit),$4)))
--div10-9eq = 9 $4



# Create a two's-complement representation of the given number
# $1 = hexadecimal number as list
-twoscompl16 = $(wordlist 2,$(-num-limit),$(call -sub16,$(join 1 $(patsubst %,0,$1),0 $1)))
-onescompl16 = $(call -sub16,$(join $(patsubst %,f,$1),$1))

log2 = $(call -log2,$(call -analyze,$1))
-log2 = $(call --log2,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))))
--log2 = $(words $(--log2$(firstword $1)) $(foreach i,$(wordlist 2,$(-num-limit),$1),_ _ _ _))
--log20 :=
--log21 := _
--log22 := _
--log23 := _ _
--log24 := _ _
--log25 := _ _ _
--log26 := _ _ _
--log27 := _ _ _
--log28 := _ _ _
--log29 := _ _ _ _
--log2a := _ _ _ _
--log2b := _ _ _ _
--log2c := _ _ _ _
--log2d := _ _ _ _
--log2e := _ _ _ _
--log2f := _ _ _ _

#----------------------------------------------------------------------
# The following elisp code generates the tables and functions for binary and, or and
# xor. These operations are intrinsically implemented in hexadecimal.
#
# (let* ((base 16)
#        (i 0)
#        (and-pairs "")
#        (or-pairs "")
#        (xor-pairs "")
#        j)
#    (while (< i base)
#        (setq j 0)
#        (while (< j base)
#            (setq
#                and-pairs (concat and-pairs (format "%x%x:=%x " i j (logand i j)))
#                or-pairs (concat or-pairs (format "%x%x:=%x " i j (logior i j)))
#                xor-pairs (concat xor-pairs (format "%x%x:=%x " i j (logxor i j)))
#                j (1+ j)))
#        (setq i (1+ i)))
#    (insert "-and-pairs :=" and-pairs "\n$(foreach i,$(-and-pairs),$(eval --band$(i)))\n"
#            "-or-pairs :=" or-pairs "\n$(foreach i,$(-or-pairs),$(eval --bor$(i)))\n"
#            "-xor-pairs :=" xor-pairs "\n$(foreach i,$(-xor-pairs),$(eval --bxor$(i)))\n")
#    (dolist (f (list "and" "or" "xor" )) 
#       (insert 
#           (format "bit-%s = $(call -bit-%s,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1),$(findstring -,$2))\n" f f)
#           (format "-bit-%s = $(call --bit-%s,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to16,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))\n" f f)
#           (format "--bit-%s = $(call -fmt$5,$(call -16to$5,$(foreach i,$(call -sgnxt-merge,$(if $3,$(call -twoscompl16,$1),$1),$(if $4,$(call -twoscompl16,$2),$2),$3,$4),$(--b%s$(i)))))\n" f f))))

-and-pairs :=00:=0 01:=0 02:=0 03:=0 04:=0 05:=0 06:=0 07:=0 08:=0 09:=0 0a:=0 0b:=0 0c:=0 0d:=0 0e:=0 0f:=0 10:=0 11:=1 12:=0 13:=1 14:=0 15:=1 16:=0 17:=1 18:=0 19:=1 1a:=0 1b:=1 1c:=0 1d:=1 1e:=0 1f:=1 20:=0 21:=0 22:=2 23:=2 24:=0 25:=0 26:=2 27:=2 28:=0 29:=0 2a:=2 2b:=2 2c:=0 2d:=0 2e:=2 2f:=2 30:=0 31:=1 32:=2 33:=3 34:=0 35:=1 36:=2 37:=3 38:=0 39:=1 3a:=2 3b:=3 3c:=0 3d:=1 3e:=2 3f:=3 40:=0 41:=0 42:=0 43:=0 44:=4 45:=4 46:=4 47:=4 48:=0 49:=0 4a:=0 4b:=0 4c:=4 4d:=4 4e:=4 4f:=4 50:=0 51:=1 52:=0 53:=1 54:=4 55:=5 56:=4 57:=5 58:=0 59:=1 5a:=0 5b:=1 5c:=4 5d:=5 5e:=4 5f:=5 60:=0 61:=0 62:=2 63:=2 64:=4 65:=4 66:=6 67:=6 68:=0 69:=0 6a:=2 6b:=2 6c:=4 6d:=4 6e:=6 6f:=6 70:=0 71:=1 72:=2 73:=3 74:=4 75:=5 76:=6 77:=7 78:=0 79:=1 7a:=2 7b:=3 7c:=4 7d:=5 7e:=6 7f:=7 80:=0 81:=0 82:=0 83:=0 84:=0 85:=0 86:=0 87:=0 88:=8 89:=8 8a:=8 8b:=8 8c:=8 8d:=8 8e:=8 8f:=8 90:=0 91:=1 92:=0 93:=1 94:=0 95:=1 96:=0 97:=1 98:=8 99:=9 9a:=8 9b:=9 9c:=8 9d:=9 9e:=8 9f:=9 a0:=0 a1:=0 a2:=2 a3:=2 a4:=0 a5:=0 a6:=2 a7:=2 a8:=8 a9:=8 aa:=a ab:=a ac:=8 ad:=8 ae:=a af:=a b0:=0 b1:=1 b2:=2 b3:=3 b4:=0 b5:=1 b6:=2 b7:=3 b8:=8 b9:=9 ba:=a bb:=b bc:=8 bd:=9 be:=a bf:=b c0:=0 c1:=0 c2:=0 c3:=0 c4:=4 c5:=4 c6:=4 c7:=4 c8:=8 c9:=8 ca:=8 cb:=8 cc:=c cd:=c ce:=c cf:=c d0:=0 d1:=1 d2:=0 d3:=1 d4:=4 d5:=5 d6:=4 d7:=5 d8:=8 d9:=9 da:=8 db:=9 dc:=c dd:=d de:=c df:=d e0:=0 e1:=0 e2:=2 e3:=2 e4:=4 e5:=4 e6:=6 e7:=6 e8:=8 e9:=8 ea:=a eb:=a ec:=c ed:=c ee:=e ef:=e f0:=0 f1:=1 f2:=2 f3:=3 f4:=4 f5:=5 f6:=6 f7:=7 f8:=8 f9:=9 fa:=a fb:=b fc:=c fd:=d fe:=e ff:=f 
$(foreach i,$(-and-pairs),$(eval --band$(i)))
-or-pairs :=00:=0 01:=1 02:=2 03:=3 04:=4 05:=5 06:=6 07:=7 08:=8 09:=9 0a:=a 0b:=b 0c:=c 0d:=d 0e:=e 0f:=f 10:=1 11:=1 12:=3 13:=3 14:=5 15:=5 16:=7 17:=7 18:=9 19:=9 1a:=b 1b:=b 1c:=d 1d:=d 1e:=f 1f:=f 20:=2 21:=3 22:=2 23:=3 24:=6 25:=7 26:=6 27:=7 28:=a 29:=b 2a:=a 2b:=b 2c:=e 2d:=f 2e:=e 2f:=f 30:=3 31:=3 32:=3 33:=3 34:=7 35:=7 36:=7 37:=7 38:=b 39:=b 3a:=b 3b:=b 3c:=f 3d:=f 3e:=f 3f:=f 40:=4 41:=5 42:=6 43:=7 44:=4 45:=5 46:=6 47:=7 48:=c 49:=d 4a:=e 4b:=f 4c:=c 4d:=d 4e:=e 4f:=f 50:=5 51:=5 52:=7 53:=7 54:=5 55:=5 56:=7 57:=7 58:=d 59:=d 5a:=f 5b:=f 5c:=d 5d:=d 5e:=f 5f:=f 60:=6 61:=7 62:=6 63:=7 64:=6 65:=7 66:=6 67:=7 68:=e 69:=f 6a:=e 6b:=f 6c:=e 6d:=f 6e:=e 6f:=f 70:=7 71:=7 72:=7 73:=7 74:=7 75:=7 76:=7 77:=7 78:=f 79:=f 7a:=f 7b:=f 7c:=f 7d:=f 7e:=f 7f:=f 80:=8 81:=9 82:=a 83:=b 84:=c 85:=d 86:=e 87:=f 88:=8 89:=9 8a:=a 8b:=b 8c:=c 8d:=d 8e:=e 8f:=f 90:=9 91:=9 92:=b 93:=b 94:=d 95:=d 96:=f 97:=f 98:=9 99:=9 9a:=b 9b:=b 9c:=d 9d:=d 9e:=f 9f:=f a0:=a a1:=b a2:=a a3:=b a4:=e a5:=f a6:=e a7:=f a8:=a a9:=b aa:=a ab:=b ac:=e ad:=f ae:=e af:=f b0:=b b1:=b b2:=b b3:=b b4:=f b5:=f b6:=f b7:=f b8:=b b9:=b ba:=b bb:=b bc:=f bd:=f be:=f bf:=f c0:=c c1:=d c2:=e c3:=f c4:=c c5:=d c6:=e c7:=f c8:=c c9:=d ca:=e cb:=f cc:=c cd:=d ce:=e cf:=f d0:=d d1:=d d2:=f d3:=f d4:=d d5:=d d6:=f d7:=f d8:=d d9:=d da:=f db:=f dc:=d dd:=d de:=f df:=f e0:=e e1:=f e2:=e e3:=f e4:=e e5:=f e6:=e e7:=f e8:=e e9:=f ea:=e eb:=f ec:=e ed:=f ee:=e ef:=f f0:=f f1:=f f2:=f f3:=f f4:=f f5:=f f6:=f f7:=f f8:=f f9:=f fa:=f fb:=f fc:=f fd:=f fe:=f ff:=f 
$(foreach i,$(-or-pairs),$(eval --bor$(i)))
-xor-pairs :=00:=0 01:=1 02:=2 03:=3 04:=4 05:=5 06:=6 07:=7 08:=8 09:=9 0a:=a 0b:=b 0c:=c 0d:=d 0e:=e 0f:=f 10:=1 11:=0 12:=3 13:=2 14:=5 15:=4 16:=7 17:=6 18:=9 19:=8 1a:=b 1b:=a 1c:=d 1d:=c 1e:=f 1f:=e 20:=2 21:=3 22:=0 23:=1 24:=6 25:=7 26:=4 27:=5 28:=a 29:=b 2a:=8 2b:=9 2c:=e 2d:=f 2e:=c 2f:=d 30:=3 31:=2 32:=1 33:=0 34:=7 35:=6 36:=5 37:=4 38:=b 39:=a 3a:=9 3b:=8 3c:=f 3d:=e 3e:=d 3f:=c 40:=4 41:=5 42:=6 43:=7 44:=0 45:=1 46:=2 47:=3 48:=c 49:=d 4a:=e 4b:=f 4c:=8 4d:=9 4e:=a 4f:=b 50:=5 51:=4 52:=7 53:=6 54:=1 55:=0 56:=3 57:=2 58:=d 59:=c 5a:=f 5b:=e 5c:=9 5d:=8 5e:=b 5f:=a 60:=6 61:=7 62:=4 63:=5 64:=2 65:=3 66:=0 67:=1 68:=e 69:=f 6a:=c 6b:=d 6c:=a 6d:=b 6e:=8 6f:=9 70:=7 71:=6 72:=5 73:=4 74:=3 75:=2 76:=1 77:=0 78:=f 79:=e 7a:=d 7b:=c 7c:=b 7d:=a 7e:=9 7f:=8 80:=8 81:=9 82:=a 83:=b 84:=c 85:=d 86:=e 87:=f 88:=0 89:=1 8a:=2 8b:=3 8c:=4 8d:=5 8e:=6 8f:=7 90:=9 91:=8 92:=b 93:=a 94:=d 95:=c 96:=f 97:=e 98:=1 99:=0 9a:=3 9b:=2 9c:=5 9d:=4 9e:=7 9f:=6 a0:=a a1:=b a2:=8 a3:=9 a4:=e a5:=f a6:=c a7:=d a8:=2 a9:=3 aa:=0 ab:=1 ac:=6 ad:=7 ae:=4 af:=5 b0:=b b1:=a b2:=9 b3:=8 b4:=f b5:=e b6:=d b7:=c b8:=3 b9:=2 ba:=1 bb:=0 bc:=7 bd:=6 be:=5 bf:=4 c0:=c c1:=d c2:=e c3:=f c4:=8 c5:=9 c6:=a c7:=b c8:=4 c9:=5 ca:=6 cb:=7 cc:=0 cd:=1 ce:=2 cf:=3 d0:=d d1:=c d2:=f d3:=e d4:=9 d5:=8 d6:=b d7:=a d8:=5 d9:=4 da:=7 db:=6 dc:=1 dd:=0 de:=3 df:=2 e0:=e e1:=f e2:=c e3:=d e4:=a e5:=b e6:=8 e7:=9 e8:=6 e9:=7 ea:=4 eb:=5 ec:=2 ed:=3 ee:=0 ef:=1 f0:=f f1:=e f2:=d f3:=c f4:=b f5:=a f6:=9 f7:=8 f8:=7 f9:=6 fa:=5 fb:=4 fc:=3 fd:=2 fe:=1 ff:=0 
$(foreach i,$(-xor-pairs),$(eval --bxor$(i)))
bit-and = $(call -bit-and,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1),$(findstring -,$2))
-bit-and = $(call --bit-and,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to16,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
--bit-and = $(call -fmt$5,$(call -16to$5,$(foreach i,$(call -sgnxt-merge,$(if $3,$(call -twoscompl16,$1),$1),$(if $4,$(call -twoscompl16,$2),$2),$3,$4),$(--band$(i)))))
bit-or = $(call -bit-or,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1),$(findstring -,$2))
-bit-or = $(call --bit-or,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to16,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
--bit-or = $(call -fmt$5,$(call -16to$5,$(foreach i,$(call -sgnxt-merge,$(if $3,$(call -twoscompl16,$1),$1),$(if $4,$(call -twoscompl16,$2),$2),$3,$4),$(--bor$(i)))))
bit-xor = $(call -bit-xor,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1),$(findstring -,$2))
-bit-xor = $(call --bit-xor,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to16,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
--bit-xor = $(call -fmt$5,$(call -16to$5,$(foreach i,$(call -sgnxt-merge,$(if $3,$(call -twoscompl16,$1),$1),$(if $4,$(call -twoscompl16,$2),$2),$3,$4),$(--bxor$(i)))))

bit-not = $(call -bit-not,$(call -analyze,$1),$(findstring -,$1))
-bit-not = $(call --bit-not,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$2,$(firstword $1))
--bit-not = $(call -fmt$3,$(call -16to$3,$(call -onescompl16,$(if $2,$(call -twoscompl16,$1),$1))))

#----------------------------------------------------------------------
###### $(call add,_num1_,_num2_)
## Calculate _num1+num2_. Both arguments are signed integers.
## Numeric bases can be mixed, the result is given in the same base as _num1_.
## Examples:
## - `$(call add,-1,0)` --> `-1`
## - `$(call add,0x12,13)` --> `0x1f`
add = $(call -add,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1)v$(findstring -,$2))
-add = $(call --add$3,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1))),$(call -$(firstword $2)to$(firstword $1),$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$(firstword $1))
--addv = $(call -fmt$3,$(call -add$3,$(call -shift-merge,$1,$2)))
--add-v = $(if $(findstring >,$(-ucmp)),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$1,$2)),-),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$2,$1))))
--add-v- = $(call -fmt$3,$(call -add$3,$(call -shift-merge,$1,$2)),-)
--addv- = $(if $(findstring <,$(-ucmp)),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$2,$1)),-),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$1,$2))))

#----------------------------------------------------------------------
###### $(call sub,_num1_,_num2_)
## Calculate _num1-num2_. Both arguments are signed integers.
## Numeric bases can be mixed, the result is given in the same base as _num1_.
## Examples:
## - `$(call sub,12,-10)` --> `22`
## - `$(call sub,012,-10)` --> `024`
sub = $(call -sub,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1)v$(findstring -,$2))
-sub = $(call --sub$3,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1))),$(call -$(firstword $2)to$(firstword $1),$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$(firstword $1))
--subv = $(if $(findstring <,$(-ucmp)),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$2,$1)),-),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$1,$2))))
--sub-v = $(call -fmt$3,$(call -add$3,$(call -shift-merge,$1,$2)),-)
--sub-v- = $(if $(findstring >,$(-ucmp)),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$1,$2)),-),$(call -fmt$3,$(call -sub$3,$(call -shift-merge,$2,$1))))
--subv- = $(call -fmt$3,$(call -add$3,$(call -shift-merge,$1,$2)))

#----------------------------------------------------------------------
###### $(call cmp,_num1_,_num2_)
## Compare _num1 with num2_ and emit the characters `<`,`>` or `=`. Both arguments
## are signed integers. Numeric bases can be mixed.
## Examples:
## - `$(call cmp,12,-14)` --> `>`
## - `$(call cmp,012,10)` --> `=`
cmp = $(call -cmp,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1)v$(findstring -,$2))
-cmp = $(call --cmp$3,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1))),$(call -$(firstword $2)to$(firstword $1),$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))),$(firstword $1))
--cmpv = $(-ucmp)
--cmp-v = <
--cmp-v- = $(call -ucmp,$2,$1)
--cmpv- = >

#----------------------------------------------------------------------
###### $(call int-(le|lt|ge|gt|eq),_num1_,_num2_)
## Compare _num1 with num2_ and emit the characters `<`,`>`,`=` or the empty string.
## The postfix alternatives decide which comparison is executed:
## less-than (lt), less-or-equal (le), greater-than (gt),
## greater-or-equal (ge) and equal (eq). 
## Both arguments are signed integers. Numeric bases can be mixed. The result can
## be used in an $(if ) expression.
## Examples:
## - `$(call int-ge,12,-14)` --> `>`
## - `$(call int-le,12,-14)` --> `` (empty string)
## - `$(if $(call int-eq,012,10),equal,not equal)` --> `equal`
int-le = $(filter < =,$(cmp))
int-lt = $(filter <,$(cmp))
int-ge = $(filter > =,$(cmp))
int-gt = $(filter >,$(cmp))
int-eq = $(filter =,$(cmp))

#----------------------------------------------------------------------
###### $(call mul,_num1_,_num2_)
## Multiply integer _num1_ with integer _num2_. Both arguments can be signed.
## Numeric bases can be mixed, the result is given in the same base as _num1_.
## Examples:
## - `$(call mul,-1,0)` --> `0`
## - `$(call mul,-0x1,0)` --> `0x0`
## - `$(call mul,03,-7)` --> `-025`
mul = $(call -mul,$(call -analyze,$1),$(call -analyze,$2),$(subst --,,$(findstring -,$1)$(findstring -,$2)))
-mul = $(call -fmt$(firstword $1),$(call -mul$(firstword $1),$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1))),$(call -$(firstword $2)to$(firstword $1),$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2))))),$3)

#----------------------------------------------------------------------
###### $(call div,_num1_,_num2_)
## Divide integer _num1_ by integer _num2_. Both arguments can be signed.
## Numeric bases can be mixed, the result is given in the same base as _num1_.
## Examples:
## - `$(call div,9876543210,0x13)` --> `519818063`
## - `$(call div,0x9876543210,16)` --> `0x987654321`
div = $(call -div,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1)$(findstring -,$2))
-div = $(subst --,,$3)$(call -fmt$(firstword $1),$(call -10to$(firstword $1),$(call -div10,$(call -$(firstword $1)to10,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to10,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))))))
-div10 = $(call -div10-1,$1,$2,$(wordlist $(words _ $2),$(words $1),$(-all-0)))

#----------------------------------------------------------------------
mod = $(call -mod,$(call -analyze,$1),$(call -analyze,$2),$(findstring -,$1)$(findstring -,$2))
-mod = $(subst --,,$3)$(call -fmt$(firstword $1),$(call -10to$(firstword $1),$(call -mod10,$(call -$(firstword $1)to10,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))),$(call -$(firstword $2)to10,$(call -xpld-$(firstword $2),$(call -nrmlz,$(lastword $2)))))))
-mod10 = $(call -sub10,$(call -shift-merge,$1,$(call -mul10,$(call -div10,$1,$2),$2)))

#----------------------------------------------------------------------
# Negate a number
neg = $(if $(findstring -,$1),$(subst -,,$1),-$1)

#----------------------------------------------------------------------
to-oct = $(call -to-oct,$(call -analyze,$1),$(findstring -,$1))
-to-oct = $2$(call -fmt8,$(call -$(firstword $1)to8,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))))
to-hex = $(call -to-hex,$(call -analyze,$1),$(findstring -,$1))
-to-hex = $2$(call -fmt16,$(call -$(firstword $1)to16,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))))
to-dec = $(call -to-dec,$(call -analyze,$1),$(findstring -,$1))
-to-dec = $2$(call -fmt10,$(call -$(firstword $1)to10,$(call -xpld-$(firstword $1),$(call -nrmlz,$(lastword $1)))))

#----------------------------------------------------------------------
# Return a number zero extended to the given number of places. The sign gets dropped.
zero-ext = $(call -zero-ext,$(call -nrmlz,$(lastword $(call -analyze,$1))),$(or $2,0))
-zero-ext = $(subst $(space),,$(wordlist $(words _ $(call -xpld-16,$1)),$2,$(-all-0)))$1

#----------------------------------------------------------------------
# Round a number down to the given modulus.
# $1 = number
# $2 = modulus
# - round(0x1234,0x100) --> 0x1200
round-dn = $(call bit-and,$1,$(call neg,$2))

#----------------------------------------------------------------------
# Round a number up to the next given modulus.
# $1 = number
# $2 = modulus
# - round-up(0x1234,0x100) --> 0x1300
round-up = $(call round-dn,$(call add,$1,$(call sub,$2,1)),$2)

#----------------------------------------------------------------------
# Round a number up to the next given modulus - 1
# $1 = number
# $2 = modulus
# - fill-up(0x1234,0x100) --> 0x12ff
fill-up = $(call bit-or,$1,$(call sub,$2,1))



#----------------------------------------------------------------------
# Reorder the records of a table according to the given index list.
# The reorder index is a list of starting indexes of the table records.
# The index list doesn't need to be complete, missing records are dropped.
# $1 = table as list
# $2 = table width
# $3 = reorder index
# -
# -reorder-tbl(1 2 3 4 5 6 7 8 9 10 11 12,4,10 1 7) -> 10 11 12 1 2 3 7 8 9
-reorder-tbl = $(call --reorder-tbl,$1,$(call -usub10,$2,1),$3)
--reorder-tbl = $(foreach i,$3,$(wordlist $(i),$(call -uadd10,$(i),$2),$1))

#----------------------------------------------------------------------
# Produce a list of zeros which is length floor(log2) of the given number.
# $1 - decimal literal
# - -log2-zeros(15)
-log2-zeros = $(wordlist 2,$(call log2,$1),$(-all-0))


#----------------------------------------------------------------------
# Unsigned add, sub, multiply of base10 numbers. Serve as helper functions
# for table and sorting operations.
# $1 - first number
# $2 - second number
-umul10 = $(call -nrmlz,$(subst $(space),,$(call -mul10,$(call -xpld-10,$1),$(call -xpld-10,$2))))
-uadd10 = $(call -nrmlz,$(subst $(space),,$(call -add10,$(call -shift-merge,$(call -xpld-10,$1),$(call -xpld-10,$2)))))
-usub10 = $(call -nrmlz,$(subst $(space),,$(call -sub10,$(call -shift-merge,$(call -xpld-10,$1),$(call -xpld-10,$2)))))

#----------------------------------------------------------------------
# Generate a decimal index for the list. The index is a list of decimal
# literals starting from 0 counting upwards and has as many digits
# (leading 0's) as necessary to cover the range of elements.
# $1 - list
# $2 - prefix string
# - index(a b c d e f..) -->  00 01 02 03 04 05 06 07 08 09 10 11 ..
-bld-ix = $(subst $(space), $2, $(wordlist 1,$(words $1),$(call --bld-ix,$(call -xpld-10,$(words $1)))))
--bld-ix = $(call ---bld-ix,$(wordlist 2,10,$1),0 $(wordlist 1,$(firstword $1),1 2 3 4 5 6 7 8 9))
---bld-ix = $(if $1,$(call ---bld-ix,$(wordlist 2,10,$1),$(foreach i,$2,$(i)0 $(i)1 $(i)2 $(i)3 $(i)4 $(i)5 $(i)6 $(i)7 $(i)8 $(i)9)),$2)


#----------------------------------------------------------------------
# Retrieve one table line as a $(-separator) separated record
# $1 - table
# $2 - table width
# - chop-rec(1 2 3 4 5 6,3) -> 1¤2¤3  4¤5¤6
-chop-rec = $(call --chop-rec,$1,$2,$(call -uadd10,$2,1))
--chop-rec = $(if $1,$(subst $(space),$(-separator),$(wordlist 1,$2,$1)) $(call --chop-rec,$(wordlist $3,2147483647,$1),$2,$3))

#----------------------------------------------------------------------
# Generate a list of sorted indexes from the given unsorted keys.
# This means that if the first key would be sorted to position 15,
# then in the returned list at position 15 there will be a 1.
# These numbers can be directly used as index in the $(wordlist) function.
# Character 164('¤') is used as separator and is forbidden in key columns.
# $1 - list of keys before sorting
# $2 - table width
-sort-ix = $(foreach i,$(call drop-prfx,$(sort $(join $1,$(call -bld-ix,$1,$(-separator))))),$(call -uadd10,1,$(call -umul10,$(i),$2)))

#----------------------------------------------------------------------
# Generate a list of reversely sorted indexes from the given unsorted keys.
# This means that if the Nth key would be sorted to position 15,
# then in the returned list at position 15 there will be a N.
# These numbers can be directly used as index in the $(wordlist) function.
# $1 - list of keys before sorting
# $2 - table width
-rsort-ix = $(call rev-list,$(call -sort-ix,$1,$2))

#----------------------------------------------------------------------
###### $(call sort-tbl,_table_,_key-gen_)
## Sort a _table_ by lines. Key comparison is done by lexical ordering.
## Lexical ordering means that 'aa' < 'aaa' < 'aab' < 'ab'. The empty string
## always compares smaller than any other string. The strings may contain spaces
## but leading and trailing spaces are not considered in the comparison and
## multiple interior spaces are substituted by a single one. Spaces compare
## smaller than downcase characters but greater than upcase. In short: you
## should know what you are doing if you have spaces inside your strings.
##
## The _key-gen_ is a function expression which shall yield the key
## for the sorting comparison. Note that the key is *always* 
## evaluated as a string - this means that you usually can't use
## numerals directly as key but have to left-pad them with 0's to 
## make them the same length.
sort-tbl = $(call -sort-tbl,$(strip $1),$2)
-sort-tbl = $(call --sort-tbl,$(wordlist 2,2147483647,$1),$(firstword $1),$2)
--sort-tbl = $2 $(call -reorder-tbl,$1,$2,$(call -sort-ix,$(foreach params,$(call -chop-rec,$1,$2),$(call exec,$3,$(subst $(-separator),$(space),$(params)))),$2))

#----------------------------------------------------------------------
# Reverse sort a table by lines.
# $1 - table
# $2 - keygenerator function (takes one table line as parameters $1,$2,$3,etc.
rsort-tbl = $(call -rsort-tbl,$(strip $1),$2)
-rsort-tbl = $(call --rsort-tbl,$(wordlist 2,2147483647,$1),$(firstword $1),$2)
--rsort-tbl = $2 $(call -reorder-tbl,$1,$2,$(call -rsort-ix,$(foreach params,$(call -chop-rec,$1,$2),$(call exec,$3,$(subst $(-separator),$(space),$(params)))),$2))

#----------------------------------------------------------------------
# Map a function to each table line. The column elements of the table are 
# passed as parameters to the lambda/function.
# $1 - table
# $2 - output function (takes one table line as parameters $$1,$$2,$$3,etc.
map-tbl = $(call -map-tbl,$(strip $1),$2)
-map-tbl = $(call --map-tbl,$(wordlist 2,2147483647,$1),$(firstword $1),$2)
--map-tbl = $(call ---map-tbl,$(call -chop-rec,$1,$2),$3)
---map-tbl = $(if $1,$(call exec,$2,$(subst $(-separator),$(space),$(firstword $1)))$(call ---map-tbl,$(wordlist 2,2147483647,$1),$2))


#----------------------------------------------------------------------
# $1=list of numbers (tuple indices)
# $2=list of words from which to choose
# returns: the words from $2 which correspond to indexes (starting with 1) from $1
pick = $(foreach elem,$1,$(word $(elem),$2))

#----------------------------------------------------------------------
###### $(call select,_col-nrs_,_table_,_where-clause_)
## Select all rows from a _table_ which fulfill the _where-clause_ and pick the subset
## of _col-nrs_ from these rows to form an output list. A gmtt **table** is
## a list with a leading decimal which denotes the number of columns in this 'table'.
## See the documentation for gmtt at [https://github.com/markpiffer/gmtt].
## The _where-clause_ is a function or a 'exec' expression (i.e. function expression
## written into the parameter place directly) which receives the elements of each row of
## the table in order as parameters `$1`,`$2`,`$3` etc. **Note:** the function call/exec
## needs to be $-quoted, that is, every '$' that appears as variable reference must be
## doubled '$$'. See the examples below. The clause shall return true (non-empty string)
## or false (empty string) to accept/reject each rows elements into/from the result
## of the select. The selection is limited to the column numbers given in _col-nrs_,
## in their respective order. Do not confuse the _col-nrs_ subset with the parameters 
## given to the 'where' function, the former is just a possibly reordered subset of the
## latter which is formed after the 'where' function accepted the record.
## (`select` mimics a SQL `SELECT model, price FROM cars WHERE color="red"` and 
## returns effectively a list of subsets from all positively selected rows. If you prepend
## the result list with its column count, you have a new gmtt table)
##
## Exapmle:
## - `test-tbl := 4   foo bar baz 11    foo bar baf 22   faa bar baz 33`
## - `$(call select,3 1 2 3,$(test-tbl),$$(call str-match,$$1,%oo))` --> `baz foo bar baz baf foo bar baf`
## The same can be achieved, if we use a function as where clause:
## - `ends-in-oo = $(call str-match,$1,%oo)`
## - `$(call select,3 1 2 3,$(test-tbl),$$(call ends-in-oo,$$1))` --> `baz foo bar baz baf foo bar baf`
select = $(strip $(call -select,$1,$(strip $2),$3))
-select = $(call --select,$(wordlist 2,2147483647,$2),$(firstword $2),$(call add,$(firstword $2),1),$1,$3)
--select = $(if $1,$(if $(call exec,$5,$(call list2param,$(wordlist 1,$2,$1))), $(call pick,$4,$1))$(call --select,$(wordlist $3,2147483647,$1),$2,$3,$4,$5))



#----------------------------------------------------------------------
# $1 = list of column numbers
# $2 = table
# $3 = "where-clause", function of $4 parameters returning true (non-null) or false (empty string)
# $4 = a function which processes a list containing the selection from $3 on each iteration of the select
# returns: a list of columns (selection in $2) from the rows in table $1 where condition $3 is non-null
map-select = $(strip $(call -map-select,$1,$(strip $2),$3,$4))
-map-select = $(call --map-select,$(wordlist 2,2147483647,$2),$(firstword $2),$(call add,$(firstword $2),1),$1,$3,$4)
--map-select = $(if $1,$(if $(call exec,$5,$(call list2param,$(wordlist 1,$2,$1))), $(call exec,$6,$(call list2param,$(call pick,$4,$1))))$(call --map-select,$(wordlist $3,2147483647,$1),$2,$3,$4,$5,$6))


#----------------------------------------------------------------------
###### $(call join-tbl,_table1_,table2_[,_NIL-value_])
## Join two tables side by side. The resulting table has as many columns
## as both input tables combined. The optional NIL value will be 
## used to fill empty places in the resulting table if one input table has
## less rows than the other. If the NIL value is *not* given, the shorter
## of the two tables will be repeated as many times as necessary to fill up
## the number of rows to that of the longer table.
## The order of the tables is preserved.
##
## Examples:
## - `$(call join-tbl,1 one two,2 first 1st second 2nd third 3rd)` --> `3 one first 1st two second 2nd one third 3rd`
## - `$(call join-tbl,2 one apple two oranges,1 1st 2nd 3rd,NIL)` --> `3 one apple 1st two oranges 2nd NIL NIL 3rd`
join-tbl = $(call add,$(word 1,$1),$(word 1,$2)) $(call -join-tbl,$(call -chop-rec,$(wordlist 2,2147483647,$1),$(word 1,$1)),$(call -chop-rec,$(wordlist 2,2147483647,$2),$(word 1,$2)),$(if $3,$(subst $(space),$(-separator),$(call n-list,$3,$(word 1,$1)))),$(if $3,$(subst $(space),$(-separator),$(call n-list,$3,$(word 1,$2)))))
-join-tbl = $(call --join-tbl,$1,$2,$(call sub,$(words $1),$(words $2)),$3,$4)
--join-tbl = $(if $(findstring -,$3),$(call ---join-tbl,$1 $(call n-list,$(or $4,$1),$(subst -,,$3)),$2),$(if $(subst 0,,$3),$(call ---join-tbl,$1,$2 $(call n-list,$(or $5,$2),$3)),$(call ---join-tbl,$1,$2)))
---join-tbl = $(subst $(-separator), ,$(join $(patsubst %,%$(-separator),$1),$2))


##### Miscellaneous Functions

###### $(call head,_list_)
## Functionally identical to `$(firstword )`. Return the head (first element) of a list.
head = $(firstword $1)

###### $(call tail,_list_)
## Return the second and subsequent elements of a list as a new list.
tail = $(wordlist 2,2147483647,$1)

###### $(call while,_quoted-condition_,_quoted-code_[,_quoted-last-statements_])
## Execute a while loop of _quoted-code_ as long as _condition_ is true (not the empty string).
## All code statements need to be given as quoted make code (replace `$` with `$$`). 
## The code in _quoted-code_ and _quoted-last-statement_ is `eval`ed in the while loop.
## This means that you can use variable assignments like in ordinary code BUT the assignments
## will be visible outside of the while loop! The optional _quoted-last-statement_ is executed *always*
## when leaving the while loop, even when the loop body was never executed.
## Example: filter out the `-mllvm` flags from the `CFLAGS` variable and put them in an extra variable
## `CFLAGS := -DFOO -DBAR -mllvm llvmflag1 -mllvm llvmflag2`
## `$(call while, $$(call glob-match,$$(CFLAGS),*-mllvm *),\`
## `   tmp := $$(call glob-match,$$(CFLAGS),*-mllvm *)$(newline)\`
## `   rest := $$(call spc-unmask,$$(word 3,$$(tmp)))$(newline)\`
## `   llvm_flg := $$(firstword $$(rest))$(newline)\`
## `   CFLAGS := $$(firstword $$(tmp)) $$(call tail,$$(rest))$(newline)\`
## `   $$(info [[[$$(tmp) --- $$(rest) --- $$(CFLAGS) ]]])$(newline)\`
## `   MLLVM_FLAGS+=$$(llvm_flg),\`
## `MLLVM_FLAGS := $$(strip $$(MLLVM_FLAGS))$(newline)\`
## `CFLAGS := $$(call spc-unmask,$$(CFLAGS))\`
## `)`
while = $(if $(call exec,$1),$(eval $2)$(call while,$1,$2,$3),$(eval $3))

#----------------------------------------------------------------------
###### $(call verbose,[_string1_],[_string2_],[_string3_],[_string4_],[_string5_],[_string6_],[_string7_],[_string8_],[_string9_])
## Write strings to stdout depending on warning level. The global variable `VERBOSITY`
## needs to be defined as a string of numerals from 0..9, e.g. `VERBOSITY = 01289`. This will 
## trigger the output of parameters 1,2,3,9 and 10 for all `$(call verbosity ...)` invocations in
## the makefile. Verbosity is additive, *not* hierarchical, i.e. `VERBOSITY=9` will not effect the output
## of all warnings/info-strings from level 0 to 9 but only for level 9. 
## Examples:
## - `VERBOSITY = 0`
## - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 0`
## - `VERBOSITY = 2`
## - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 2`
## - `VERBOSITY = 012345`
## - `$(call verbose,warninglevel 0,warninglevel 1,warninglevel 2,info 1,info 2,info 3)` -> `warninglevel 0 warninglevel 1 warninglevel 2 info 1 info 2 info 3`
verbose = $(if $(VERBOSITY),$(info $(-verbose)))
-verbose = $(if $(findstring 0,$(VERBOSITY)),$1 )$(if $(findstring 1,$(VERBOSITY)),$2 )$(if $(findstring 2,$(VERBOSITY)),$3 )$(if $(findstring 3,$(VERBOSITY)),$4 )$(if $(findstring 4,$(VERBOSITY)),$5 )$(if $(findstring 5,$(VERBOSITY)),$6 )$(if $(findstring 6,$(VERBOSITY)),$7 )$(if $(findstring 7,$(VERBOSITY)),$8 )$(if $(findstring 8,$(VERBOSITY)),$9 )$(if $(findstring 9,$(VERBOSITY)),$(10) )

#----------------------------------------------------------------------
# Return list of all files with the given _filename_ in the tree starting at _dirname_
# $1 filename
# $2 dirname (all subdirectories will be searched too)
search-down=$(if $(wildcard $2/$1),$(wildcard $2/$1))$(foreach dir,$(wildcard $2/*/.),$(call search-down,$1,$(subst /.$(space),,$(dir)$(space))))

#----------------------------------------------------------------------
# Return list of all files 
# $1 filename
# $2 dirname within which (+subdirectories) filename is to be found
# $3 current directory
search-up=$(if $(wildcard $3/$1),$(wildcard $3/$1))$(if $(wildcard $3/$2),$(call search-down,$1,$3/$2),$(call search-up,$1,$2,$3/..)))

#----------------------------------------------------------------------
# $1 filename
# $2 dirname within which (+subdirectories) filename is to be found
search-above=$(firstword $(call search-up,$1,$2,$(wildcard .)))

#----------------------------------------------------------------------
###### $(call collect-files-uniq,_list-of-globs_)
## Return a list with the full path + name of all files which match one of the glob
## expressions. If files of the same name exist under different
## paths, the first glob which matches takes precedence and all further appearances
## of this file are ignored (i.e. not returned).
## Example: `$(call collect-files-uniq,dir1/*.c dir2/*.c dir2/foo.h dir1/*.h dir2/*.h)`
## This returns all files ending in `.c` from `dir1` and also those from `dir2` which didn't appear
## in earlier. Moreover, `dir2/foo.h` (if it exists) takes presecedence over a possible `dir1/foo.h`.
collect-files-uniq = $(-gmtt-dbg-args)$(if $1,$(call -collect-files-uniq,$(wordlist 2,2147483647,$1),$2,$3,$(filter-out $3,$(wildcard $(firstword $1)))),$2)
-collect-files-uniq = $(-gmtt-dbg-args)$(call collect-files-uniq,$1,$2 $4,$3 $(addprefix %/,$(notdir $4)))


#----------------------------------------------------------------------
#$(call -gmtt-test,$$(call f,x),y)
-gmtt-test = $(if $(call -compare-result,$(call exec,$1),$2),$(info Ok: $1 = $2),$(info Test failed: $1 = $(call exec,$1), should be: $2))
-compare-result = $(call str-eq,$(strip $1),$(strip $2))
