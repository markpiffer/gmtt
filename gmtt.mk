########################################################################
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
# Copyright (c) 2017 Mark Piffer
#


empty := #    
false := $(empty)
space := $(strip) $(strip)
comma := ,
define newline :=
$(strip)
$(strip)
endef
hex_chars := 0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
alnum_chars := _ 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z


__gmtt-dbg-args = $(if $(__gmtt-dbg-info),$(info $0($(if $1$2$3$4$5$6,<$1>$(if $2$3$4$5$6,<$2>$(if $3$4$5$6,<$3>$(if $4$5$6,<$4>$(if $5$6,<$5>$(if $6,<$6>)))))))))

######################################################################
# Maximum number of elements in a table (column times rows)
tbl-limit := 65000


######################################################################
# Insert a blank after every occurrence of the substrings in the given string.
# $1 = list of substrings (or single characters)
# $2 = string to explode
# Example: explode(a b c d e f,0deadbeef0) --> 0d e a d b e e f 0
explode = $(if $1,$(subst $(firstword $1),$(firstword $1) ,$(call explode,$(wordlist 2,255,$1),$2)),$2)

######################################################################
# Remove all spaces from a list/string
# $1 string or list
implode = $(subst $(space),,$1)

######################################################################
# Repeat a string for 2^N times. N is given in the form of a list with length N.
# $1 = string to repeat
# $2 = list of length N
# Example: (foo,1 2 3) --> foofoofoofoofoofoofoofoo
#__rpt2pN = $(subst $1,$1$1,$(if $(wordlist 2,2,$2),$(call __rpt2pN,$1,$(wordlist 2,64,$2)),$1))

######################################################################
# Repeat a string for 2^N times. N is given as a decimal.
# $1 = string to repeat
# $2 = list of length N
# Example: (foo,3) --> foofoofoofoofoofoofoofoo
#rpt2pN = $(call __rpt2pN,$1,$(wordlist 1,$2, _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _))

######################################################################
# Create a list of N repetitions of a string
# $1 = string to repeat
# $2 = number of repetitions
listN = $(if $(word $2,$1),$(wordlist 1,$2,$1),$(call listN,$1 $1,$2))

######################################################################
# Count a binary literal up by 1
# $1 = binary literal string
# Example: bincnt(010011) -> 010100
bincnt=$(if $1,$(if $(patsubst %1,,$1),$(patsubst %0,%1,$1),$(call bincnt,$(patsubst %1,%,$1))0),1)

######################################################################
# Generate a different symbol at each call. Last generated symbol is
# accessible via $(last_symgen)
symgen = $(eval last_symgen:=sym$(call bincnt,$(subst sym,,$(last_symgen))))$(last_symgen)

######################################################################
# Left-pad a number literal with the given character (or string)
# $1 = number to pad
# $2 = final width
# $3 = padding character
lpad = $(call implode,$(wordlist $(words _ $(call explode,$(alnum_chars),$1)),$2,$(call listN,$3,$2)))$1

######################################################################
# Remove a prefix from a string. If the prefix doesn't exist, the
# string is unchanged.
# $1 = string
# $2 = prefix
lstrip = $(patsubst $2%,%,$1)

######################################################################
# return t if string $1 and $2 are identical, empty string otherwise
str-eq = $(if $(subst x$1,,x$2),,t)

str-smaller = $(call str-eq,$(sort $1 $2),$1 $2)
str-greater = $(call str-eq,$(sort $1 $2),$2 $1)

######################################################################
# return t if string $1 and $2 match (with % wildcard either in $1 or $2), empty string otherwise
str-match = $(if $(and $(strip $(patsubst $(strip $1),,$(strip $2))),$(strip $(patsubst $(strip $2),,$(strip $1)))),,t)

######################################################################
# $1 = wordlist to invert
# $2 = length of wordlist
_rev-list = $(if $1,$(call _rev-list,$(wordlist 2,$2,$1),$2) $(firstword $1))


######################################################################
# $1 = list to invert
rev-list = $(call _rev-list,$1,$(words $1))

list2param = $(subst $(space),$(comma),$(strip $1))

lambda = $(eval _lambda=$1)$(eval _lambda:=$$(call _lambda,$(call list2param,$2)))$(_lambda)
format = $(lambda)

######################################################################
# $1 filename
# $2 dirname (all subdirectories will be searched too)
search-down=$(if $(wildcard $2/$1),$(wildcard $2/$1),$(foreach dir,$(wildcard $2/*/.),$(call search_down,$1,$(subst /.$(space),,$(dir)$(space)))))

######################################################################
# $1 filename
# $2 dirname within which (+subdirectories) filename is to be found
# $3 current directory
search-up=$(if $(wildcard $3/$1),$(wildcard $3/$1),$(if $(wildcard $3/$2),$(call search_down,$1,$3/$2),$(call search_up,$1,$2,$3/..)))

######################################################################
# $1 filename
# $2 dirname within which (+subdirectories) filename is to be found
search-above=$(firstword $(call search_up,$1,$2,$(wildcard .)))


######################################################################
num-limit:=64




_all-0 := $(call listN,0,64)
_all-f := $(call listN,f,64)

_shift-merge = $(if $(word $(words $1),$2),$(join $(wordlist $(words _ $1),$(words $2),$(_all-0)) $1,$2),$(join $1,$(wordlist $(words _ $2),$(words $1),$(_all-0)) $2))


########################################################################
# Merge two sign-extended numbers. This function is only implemented for
# hexadecimal numbers.
#
# $1 = 1st number as list
# $2 = 2nd number as list
# $3 = 1st sign (- or empty)
# $4 = 2nd sign (- or empty)
_sgnxt-merge = $(if $(word $(words $1),$2),$(join $(wordlist $(words _ $1),$(words $2),$(_all-$(subst 0-,f,0$3))) $1,$2),$(join $1,$(wordlist $(words _ $2),$(words $1),$(_all-$(subst 0-,f,0$4))) $2))

_xpld-num = $(subst 0, 0,$(subst 1, 1,$(subst 2, 2,$(subst 3, 3,$(subst 4, 4,$(subst 5, 5,$(subst 6, 6,$(subst 7, 7,$(subst 8, 8,$(subst 9, 9,$(subst a, a,$(subst b, b,$(subst c, c,$(subst d, d,$(subst e, e,$(subst f, f,$1))))))))))))))))

######################################################################
# The following elisp code generates the tables for addition, subtraction
# and multiplication for the selected base. Currently the bases 8,10 and
# 16 are in use in gmtt.
#
# (let* ((base 8)
#        (i base)
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
#       (insert "add" (int-to-string base) "_pairs :=" add-pairs "\n"
#               "carry" (int-to-string base) "_pairs :=" carry-pairs "\n"
#               "$(foreach i,$(add" (int-to-string base) "_pairs),$(eval ad" (int-to-string base) "_$(i)))\n"
#               "$(foreach i,$(carry" (int-to-string base) "_pairs),$(eval cy" (int-to-string base) "_$(i)))\n"
#                "sub" (int-to-string base) "_pairs :=" sub-pairs "\n"
#               "borrow" (int-to-string base) "_pairs :=" borrow-pairs "\n"
#               "$(foreach i,$(sub" (int-to-string base) "_pairs),$(eval sb" (int-to-string base) "_$(i)))\n"
#               "$(foreach i,$(borrow" (int-to-string base) "_pairs),$(eval bw" (int-to-string base) "_$(i)))\n"
#               "mul" (int-to-string base) "_pairs :=" mul-pairs "\n"
#               "mulcarry" (int-to-string base) "_pairs :=" mulcarry-pairs "\n"
#               "$(foreach i,$(mul" (int-to-string base) "_pairs),$(eval ml" (int-to-string base) "_$(i)))\n"
#               "$(foreach i,$(mulcarry" (int-to-string base) "_pairs),$(eval mc" (int-to-string base) "_$(i)))\n"
#               "_add" (int-to-string base) " = $(call __add" (int-to-string base) ",$(foreach pair,$1,$(cy" (int-to-string base)
#                     "_$(pair))$(ad" (int-to-string base) "_$(pair))))\n"
#               "__add" (int-to-string base) " = $(if $(findstring y,$1),$(call __add" (int-to-string base)
#                     ",$(foreach pair,$(subst $(space)y,1 ,$(subst " (format "%x %x" (1- base) (1- base)) " y,y0 0 , $1)),$(cy"
#                     (int-to-string base) "_$(pair))$(ad" (int-to-string base) "_$(pair)))),$1)\n"
#               "_sub" (int-to-string base) " = $(call __sub" (int-to-string base) ",$(foreach pair,$1,$(bw" (int-to-string base)
#                     "_$(pair))$(sb" (int-to-string base) "_$(pair))))\n"
#               "__sub" (int-to-string base) " = $(if $(findstring z,$1),$(call __sub" (int-to-string base)
#                     ",$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z" (format "%x %x" (1- base) (1- base)) " , $1)),$(bw"
#                     (int-to-string base) "_$(pair))$(sb" (int-to-string base) "_$(pair)))),$1)\n"
#               "_mul" (int-to-string base) " = $(if $(word $(words $1),$2),$(call __mul" (int-to-string base)
#                     ",$1,0 $2),$(call __mul" (int-to-string base) ",$2,0 $1))\n"
#               "__mul" (int-to-string base) " = $(if $1,$(call _add" (int-to-string base) ",$(join $(call __mul"
#                     (int-to-string base) "_$(firstword $1),$2),0 $(call __mul" (int-to-string base) ",$(wordlist 2,64,$1),$2))))\n"
#               "__mul" (int-to-string base) "_0 = $(patsubst %,0,$1)\n"
#               "__mul" (int-to-string base) "_1 = $1\n")
#               (setq i 2)
#               (while (< i base)
#                   (insert (format "__mul%d_%x = $(call _add%d,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc%d_%x$(i)) $(ml%d_%x$(i)))))\n" base i base base i base i))
#                   (setq i (1+ i))))

add8_pairs := 7:=7 77:=6 76:=5 75:=4 74:=3 73:=2 72:=1 71:=0 70:=7 6:=6 67:=5 66:=4 65:=3 64:=2 63:=1 62:=0 61:=7 60:=6 5:=5 57:=4 56:=3 55:=2 54:=1 53:=0 52:=7 51:=6 50:=5 4:=4 47:=3 46:=2 45:=1 44:=0 43:=7 42:=6 41:=5 40:=4 3:=3 37:=2 36:=1 35:=0 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 27:=1 26:=0 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 17:=0 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
carry8_pairs := 7:= 77:=y 76:=y 75:=y 74:=y 73:=y 72:=y 71:=y 70:= 6:= 67:=y 66:=y 65:=y 64:=y 63:=y 62:=y 61:= 60:= 5:= 57:=y 56:=y 55:=y 54:=y 53:=y 52:= 51:= 50:= 4:= 47:=y 46:=y 45:=y 44:=y 43:= 42:= 41:= 40:= 3:= 37:=y 36:=y 35:=y 34:= 33:= 32:= 31:= 30:= 2:= 27:=y 26:=y 25:= 24:= 23:= 22:= 21:= 20:= 1:= 17:=y 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(add8_pairs),$(eval ad8_$(i)))
$(foreach i,$(carry8_pairs),$(eval cy8_$(i)))
sub8_pairs := 7:=7 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 67:=7 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 57:=6 56:=7 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 47:=5 46:=6 45:=7 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 37:=4 36:=5 35:=6 34:=7 33:=0 32:=1 31:=2 30:=3 2:=2 27:=3 26:=4 25:=5 24:=6 23:=7 22:=0 21:=1 20:=2 1:=1 17:=2 16:=3 15:=4 14:=5 13:=6 12:=7 11:=0 10:=1 0:=0 07:=1 06:=2 05:=3 04:=4 03:=5 02:=6 01:=7 00:=0
borrow8_pairs := 7:= 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(sub8_pairs),$(eval sb8_$(i)))
$(foreach i,$(borrow8_pairs),$(eval bw8_$(i)))
mul8_pairs := 7:=0 77:=1 76:=2 75:=3 74:=4 73:=5 72:=6 71:=7 70:=0 6:=0 67:=2 66:=4 65:=6 64:=0 63:=2 62:=4 61:=6 60:=0 5:=0 57:=3 56:=6 55:=1 54:=4 53:=7 52:=2 51:=5 50:=0 4:=0 47:=4 46:=0 45:=4 44:=0 43:=4 42:=0 41:=4 40:=0 3:=0 37:=5 36:=2 35:=7 34:=4 33:=1 32:=6 31:=3 30:=0 2:=0 27:=6 26:=4 25:=2 24:=0 23:=6 22:=4 21:=2 20:=0 1:=0 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
mulcarry8_pairs := 7:= 77:=6 76:=5 75:=4 74:=3 73:=2 72:=1 71:= 70:= 6:= 67:=5 66:=4 65:=3 64:=3 63:=2 62:=1 61:= 60:= 5:= 57:=4 56:=3 55:=3 54:=2 53:=1 52:=1 51:= 50:= 4:= 47:=3 46:=3 45:=2 44:=2 43:=1 42:=1 41:= 40:= 3:= 37:=2 36:=2 35:=1 34:=1 33:=1 32:= 31:= 30:= 2:= 27:=1 26:=1 25:=1 24:=1 23:= 22:= 21:= 20:= 1:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(mul8_pairs),$(eval ml8_$(i)))
$(foreach i,$(mulcarry8_pairs),$(eval mc8_$(i)))
_add8 = $(__gmtt-dbg-args)$(call __add8,$(foreach pair,$1,$(cy8_$(pair))$(ad8_$(pair))))
__add8 = $(if $(findstring y,$1),$(call __add8,$(foreach pair,$(subst $(space)y,1 ,$(subst 7 7 y,y0 0 , $1)),$(cy8_$(pair))$(ad8_$(pair)))),$1)
_sub8 = $(__gmtt-dbg-args)$(call __sub8,$(foreach pair,$1,$(bw8_$(pair))$(sb8_$(pair))))
__sub8 = $(if $(findstring z,$1),$(call __sub8,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z7 7 , $1)),$(bw8_$(pair))$(sb8_$(pair)))),$1)
_mul8 = $(__gmtt-dbg-args)$(if $(word $(words $1),$2),$(call __mul8,$1,0 $2),$(call __mul8,$2,0 $1))
__mul8 = $(if $1,$(call _add8,$(join $(call __mul8_$(firstword $1),$2),0 $(call __mul8,$(wordlist 2,64,$1),$2))))
__mul8_0 = $(patsubst %,0,$1)
__mul8_1 = $1
__mul8_2 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_2$(i)) $(ml8_2$(i)))))
__mul8_3 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_3$(i)) $(ml8_3$(i)))))
__mul8_4 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_4$(i)) $(ml8_4$(i)))))
__mul8_5 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_5$(i)) $(ml8_5$(i)))))
__mul8_6 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_6$(i)) $(ml8_6$(i)))))
__mul8_7 = $(call _add8,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc8_7$(i)) $(ml8_7$(i)))))

add16_pairs := f:=f ff:=e fe:=d fd:=c fc:=b fb:=a fa:=9 f9:=8 f8:=7 f7:=6 f6:=5 f5:=4 f4:=3 f3:=2 f2:=1 f1:=0 f0:=f e:=e ef:=d ee:=c ed:=b ec:=a eb:=9 ea:=8 e9:=7 e8:=6 e7:=5 e6:=4 e5:=3 e4:=2 e3:=1 e2:=0 e1:=f e0:=e d:=d df:=c de:=b dd:=a dc:=9 db:=8 da:=7 d9:=6 d8:=5 d7:=4 d6:=3 d5:=2 d4:=1 d3:=0 d2:=f d1:=e d0:=d c:=c cf:=b ce:=a cd:=9 cc:=8 cb:=7 ca:=6 c9:=5 c8:=4 c7:=3 c6:=2 c5:=1 c4:=0 c3:=f c2:=e c1:=d c0:=c b:=b bf:=a be:=9 bd:=8 bc:=7 bb:=6 ba:=5 b9:=4 b8:=3 b7:=2 b6:=1 b5:=0 b4:=f b3:=e b2:=d b1:=c b0:=b a:=a af:=9 ae:=8 ad:=7 ac:=6 ab:=5 aa:=4 a9:=3 a8:=2 a7:=1 a6:=0 a5:=f a4:=e a3:=d a2:=c a1:=b a0:=a 9:=9 9f:=8 9e:=7 9d:=6 9c:=5 9b:=4 9a:=3 99:=2 98:=1 97:=0 96:=f 95:=e 94:=d 93:=c 92:=b 91:=a 90:=9 8:=8 8f:=7 8e:=6 8d:=5 8c:=4 8b:=3 8a:=2 89:=1 88:=0 87:=f 86:=e 85:=d 84:=c 83:=b 82:=a 81:=9 80:=8 7:=7 7f:=6 7e:=5 7d:=4 7c:=3 7b:=2 7a:=1 79:=0 78:=f 77:=e 76:=d 75:=c 74:=b 73:=a 72:=9 71:=8 70:=7 6:=6 6f:=5 6e:=4 6d:=3 6c:=2 6b:=1 6a:=0 69:=f 68:=e 67:=d 66:=c 65:=b 64:=a 63:=9 62:=8 61:=7 60:=6 5:=5 5f:=4 5e:=3 5d:=2 5c:=1 5b:=0 5a:=f 59:=e 58:=d 57:=c 56:=b 55:=a 54:=9 53:=8 52:=7 51:=6 50:=5 4:=4 4f:=3 4e:=2 4d:=1 4c:=0 4b:=f 4a:=e 49:=d 48:=c 47:=b 46:=a 45:=9 44:=8 43:=7 42:=6 41:=5 40:=4 3:=3 3f:=2 3e:=1 3d:=0 3c:=f 3b:=e 3a:=d 39:=c 38:=b 37:=a 36:=9 35:=8 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 2f:=1 2e:=0 2d:=f 2c:=e 2b:=d 2a:=c 29:=b 28:=a 27:=9 26:=8 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 1f:=0 1e:=f 1d:=e 1c:=d 1b:=c 1a:=b 19:=a 18:=9 17:=8 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 0f:=f 0e:=e 0d:=d 0c:=c 0b:=b 0a:=a 09:=9 08:=8 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
carry16_pairs := f:= ff:=y fe:=y fd:=y fc:=y fb:=y fa:=y f9:=y f8:=y f7:=y f6:=y f5:=y f4:=y f3:=y f2:=y f1:=y f0:= e:= ef:=y ee:=y ed:=y ec:=y eb:=y ea:=y e9:=y e8:=y e7:=y e6:=y e5:=y e4:=y e3:=y e2:=y e1:= e0:= d:= df:=y de:=y dd:=y dc:=y db:=y da:=y d9:=y d8:=y d7:=y d6:=y d5:=y d4:=y d3:=y d2:= d1:= d0:= c:= cf:=y ce:=y cd:=y cc:=y cb:=y ca:=y c9:=y c8:=y c7:=y c6:=y c5:=y c4:=y c3:= c2:= c1:= c0:= b:= bf:=y be:=y bd:=y bc:=y bb:=y ba:=y b9:=y b8:=y b7:=y b6:=y b5:=y b4:= b3:= b2:= b1:= b0:= a:= af:=y ae:=y ad:=y ac:=y ab:=y aa:=y a9:=y a8:=y a7:=y a6:=y a5:= a4:= a3:= a2:= a1:= a0:= 9:= 9f:=y 9e:=y 9d:=y 9c:=y 9b:=y 9a:=y 99:=y 98:=y 97:=y 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 8f:=y 8e:=y 8d:=y 8c:=y 8b:=y 8a:=y 89:=y 88:=y 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 7f:=y 7e:=y 7d:=y 7c:=y 7b:=y 7a:=y 79:=y 78:= 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 6f:=y 6e:=y 6d:=y 6c:=y 6b:=y 6a:=y 69:= 68:= 67:= 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 5f:=y 5e:=y 5d:=y 5c:=y 5b:=y 5a:= 59:= 58:= 57:= 56:= 55:= 54:= 53:= 52:= 51:= 50:= 4:= 4f:=y 4e:=y 4d:=y 4c:=y 4b:= 4a:= 49:= 48:= 47:= 46:= 45:= 44:= 43:= 42:= 41:= 40:= 3:= 3f:=y 3e:=y 3d:=y 3c:= 3b:= 3a:= 39:= 38:= 37:= 36:= 35:= 34:= 33:= 32:= 31:= 30:= 2:= 2f:=y 2e:=y 2d:= 2c:= 2b:= 2a:= 29:= 28:= 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 1f:=y 1e:= 1d:= 1c:= 1b:= 1a:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 0f:= 0e:= 0d:= 0c:= 0b:= 0a:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(add16_pairs),$(eval ad16_$(i)))
$(foreach i,$(carry16_pairs),$(eval cy16_$(i)))
sub16_pairs := f:=f ff:=0 fe:=1 fd:=2 fc:=3 fb:=4 fa:=5 f9:=6 f8:=7 f7:=8 f6:=9 f5:=a f4:=b f3:=c f2:=d f1:=e f0:=f e:=e ef:=f ee:=0 ed:=1 ec:=2 eb:=3 ea:=4 e9:=5 e8:=6 e7:=7 e6:=8 e5:=9 e4:=a e3:=b e2:=c e1:=d e0:=e d:=d df:=e de:=f dd:=0 dc:=1 db:=2 da:=3 d9:=4 d8:=5 d7:=6 d6:=7 d5:=8 d4:=9 d3:=a d2:=b d1:=c d0:=d c:=c cf:=d ce:=e cd:=f cc:=0 cb:=1 ca:=2 c9:=3 c8:=4 c7:=5 c6:=6 c5:=7 c4:=8 c3:=9 c2:=a c1:=b c0:=c b:=b bf:=c be:=d bd:=e bc:=f bb:=0 ba:=1 b9:=2 b8:=3 b7:=4 b6:=5 b5:=6 b4:=7 b3:=8 b2:=9 b1:=a b0:=b a:=a af:=b ae:=c ad:=d ac:=e ab:=f aa:=0 a9:=1 a8:=2 a7:=3 a6:=4 a5:=5 a4:=6 a3:=7 a2:=8 a1:=9 a0:=a 9:=9 9f:=a 9e:=b 9d:=c 9c:=d 9b:=e 9a:=f 99:=0 98:=1 97:=2 96:=3 95:=4 94:=5 93:=6 92:=7 91:=8 90:=9 8:=8 8f:=9 8e:=a 8d:=b 8c:=c 8b:=d 8a:=e 89:=f 88:=0 87:=1 86:=2 85:=3 84:=4 83:=5 82:=6 81:=7 80:=8 7:=7 7f:=8 7e:=9 7d:=a 7c:=b 7b:=c 7a:=d 79:=e 78:=f 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 6f:=7 6e:=8 6d:=9 6c:=a 6b:=b 6a:=c 69:=d 68:=e 67:=f 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 5f:=6 5e:=7 5d:=8 5c:=9 5b:=a 5a:=b 59:=c 58:=d 57:=e 56:=f 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 4f:=5 4e:=6 4d:=7 4c:=8 4b:=9 4a:=a 49:=b 48:=c 47:=d 46:=e 45:=f 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 3f:=4 3e:=5 3d:=6 3c:=7 3b:=8 3a:=9 39:=a 38:=b 37:=c 36:=d 35:=e 34:=f 33:=0 32:=1 31:=2 30:=3 2:=2 2f:=3 2e:=4 2d:=5 2c:=6 2b:=7 2a:=8 29:=9 28:=a 27:=b 26:=c 25:=d 24:=e 23:=f 22:=0 21:=1 20:=2 1:=1 1f:=2 1e:=3 1d:=4 1c:=5 1b:=6 1a:=7 19:=8 18:=9 17:=a 16:=b 15:=c 14:=d 13:=e 12:=f 11:=0 10:=1 0:=0 0f:=1 0e:=2 0d:=3 0c:=4 0b:=5 0a:=6 09:=7 08:=8 07:=9 06:=a 05:=b 04:=c 03:=d 02:=e 01:=f 00:=0
borrow16_pairs := f:= ff:= fe:= fd:= fc:= fb:= fa:= f9:= f8:= f7:= f6:= f5:= f4:= f3:= f2:= f1:= f0:= e:= ef:=z ee:= ed:= ec:= eb:= ea:= e9:= e8:= e7:= e6:= e5:= e4:= e3:= e2:= e1:= e0:= d:= df:=z de:=z dd:= dc:= db:= da:= d9:= d8:= d7:= d6:= d5:= d4:= d3:= d2:= d1:= d0:= c:= cf:=z ce:=z cd:=z cc:= cb:= ca:= c9:= c8:= c7:= c6:= c5:= c4:= c3:= c2:= c1:= c0:= b:= bf:=z be:=z bd:=z bc:=z bb:= ba:= b9:= b8:= b7:= b6:= b5:= b4:= b3:= b2:= b1:= b0:= a:= af:=z ae:=z ad:=z ac:=z ab:=z aa:= a9:= a8:= a7:= a6:= a5:= a4:= a3:= a2:= a1:= a0:= 9:= 9f:=z 9e:=z 9d:=z 9c:=z 9b:=z 9a:=z 99:= 98:= 97:= 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 8f:=z 8e:=z 8d:=z 8c:=z 8b:=z 8a:=z 89:=z 88:= 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 7f:=z 7e:=z 7d:=z 7c:=z 7b:=z 7a:=z 79:=z 78:=z 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 6f:=z 6e:=z 6d:=z 6c:=z 6b:=z 6a:=z 69:=z 68:=z 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 5f:=z 5e:=z 5d:=z 5c:=z 5b:=z 5a:=z 59:=z 58:=z 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 4f:=z 4e:=z 4d:=z 4c:=z 4b:=z 4a:=z 49:=z 48:=z 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 3f:=z 3e:=z 3d:=z 3c:=z 3b:=z 3a:=z 39:=z 38:=z 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 2f:=z 2e:=z 2d:=z 2c:=z 2b:=z 2a:=z 29:=z 28:=z 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 1f:=z 1e:=z 1d:=z 1c:=z 1b:=z 1a:=z 19:=z 18:=z 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 0f:=z 0e:=z 0d:=z 0c:=z 0b:=z 0a:=z 09:=z 08:=z 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(sub16_pairs),$(eval sb16_$(i)))
$(foreach i,$(borrow16_pairs),$(eval bw16_$(i)))
mul16_pairs := f:=0 ff:=1 fe:=2 fd:=3 fc:=4 fb:=5 fa:=6 f9:=7 f8:=8 f7:=9 f6:=a f5:=b f4:=c f3:=d f2:=e f1:=f f0:=0 e:=0 ef:=2 ee:=4 ed:=6 ec:=8 eb:=a ea:=c e9:=e e8:=0 e7:=2 e6:=4 e5:=6 e4:=8 e3:=a e2:=c e1:=e e0:=0 d:=0 df:=3 de:=6 dd:=9 dc:=c db:=f da:=2 d9:=5 d8:=8 d7:=b d6:=e d5:=1 d4:=4 d3:=7 d2:=a d1:=d d0:=0 c:=0 cf:=4 ce:=8 cd:=c cc:=0 cb:=4 ca:=8 c9:=c c8:=0 c7:=4 c6:=8 c5:=c c4:=0 c3:=4 c2:=8 c1:=c c0:=0 b:=0 bf:=5 be:=a bd:=f bc:=4 bb:=9 ba:=e b9:=3 b8:=8 b7:=d b6:=2 b5:=7 b4:=c b3:=1 b2:=6 b1:=b b0:=0 a:=0 af:=6 ae:=c ad:=2 ac:=8 ab:=e aa:=4 a9:=a a8:=0 a7:=6 a6:=c a5:=2 a4:=8 a3:=e a2:=4 a1:=a a0:=0 9:=0 9f:=7 9e:=e 9d:=5 9c:=c 9b:=3 9a:=a 99:=1 98:=8 97:=f 96:=6 95:=d 94:=4 93:=b 92:=2 91:=9 90:=0 8:=0 8f:=8 8e:=0 8d:=8 8c:=0 8b:=8 8a:=0 89:=8 88:=0 87:=8 86:=0 85:=8 84:=0 83:=8 82:=0 81:=8 80:=0 7:=0 7f:=9 7e:=2 7d:=b 7c:=4 7b:=d 7a:=6 79:=f 78:=8 77:=1 76:=a 75:=3 74:=c 73:=5 72:=e 71:=7 70:=0 6:=0 6f:=a 6e:=4 6d:=e 6c:=8 6b:=2 6a:=c 69:=6 68:=0 67:=a 66:=4 65:=e 64:=8 63:=2 62:=c 61:=6 60:=0 5:=0 5f:=b 5e:=6 5d:=1 5c:=c 5b:=7 5a:=2 59:=d 58:=8 57:=3 56:=e 55:=9 54:=4 53:=f 52:=a 51:=5 50:=0 4:=0 4f:=c 4e:=8 4d:=4 4c:=0 4b:=c 4a:=8 49:=4 48:=0 47:=c 46:=8 45:=4 44:=0 43:=c 42:=8 41:=4 40:=0 3:=0 3f:=d 3e:=a 3d:=7 3c:=4 3b:=1 3a:=e 39:=b 38:=8 37:=5 36:=2 35:=f 34:=c 33:=9 32:=6 31:=3 30:=0 2:=0 2f:=e 2e:=c 2d:=a 2c:=8 2b:=6 2a:=4 29:=2 28:=0 27:=e 26:=c 25:=a 24:=8 23:=6 22:=4 21:=2 20:=0 1:=0 1f:=f 1e:=e 1d:=d 1c:=c 1b:=b 1a:=a 19:=9 18:=8 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 0f:=0 0e:=0 0d:=0 0c:=0 0b:=0 0a:=0 09:=0 08:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
mulcarry16_pairs := f:= ff:=e fe:=d fd:=c fc:=b fb:=a fa:=9 f9:=8 f8:=7 f7:=6 f6:=5 f5:=4 f4:=3 f3:=2 f2:=1 f1:= f0:= e:= ef:=d ee:=c ed:=b ec:=a eb:=9 ea:=8 e9:=7 e8:=7 e7:=6 e6:=5 e5:=4 e4:=3 e3:=2 e2:=1 e1:= e0:= d:= df:=c de:=b dd:=a dc:=9 db:=8 da:=8 d9:=7 d8:=6 d7:=5 d6:=4 d5:=4 d4:=3 d3:=2 d2:=1 d1:= d0:= c:= cf:=b ce:=a cd:=9 cc:=9 cb:=8 ca:=7 c9:=6 c8:=6 c7:=5 c6:=4 c5:=3 c4:=3 c3:=2 c2:=1 c1:= c0:= b:= bf:=a be:=9 bd:=8 bc:=8 bb:=7 ba:=6 b9:=6 b8:=5 b7:=4 b6:=4 b5:=3 b4:=2 b3:=2 b2:=1 b1:= b0:= a:= af:=9 ae:=8 ad:=8 ac:=7 ab:=6 aa:=6 a9:=5 a8:=5 a7:=4 a6:=3 a5:=3 a4:=2 a3:=1 a2:=1 a1:= a0:= 9:= 9f:=8 9e:=7 9d:=7 9c:=6 9b:=6 9a:=5 99:=5 98:=4 97:=3 96:=3 95:=2 94:=2 93:=1 92:=1 91:= 90:= 8:= 8f:=7 8e:=7 8d:=6 8c:=6 8b:=5 8a:=5 89:=4 88:=4 87:=3 86:=3 85:=2 84:=2 83:=1 82:=1 81:= 80:= 7:= 7f:=6 7e:=6 7d:=5 7c:=5 7b:=4 7a:=4 79:=3 78:=3 77:=3 76:=2 75:=2 74:=1 73:=1 72:= 71:= 70:= 6:= 6f:=5 6e:=5 6d:=4 6c:=4 6b:=4 6a:=3 69:=3 68:=3 67:=2 66:=2 65:=1 64:=1 63:=1 62:= 61:= 60:= 5:= 5f:=4 5e:=4 5d:=4 5c:=3 5b:=3 5a:=3 59:=2 58:=2 57:=2 56:=1 55:=1 54:=1 53:= 52:= 51:= 50:= 4:= 4f:=3 4e:=3 4d:=3 4c:=3 4b:=2 4a:=2 49:=2 48:=2 47:=1 46:=1 45:=1 44:=1 43:= 42:= 41:= 40:= 3:= 3f:=2 3e:=2 3d:=2 3c:=2 3b:=2 3a:=1 39:=1 38:=1 37:=1 36:=1 35:= 34:= 33:= 32:= 31:= 30:= 2:= 2f:=1 2e:=1 2d:=1 2c:=1 2b:=1 2a:=1 29:=1 28:=1 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 1f:= 1e:= 1d:= 1c:= 1b:= 1a:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 0f:= 0e:= 0d:= 0c:= 0b:= 0a:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(mul16_pairs),$(eval ml16_$(i)))
$(foreach i,$(mulcarry16_pairs),$(eval mc16_$(i)))
_add16 = $(call __add16,$(foreach pair,$1,$(cy16_$(pair))$(ad16_$(pair))))
__add16 = $(if $(findstring y,$1),$(call __add16,$(foreach pair,$(subst $(space)y,1 ,$(subst f f y,y0 0 , $1)),$(cy16_$(pair))$(ad16_$(pair)))),$1)
_sub16 = $(__gmtt-dbg-args)$(call __sub16,$(foreach pair,$1,$(bw16_$(pair))$(sb16_$(pair))))
__sub16 = $(if $(findstring z,$1),$(call __sub16,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,zf f , $1)),$(bw16_$(pair))$(sb16_$(pair)))),$1)
_mul16 = $(if $(word $(words $1),$2),$(call __mul16,$1,0 $2),$(call __mul16,$2,0 $1))
__mul16 = $(if $1,$(call _add16,$(join $(call __mul16_$(firstword $1),$2),0 $(call __mul16,$(wordlist 2,64,$1),$2))))
__mul16_0 = $(patsubst %,0,$1)
__mul16_1 = $1
__mul16_2 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_2$(i)) $(ml16_2$(i)))))
__mul16_3 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_3$(i)) $(ml16_3$(i)))))
__mul16_4 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_4$(i)) $(ml16_4$(i)))))
__mul16_5 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_5$(i)) $(ml16_5$(i)))))
__mul16_6 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_6$(i)) $(ml16_6$(i)))))
__mul16_7 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_7$(i)) $(ml16_7$(i)))))
__mul16_8 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_8$(i)) $(ml16_8$(i)))))
__mul16_9 = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_9$(i)) $(ml16_9$(i)))))
__mul16_a = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_a$(i)) $(ml16_a$(i)))))
__mul16_b = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_b$(i)) $(ml16_b$(i)))))
__mul16_c = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_c$(i)) $(ml16_c$(i)))))
__mul16_d = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_d$(i)) $(ml16_d$(i)))))
__mul16_e = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_e$(i)) $(ml16_e$(i)))))
__mul16_f = $(call _add16,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc16_f$(i)) $(ml16_f$(i)))))


add10_pairs := 9:=9 99:=8 98:=7 97:=6 96:=5 95:=4 94:=3 93:=2 92:=1 91:=0 90:=9 8:=8 89:=7 88:=6 87:=5 86:=4 85:=3 84:=2 83:=1 82:=0 81:=9 80:=8 7:=7 79:=6 78:=5 77:=4 76:=3 75:=2 74:=1 73:=0 72:=9 71:=8 70:=7 6:=6 69:=5 68:=4 67:=3 66:=2 65:=1 64:=0 63:=9 62:=8 61:=7 60:=6 5:=5 59:=4 58:=3 57:=2 56:=1 55:=0 54:=9 53:=8 52:=7 51:=6 50:=5 4:=4 49:=3 48:=2 47:=1 46:=0 45:=9 44:=8 43:=7 42:=6 41:=5 40:=4 3:=3 39:=2 38:=1 37:=0 36:=9 35:=8 34:=7 33:=6 32:=5 31:=4 30:=3 2:=2 29:=1 28:=0 27:=9 26:=8 25:=7 24:=6 23:=5 22:=4 21:=3 20:=2 1:=1 19:=0 18:=9 17:=8 16:=7 15:=6 14:=5 13:=4 12:=3 11:=2 10:=1 0:=0 09:=9 08:=8 07:=7 06:=6 05:=5 04:=4 03:=3 02:=2 01:=1 00:=0
carry10_pairs := 9:= 99:=y 98:=y 97:=y 96:=y 95:=y 94:=y 93:=y 92:=y 91:=y 90:= 8:= 89:=y 88:=y 87:=y 86:=y 85:=y 84:=y 83:=y 82:=y 81:= 80:= 7:= 79:=y 78:=y 77:=y 76:=y 75:=y 74:=y 73:=y 72:= 71:= 70:= 6:= 69:=y 68:=y 67:=y 66:=y 65:=y 64:=y 63:= 62:= 61:= 60:= 5:= 59:=y 58:=y 57:=y 56:=y 55:=y 54:= 53:= 52:= 51:= 50:= 4:= 49:=y 48:=y 47:=y 46:=y 45:= 44:= 43:= 42:= 41:= 40:= 3:= 39:=y 38:=y 37:=y 36:= 35:= 34:= 33:= 32:= 31:= 30:= 2:= 29:=y 28:=y 27:= 26:= 25:= 24:= 23:= 22:= 21:= 20:= 1:= 19:=y 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(add10_pairs),$(eval ad10_$(i)))
$(foreach i,$(carry10_pairs),$(eval cy10_$(i)))
sub10_pairs := 9:=9 99:=0 98:=1 97:=2 96:=3 95:=4 94:=5 93:=6 92:=7 91:=8 90:=9 8:=8 89:=9 88:=0 87:=1 86:=2 85:=3 84:=4 83:=5 82:=6 81:=7 80:=8 7:=7 79:=8 78:=9 77:=0 76:=1 75:=2 74:=3 73:=4 72:=5 71:=6 70:=7 6:=6 69:=7 68:=8 67:=9 66:=0 65:=1 64:=2 63:=3 62:=4 61:=5 60:=6 5:=5 59:=6 58:=7 57:=8 56:=9 55:=0 54:=1 53:=2 52:=3 51:=4 50:=5 4:=4 49:=5 48:=6 47:=7 46:=8 45:=9 44:=0 43:=1 42:=2 41:=3 40:=4 3:=3 39:=4 38:=5 37:=6 36:=7 35:=8 34:=9 33:=0 32:=1 31:=2 30:=3 2:=2 29:=3 28:=4 27:=5 26:=6 25:=7 24:=8 23:=9 22:=0 21:=1 20:=2 1:=1 19:=2 18:=3 17:=4 16:=5 15:=6 14:=7 13:=8 12:=9 11:=0 10:=1 0:=0 09:=1 08:=2 07:=3 06:=4 05:=5 04:=6 03:=7 02:=8 01:=9 00:=0
borrow10_pairs := 9:= 99:= 98:= 97:= 96:= 95:= 94:= 93:= 92:= 91:= 90:= 8:= 89:=z 88:= 87:= 86:= 85:= 84:= 83:= 82:= 81:= 80:= 7:= 79:=z 78:=z 77:= 76:= 75:= 74:= 73:= 72:= 71:= 70:= 6:= 69:=z 68:=z 67:=z 66:= 65:= 64:= 63:= 62:= 61:= 60:= 5:= 59:=z 58:=z 57:=z 56:=z 55:= 54:= 53:= 52:= 51:= 50:= 4:= 49:=z 48:=z 47:=z 46:=z 45:=z 44:= 43:= 42:= 41:= 40:= 3:= 39:=z 38:=z 37:=z 36:=z 35:=z 34:=z 33:= 32:= 31:= 30:= 2:= 29:=z 28:=z 27:=z 26:=z 25:=z 24:=z 23:=z 22:= 21:= 20:= 1:= 19:=z 18:=z 17:=z 16:=z 15:=z 14:=z 13:=z 12:=z 11:= 10:= 0:= 09:=z 08:=z 07:=z 06:=z 05:=z 04:=z 03:=z 02:=z 01:=z 00:=
$(foreach i,$(sub10_pairs),$(eval sb10_$(i)))
$(foreach i,$(borrow10_pairs),$(eval bw10_$(i)))
mul10_pairs := 9:=0 99:=1 98:=2 97:=3 96:=4 95:=5 94:=6 93:=7 92:=8 91:=9 90:=0 8:=0 89:=2 88:=4 87:=6 86:=8 85:=0 84:=2 83:=4 82:=6 81:=8 80:=0 7:=0 79:=3 78:=6 77:=9 76:=2 75:=5 74:=8 73:=1 72:=4 71:=7 70:=0 6:=0 69:=4 68:=8 67:=2 66:=6 65:=0 64:=4 63:=8 62:=2 61:=6 60:=0 5:=0 59:=5 58:=0 57:=5 56:=0 55:=5 54:=0 53:=5 52:=0 51:=5 50:=0 4:=0 49:=6 48:=2 47:=8 46:=4 45:=0 44:=6 43:=2 42:=8 41:=4 40:=0 3:=0 39:=7 38:=4 37:=1 36:=8 35:=5 34:=2 33:=9 32:=6 31:=3 30:=0 2:=0 29:=8 28:=6 27:=4 26:=2 25:=0 24:=8 23:=6 22:=4 21:=2 20:=0 1:=0 19:=9 18:=8 17:=7 16:=6 15:=5 14:=4 13:=3 12:=2 11:=1 10:=0 0:=0 09:=0 08:=0 07:=0 06:=0 05:=0 04:=0 03:=0 02:=0 01:=0 00:=0
mulcarry10_pairs := 9:= 99:=8 98:=7 97:=6 96:=5 95:=4 94:=3 93:=2 92:=1 91:= 90:= 8:= 89:=7 88:=6 87:=5 86:=4 85:=4 84:=3 83:=2 82:=1 81:= 80:= 7:= 79:=6 78:=5 77:=4 76:=4 75:=3 74:=2 73:=2 72:=1 71:= 70:= 6:= 69:=5 68:=4 67:=4 66:=3 65:=3 64:=2 63:=1 62:=1 61:= 60:= 5:= 59:=4 58:=4 57:=3 56:=3 55:=2 54:=2 53:=1 52:=1 51:= 50:= 4:= 49:=3 48:=3 47:=2 46:=2 45:=2 44:=1 43:=1 42:= 41:= 40:= 3:= 39:=2 38:=2 37:=2 36:=1 35:=1 34:=1 33:= 32:= 31:= 30:= 2:= 29:=1 28:=1 27:=1 26:=1 25:=1 24:= 23:= 22:= 21:= 20:= 1:= 19:= 18:= 17:= 16:= 15:= 14:= 13:= 12:= 11:= 10:= 0:= 09:= 08:= 07:= 06:= 05:= 04:= 03:= 02:= 01:= 00:=
$(foreach i,$(mul10_pairs),$(eval ml10_$(i)))
$(foreach i,$(mulcarry10_pairs),$(eval mc10_$(i)))
_add10 = $(__gmtt-dbg-args)$(call __add10,$(foreach pair,$1,$(cy10_$(pair))$(ad10_$(pair))))
__add10 = $(if $(findstring y,$1),$(call __add10,$(foreach pair,$(subst $(space)y,1 ,$(subst 9 9 y,y0 0 , $1)),$(cy10_$(pair))$(ad10_$(pair)))),$1)
_sub10 = $(__gmtt-dbg-args)$(call __sub10,$(foreach pair,$1,$(bw10_$(pair))$(sb10_$(pair))))
__sub10 = $(if $(findstring z,$1),$(call __sub10,$(foreach pair,$(subst $(space)z,1 ,$(subst 0 0 z,z9 9 , $1)),$(bw10_$(pair))$(sb10_$(pair)))),$1)
_mul10 = $(__gmtt-dbg-args)$(if $(word $(words $1),$2),$(call __mul10,$1,0 $2),$(call __mul10,$2,0 $1))
__mul10 = $(if $1,$(call _add10,$(join $(call __mul10_$(firstword $1),$2),0 $(call __mul10,$(wordlist 2,64,$1),$2))))
__mul10_0 = $(patsubst %,0,$1)
__mul10_1 = $1
__mul10_2 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_2$(i)) $(ml10_2$(i)))))
__mul10_3 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_3$(i)) $(ml10_3$(i)))))
__mul10_4 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_4$(i)) $(ml10_4$(i)))))
__mul10_5 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_5$(i)) $(ml10_5$(i)))))
__mul10_6 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_6$(i)) $(ml10_6$(i)))))
__mul10_7 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_7$(i)) $(ml10_7$(i)))))
__mul10_8 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_8$(i)) $(ml10_8$(i)))))
__mul10_9 = $(call _add10,$(subst $(space)_,,$(space)$(foreach i,$1,_$(mc10_9$(i)) $(ml10_9$(i)))))


# Remove all leading 0's
# $1 = numeric literal with leading 0's
_nrmlz = $(if $1,$(if $(patsubst 00%,,$1),$(if $(patsubst 0%,,$1),$1,$(call _nrmlz,$(patsubst 0%,%,$1))),$(call _nrmlz,$(patsubst 00%,%,$1))),0)

_umul10 = $(call _nrmlz,$(subst $(space),,$(call _mul10,$(call _xpld-num,$1),$(call _xpld-num,$2))))
_uadd10 = $(call _nrmlz,$(subst $(space),,$(call _add10,$(call _shift-merge,$(call _xpld-num,$1),$(call _xpld-num,$2)))))
_usub10 = $(call _nrmlz,$(subst $(space),,$(call _sub10,$(call _shift-merge,$(call _xpld-num,$1),$(call _xpld-num,$2)))))


# Compare two unsigned numbers (given as lists)
# $1 = first number as list
# $2 = second number as list
_ucmp = $(__gmtt-dbg-args)$(if $(word $(words $2),$1),$(if $(word $(words $1),$2),$(call __ucmp,$(firstword $(filter-out 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff,$(join $1,$2)))),>),<)
__ucmp = $(__gmtt-dbg-args)$(if $1,$(if $(bw16_$1),<,>),=)



# Split a numeric literal into a base id (8,10 or 16) and the digits without base indicator
# and also strip the sign
# Example: _analyze(- 0x0123 ) -> 16 0123
#          _analyze(+123 ) -> 10 123
#          _analyze(-000123) -> 8 00123
_analyze = $(if $(filter 0,$1),10 0,$(subst .,10 ,$(patsubst .0%,8 %,$(patsubst .0x%,16 %,$(patsubst .0X%,16 %,.$(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst -,,$(subst +,,$(subst $(space),,$1))))))))))))))


# Convert a number in list form to a "normal" number with prefix and optional sign
# $1 = number as list
# $2 = '-' or empty string
_fmt16 = $(if $(filter-out 0,$1),$2)0x$(call _nrmlz,$(subst $(space),,$1))
_fmt10 = $(if $(filter-out 0,$1),$2)$(call _nrmlz,$(subst $(space),,$1))
_fmt8 = $(if $(filter-out 0,$1),$2)0$(call _nrmlz,$(subst $(space),,$1))



# <empty string> := $1 > $2
# b := $1 < $2
# eq := $1 = $2
bw10_ := eq
_udivcmp = $(bw10_$(firstword $(filter-out 0 00 11 22 33 44 55 66 77 88 99,$(join $1,$2))))


_10to10 = $(__gmtt-dbg-args)$1
_8to8 = $(__gmtt-dbg-args)$1
_16to16 = $(__gmtt-dbg-args)$1

_16to8 = $(__gmtt-dbg-args)$(if $1,$(call _16to8,$(wordlist 2,64,$1),$(call _add8,$(call _shift-merge,$(_16to8$(firstword $1)),$(call __mul8_4,$(call __mul8_4,$2))))),$2)
_16to80 := 0
_16to81 := 1
_16to82 := 2
_16to83 := 3
_16to84 := 4
_16to85 := 5
_16to86 := 6
_16to87 := 7
_16to88 := 1 0
_16to89 := 1 1
_16to8a := 1 2
_16to8b := 1 3
_16to8c := 1 4
_16to8d := 1 5
_16to8e := 1 6
_16to8f := 1 7

_16to10 = $(__gmtt-dbg-args)$(if $1,$(call _16to10,$(wordlist 2,64,$1),$(call _add10,$(call _shift-merge,$(_16to10$(firstword $1)),$(call __mul10_2,$(call __mul10_8,$2))))),$2)
_16to100 := 0
_16to101 := 1	 
_16to102 := 2	 
_16to103 := 3	 
_16to104 := 4	 
_16to105 := 5	 
_16to106 := 6	 
_16to107 := 7	 
_16to108 := 8	 
_16to109 := 9	 
_16to10a := 1 0
_16to10b := 1 1
_16to10c := 1 2
_16to10d := 1 3
_16to10e := 1 4
_16to10f := 1 5

_8to10 = $(__gmtt-dbg-args)$(if $1,$(call _8to10,$(wordlist 2,64,$1),$(call _add10,$(call _shift-merge,$(firstword $1),$(call __mul10_8,$2)))),$2)

_8to16 = $(__gmtt-dbg-args)$(if $1,$(call _8to16,$(wordlist 2,64,$1),$(call _add16,$(call _shift-merge,$(firstword $1),$(call __mul16_8,$2)))),$2)

_10to16 = $(__gmtt-dbg-args)$(if $1,$(call _10to16,$(wordlist 2,64,$1),$(call _add16,$(call _shift-merge,$(firstword $1),$(call __mul16_a,$2)))),$2)

_10to8 = $(__gmtt-dbg-args)$(if $1,$(call _10to8,$(wordlist 2,64,$1),$(call _add8,$(call _shift-merge,$(_10to8$(firstword $1)),$(call __mul8_2,$(call __mul8_5,$2))))),$2)
_10to80 := 0
_10to81 := 1	 
_10to82 := 2	 
_10to83 := 3	 
_10to84 := 4	 
_10to85 := 5	 
_10to86 := 6	 
_10to87 := 7	 
_10to88 := 1 0	 
_10to89 := 1 1	 


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
_div10_1 = $(__gmtt-dbg-args)$(call __div10_1$(_udivcmp),$1,$2,$3)
__div10_1 = $(__gmtt-dbg-args)$(call _div10_5,$(call _sub10,$(join $1,$2)),$(call __mul10_4,$2),$2,$3)
__div10_1z = $(__gmtt-dbg-args)0 $(if $3,$(call _div10_1,$1,0 $2,$(wordlist 2,64,$3)))
__div10_1eq = $(__gmtt-dbg-args)1 $3


# $1 = a-b
# $2 = 4*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_5 = $(__gmtt-dbg-args)$(call __div10_5$(_udivcmp),$1,$2,$3,$4)
__div10_5 = $(__gmtt-dbg-args)$(call _div10_7,$(call _sub10,$(join $1,$2)),$(call __mul10_2,$3),$3,$4)
__div10_5z = $(__gmtt-dbg-args)$(call _div10_3,$1,$(call __mul10_2,$3),$3,$4)
__div10_5eq = $(__gmtt-dbg-args)5 $4

# $1 = a-b
# $2 = 2*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_3 = $(__gmtt-dbg-args)$(call __div10_3$(_udivcmp),$1,$2,$3,$4)
__div10_3 = $(__gmtt-dbg-args)$(call _div10_4,$(call _sub10,$(join $1,$2)),$3,$3,$4)
__div10_3z = $(__gmtt-dbg-args)$(call _div10_2,$1,$3,$3,$4)
__div10_3eq = $(__gmtt-dbg-args)3 $4

# $1 = a-b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_2 = $(__gmtt-dbg-args)$(call __div10_2$(_udivcmp),$1,$2,$3,$4)
__div10_2 = $(__gmtt-dbg-args)2 $(if $4,$(call _div10_1,$(call _sub10,$(join $1,$2)),0 $2,$(wordlist 2,64,$4)))
__div10_2z = $(__gmtt-dbg-args)1 $(if $4,$(call _div10_1,$1,0 $3,$(wordlist 2,64,$4)))
__div10_2eq = $(__gmtt-dbg-args)2 $4

# $1 = a-b-2b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_4 = $(__gmtt-dbg-args)$(call __div10_4$(_udivcmp),$1,$2,$3,$4)
__div10_4 = $(__gmtt-dbg-args)4 $(if $4,$(call _div10_1,$(call _sub10,$(join $1,$2)),0 $3,$(wordlist 2,64,$4)))
__div10_4z = $(__gmtt-dbg-args)3 $(if $4,$(call _div10_1,$1,0 $3,$(wordlist 2,64,$4)))
__div10_4eq = $(__gmtt-dbg-args)4 $4

# $1 = a-b-4b
# $2 = 2*b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_7 = $(__gmtt-dbg-args)$(call __div10_7$(_udivcmp),$1,$2,$3,$4)
__div10_7 = $(__gmtt-dbg-args)$(call _div10_8,$(call _sub10,$(join $1,$2)),$3,$3,$4)
__div10_7z = $(__gmtt-dbg-args)$(call _div10_6,$1,$3,$3,$4)
__div10_7eq = $(__gmtt-dbg-args)7 $4


# $1 = a-b-4b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_6 = $(__gmtt-dbg-args)$(call __div10_6$(_udivcmp),$1,$2,$3,$4)
__div10_6 = $(__gmtt-dbg-args)6 $(if $4,$(call _div10_1,$(call _sub10,$(join $1,$2)),0 $2,$(wordlist 2,64,$4)))
__div10_6z = $(__gmtt-dbg-args)5 $(if $4,$(call _div10_1,$1,0 $2,$(wordlist 2,64,$4)))
__div10_6eq = $(__gmtt-dbg-args)6 $4

# $1 = a-b-4b-2b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_8 = $(__gmtt-dbg-args)$(call __div10_8$(_udivcmp),$1,$2,$3,$4)
__div10_8 = $(__gmtt-dbg-args)$(call _div10_9,$(call _sub10,$(join $1,$2)),$3,$3,$4)
__div10_8z = $(__gmtt-dbg-args)7 $(if $4,$(call _div10_1,$1,0 $2,$(wordlist 2,64,$4)))
__div10_8eq = $(__gmtt-dbg-args)8 $4

# $1 = a-b-4b-2b-b
# $2 = b
# $3 = b
# $4 = current power of 10 of b as list of 0's
_div10_9 = $(__gmtt-dbg-args)$(call __div10_9$(_udivcmp),$1,$2,$3,$4)
__div10_9 = $(__gmtt-dbg-args)9 $(if $4,$(call _div10_1,$(call _sub10,$(join $1,$2)),0 $2,$(wordlist 2,64,$4)))
__div10_9z = $(__gmtt-dbg-args)8 $(if $4,$(call _div10_1,$1,0 $2,$(wordlist 2,64,$4)))
__div10_9eq = $(__gmtt-dbg-args)9 $4


# $1 = a
# $2 = b
# => a*b
mul = $(__gmtt-dbg-args)$(call _mul,$(call _analyze,$1),$(call _analyze,$2),$(subst --,,$(findstring -,$1)$(findstring -,$2)))
_mul = $(__gmtt-dbg-args)$(call _fmt$(firstword $1),$(call _mul$(firstword $1),$(call _xpld-num,$(call _nrmlz,$(lastword $1))),$(call _$(firstword $2)to$(firstword $1),$(call _xpld-num,$(call _nrmlz,$(lastword $2))))),$3)



# Create a two's-complement representation of the given number
# $1 = hexadecimal number as list
_twoscompl16 = $(__gmtt-dbg-args)$(wordlist 2,64,$(call _sub16,$(join 1 $(patsubst %,0,$1),0 $1)))
_onescompl16 = $(__gmtt-dbg-args)$(call _sub16,$(join $(patsubst %,f,$1),$1))

log2 = $(__gmtt-dbg-args)$(call _log2,$(call _analyze,$1))
_log2 = $(__gmtt-dbg-args)$(call __log2,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))))
__log2 = $(words $(__log2$(firstword $1)) $(foreach i,$(wordlist 2,64,$1),_ _ _ _))
__log20 :=
__log21 := _
__log22 := _
__log23 := _ _
__log24 := _ _
__log25 := _ _ _
__log26 := _ _ _
__log27 := _ _ _
__log28 := _ _ _
__log29 := _ _ _ _
__log2a := _ _ _ _
__log2b := _ _ _ _
__log2c := _ _ _ _
__log2d := _ _ _ _
__log2e := _ _ _ _
__log2f := _ _ _ _

######################################################################
# The following elisp code generates the tables for binary and, or and
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
#    (insert "_and-pairs :=" and-pairs "\n$(foreach i,$(_and-pairs),$(eval __band$(i)))\n"
#            "_or-pairs :=" or-pairs "\n$(foreach i,$(_or-pairs),$(eval __bor$(i)))\n"
#            "_xor-pairs :=" xor-pairs "\n$(foreach i,$(_xor-pairs),$(eval __bxor$(i)))\n"))

_and-pairs :=00:=0 01:=0 02:=0 03:=0 04:=0 05:=0 06:=0 07:=0 08:=0 09:=0 0a:=0 0b:=0 0c:=0 0d:=0 0e:=0 0f:=0 10:=0 11:=1 12:=0 13:=1 14:=0 15:=1 16:=0 17:=1 18:=0 19:=1 1a:=0 1b:=1 1c:=0 1d:=1 1e:=0 1f:=1 20:=0 21:=0 22:=2 23:=2 24:=0 25:=0 26:=2 27:=2 28:=0 29:=0 2a:=2 2b:=2 2c:=0 2d:=0 2e:=2 2f:=2 30:=0 31:=1 32:=2 33:=3 34:=0 35:=1 36:=2 37:=3 38:=0 39:=1 3a:=2 3b:=3 3c:=0 3d:=1 3e:=2 3f:=3 40:=0 41:=0 42:=0 43:=0 44:=4 45:=4 46:=4 47:=4 48:=0 49:=0 4a:=0 4b:=0 4c:=4 4d:=4 4e:=4 4f:=4 50:=0 51:=1 52:=0 53:=1 54:=4 55:=5 56:=4 57:=5 58:=0 59:=1 5a:=0 5b:=1 5c:=4 5d:=5 5e:=4 5f:=5 60:=0 61:=0 62:=2 63:=2 64:=4 65:=4 66:=6 67:=6 68:=0 69:=0 6a:=2 6b:=2 6c:=4 6d:=4 6e:=6 6f:=6 70:=0 71:=1 72:=2 73:=3 74:=4 75:=5 76:=6 77:=7 78:=0 79:=1 7a:=2 7b:=3 7c:=4 7d:=5 7e:=6 7f:=7 80:=0 81:=0 82:=0 83:=0 84:=0 85:=0 86:=0 87:=0 88:=8 89:=8 8a:=8 8b:=8 8c:=8 8d:=8 8e:=8 8f:=8 90:=0 91:=1 92:=0 93:=1 94:=0 95:=1 96:=0 97:=1 98:=8 99:=9 9a:=8 9b:=9 9c:=8 9d:=9 9e:=8 9f:=9 a0:=0 a1:=0 a2:=2 a3:=2 a4:=0 a5:=0 a6:=2 a7:=2 a8:=8 a9:=8 aa:=a ab:=a ac:=8 ad:=8 ae:=a af:=a b0:=0 b1:=1 b2:=2 b3:=3 b4:=0 b5:=1 b6:=2 b7:=3 b8:=8 b9:=9 ba:=a bb:=b bc:=8 bd:=9 be:=a bf:=b c0:=0 c1:=0 c2:=0 c3:=0 c4:=4 c5:=4 c6:=4 c7:=4 c8:=8 c9:=8 ca:=8 cb:=8 cc:=c cd:=c ce:=c cf:=c d0:=0 d1:=1 d2:=0 d3:=1 d4:=4 d5:=5 d6:=4 d7:=5 d8:=8 d9:=9 da:=8 db:=9 dc:=c dd:=d de:=c df:=d e0:=0 e1:=0 e2:=2 e3:=2 e4:=4 e5:=4 e6:=6 e7:=6 e8:=8 e9:=8 ea:=a eb:=a ec:=c ed:=c ee:=e ef:=e f0:=0 f1:=1 f2:=2 f3:=3 f4:=4 f5:=5 f6:=6 f7:=7 f8:=8 f9:=9 fa:=a fb:=b fc:=c fd:=d fe:=e ff:=f 
$(foreach i,$(_and-pairs),$(eval __band$(i)))
_or-pairs :=00:=0 01:=1 02:=2 03:=3 04:=4 05:=5 06:=6 07:=7 08:=8 09:=9 0a:=a 0b:=b 0c:=c 0d:=d 0e:=e 0f:=f 10:=1 11:=1 12:=3 13:=3 14:=5 15:=5 16:=7 17:=7 18:=9 19:=9 1a:=b 1b:=b 1c:=d 1d:=d 1e:=f 1f:=f 20:=2 21:=3 22:=2 23:=3 24:=6 25:=7 26:=6 27:=7 28:=a 29:=b 2a:=a 2b:=b 2c:=e 2d:=f 2e:=e 2f:=f 30:=3 31:=3 32:=3 33:=3 34:=7 35:=7 36:=7 37:=7 38:=b 39:=b 3a:=b 3b:=b 3c:=f 3d:=f 3e:=f 3f:=f 40:=4 41:=5 42:=6 43:=7 44:=4 45:=5 46:=6 47:=7 48:=c 49:=d 4a:=e 4b:=f 4c:=c 4d:=d 4e:=e 4f:=f 50:=5 51:=5 52:=7 53:=7 54:=5 55:=5 56:=7 57:=7 58:=d 59:=d 5a:=f 5b:=f 5c:=d 5d:=d 5e:=f 5f:=f 60:=6 61:=7 62:=6 63:=7 64:=6 65:=7 66:=6 67:=7 68:=e 69:=f 6a:=e 6b:=f 6c:=e 6d:=f 6e:=e 6f:=f 70:=7 71:=7 72:=7 73:=7 74:=7 75:=7 76:=7 77:=7 78:=f 79:=f 7a:=f 7b:=f 7c:=f 7d:=f 7e:=f 7f:=f 80:=8 81:=9 82:=a 83:=b 84:=c 85:=d 86:=e 87:=f 88:=8 89:=9 8a:=a 8b:=b 8c:=c 8d:=d 8e:=e 8f:=f 90:=9 91:=9 92:=b 93:=b 94:=d 95:=d 96:=f 97:=f 98:=9 99:=9 9a:=b 9b:=b 9c:=d 9d:=d 9e:=f 9f:=f a0:=a a1:=b a2:=a a3:=b a4:=e a5:=f a6:=e a7:=f a8:=a a9:=b aa:=a ab:=b ac:=e ad:=f ae:=e af:=f b0:=b b1:=b b2:=b b3:=b b4:=f b5:=f b6:=f b7:=f b8:=b b9:=b ba:=b bb:=b bc:=f bd:=f be:=f bf:=f c0:=c c1:=d c2:=e c3:=f c4:=c c5:=d c6:=e c7:=f c8:=c c9:=d ca:=e cb:=f cc:=c cd:=d ce:=e cf:=f d0:=d d1:=d d2:=f d3:=f d4:=d d5:=d d6:=f d7:=f d8:=d d9:=d da:=f db:=f dc:=d dd:=d de:=f df:=f e0:=e e1:=f e2:=e e3:=f e4:=e e5:=f e6:=e e7:=f e8:=e e9:=f ea:=e eb:=f ec:=e ed:=f ee:=e ef:=f f0:=f f1:=f f2:=f f3:=f f4:=f f5:=f f6:=f f7:=f f8:=f f9:=f fa:=f fb:=f fc:=f fd:=f fe:=f ff:=f 
$(foreach i,$(_or-pairs),$(eval __bor$(i)))
_xor-pairs :=00:=0 01:=1 02:=2 03:=3 04:=4 05:=5 06:=6 07:=7 08:=8 09:=9 0a:=a 0b:=b 0c:=c 0d:=d 0e:=e 0f:=f 10:=1 11:=0 12:=3 13:=2 14:=5 15:=4 16:=7 17:=6 18:=9 19:=8 1a:=b 1b:=a 1c:=d 1d:=c 1e:=f 1f:=e 20:=2 21:=3 22:=0 23:=1 24:=6 25:=7 26:=4 27:=5 28:=a 29:=b 2a:=8 2b:=9 2c:=e 2d:=f 2e:=c 2f:=d 30:=3 31:=2 32:=1 33:=0 34:=7 35:=6 36:=5 37:=4 38:=b 39:=a 3a:=9 3b:=8 3c:=f 3d:=e 3e:=d 3f:=c 40:=4 41:=5 42:=6 43:=7 44:=0 45:=1 46:=2 47:=3 48:=c 49:=d 4a:=e 4b:=f 4c:=8 4d:=9 4e:=a 4f:=b 50:=5 51:=4 52:=7 53:=6 54:=1 55:=0 56:=3 57:=2 58:=d 59:=c 5a:=f 5b:=e 5c:=9 5d:=8 5e:=b 5f:=a 60:=6 61:=7 62:=4 63:=5 64:=2 65:=3 66:=0 67:=1 68:=e 69:=f 6a:=c 6b:=d 6c:=a 6d:=b 6e:=8 6f:=9 70:=7 71:=6 72:=5 73:=4 74:=3 75:=2 76:=1 77:=0 78:=f 79:=e 7a:=d 7b:=c 7c:=b 7d:=a 7e:=9 7f:=8 80:=8 81:=9 82:=a 83:=b 84:=c 85:=d 86:=e 87:=f 88:=0 89:=1 8a:=2 8b:=3 8c:=4 8d:=5 8e:=6 8f:=7 90:=9 91:=8 92:=b 93:=a 94:=d 95:=c 96:=f 97:=e 98:=1 99:=0 9a:=3 9b:=2 9c:=5 9d:=4 9e:=7 9f:=6 a0:=a a1:=b a2:=8 a3:=9 a4:=e a5:=f a6:=c a7:=d a8:=2 a9:=3 aa:=0 ab:=1 ac:=6 ad:=7 ae:=4 af:=5 b0:=b b1:=a b2:=9 b3:=8 b4:=f b5:=e b6:=d b7:=c b8:=3 b9:=2 ba:=1 bb:=0 bc:=7 bd:=6 be:=5 bf:=4 c0:=c c1:=d c2:=e c3:=f c4:=8 c5:=9 c6:=a c7:=b c8:=4 c9:=5 ca:=6 cb:=7 cc:=0 cd:=1 ce:=2 cf:=3 d0:=d d1:=c d2:=f d3:=e d4:=9 d5:=8 d6:=b d7:=a d8:=5 d9:=4 da:=7 db:=6 dc:=1 dd:=0 de:=3 df:=2 e0:=e e1:=f e2:=c e3:=d e4:=a e5:=b e6:=8 e7:=9 e8:=6 e9:=7 ea:=4 eb:=5 ec:=2 ed:=3 ee:=0 ef:=1 f0:=f f1:=e f2:=d f3:=c f4:=b f5:=a f6:=9 f7:=8 f8:=7 f9:=6 fa:=5 fb:=4 fc:=3 fd:=2 fe:=1 ff:=0 
$(foreach i,$(_xor-pairs),$(eval __bxor$(i)))




bit-and = $(__gmtt-dbg-args)$(call _bit-and,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1),$(findstring -,$2))
_bit-and = $(__gmtt-dbg-args)$(call __bit-and,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$(call _$(firstword $2)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
__bit-and = $(__gmtt-dbg-args)$(call _fmt$5,$(call _16to$5,$(foreach i,$(call _sgnxt-merge,$(if $3,$(call _twoscompl16,$1),$1),$(if $4,$(call _twoscompl16,$2),$2),$3,$4),$(__band$(i)))))

bit-or = $(__gmtt-dbg-args)$(call _bit-or,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1),$(findstring -,$2))
_bit-or = $(__gmtt-dbg-args)$(call __bit-or,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$(call _$(firstword $2)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
__bit-or = $(__gmtt-dbg-args)$(call _fmt$5,$(call _16to$5,$(foreach i,$(call _sgnxt-merge,$(if $3,$(call _twoscompl16,$1),$1),$(if $4,$(call _twoscompl16,$2),$2),$3,$4),$(__bor$(i)))))

bit-xor = $(__gmtt-dbg-args)$(call _bit-xor,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1),$(findstring -,$2))
_bit-xor = $(__gmtt-dbg-args)$(call __bit-xor,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$(call _$(firstword $2)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $2)))),$3,$4,$(firstword $1))
__bit-xor = $(__gmtt-dbg-args)$(call _fmt$5,$(call _16to$5,$(foreach i,$(call _sgnxt-merge,$(if $3,$(call _twoscompl16,$1),$1),$(if $4,$(call _twoscompl16,$2),$2),$3,$4),$(__bxor$(i)))))

bit-not = $(__gmtt-dbg-args)$(call _bit-not,$(call _analyze,$1),$(findstring -,$1))
_bit-not = $(__gmtt-dbg-args)$(call __bit-not,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$2,$(firstword $1))
__bit-not = $(__gmtt-dbg-args)$(call _fmt$3,$(call _16to$3,$(call _onescompl16,$(if $2,$(call _twoscompl16,$1),$1))))


add = $(__gmtt-dbg-args)$(call _add,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1)v$(findstring -,$2))
_add = $(__gmtt-dbg-args)$(call __add$3,$(call _xpld-num,$(call _nrmlz,$(lastword $1))),$(call _$(firstword $2)to$(firstword $1),$(call _xpld-num,$(call _nrmlz,$(lastword $2)))),$(firstword $1))
__addv = $(__gmtt-dbg-args)$(call _fmt$3,$(call _add$3,$(call _shift-merge,$1,$2)))
__add-v = $(__gmtt-dbg-args)$(if $(findstring >,$(_ucmp)),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$1,$2)),-),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$2,$1))))
__add-v- = $(__gmtt-dbg-args)$(call _fmt$3,$(call _add$3,$(call _shift-merge,$1,$2)),-)
__addv- = $(__gmtt-dbg-args)$(if $(findstring <,$(_ucmp)),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$2,$1)),-),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$1,$2))))

sub = $(__gmtt-dbg-args)$(call _sub,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1)v$(findstring -,$2))
_sub = $(__gmtt-dbg-args)$(call __sub$3,$(call _xpld-num,$(call _nrmlz,$(lastword $1))),$(call _$(firstword $2)to$(firstword $1),$(call _xpld-num,$(call _nrmlz,$(lastword $2)))),$(firstword $1))
__subv = $(__gmtt-dbg-args)$(if $(findstring <,$(_ucmp)),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$2,$1)),-),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$1,$2))))
__sub-v = $(__gmtt-dbg-args)$(call _fmt$3,$(call _add$3,$(call _shift-merge,$1,$2)),-)
__sub-v- = $(__gmtt-dbg-args)$(if $(findstring >,$(_ucmp)),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$1,$2)),-),$(call _fmt$3,$(call _sub$3,$(call _shift-merge,$2,$1))))
__subv- = $(__gmtt-dbg-args)$(call _fmt$3,$(call _add$3,$(call _shift-merge,$1,$2)))




# $1 = a
# $2 = b
# => a/b
div = $(__gmtt-dbg-args)$(call _div,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1)$(findstring -,$2))
_div = $(__gmtt-dbg-args)$(subst --,,$3)$(call _fmt$(firstword $1),$(call _10to$(firstword $1),$(call _div10,$(call _$(firstword $1)to10,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$(call _$(firstword $2)to10,$(call _xpld-num,$(call _nrmlz,$(lastword $2)))))))
_div10 = $(__gmtt-dbg-args)$(call _div10_1,$1,$2,$(wordlist $(words _ $2),$(words $1),$(_all-0)))


mod = $(__gmtt-dbg-args)$(call _mod,$(call _analyze,$1),$(call _analyze,$2),$(findstring -,$1)$(findstring -,$2))
_mod = $(__gmtt-dbg-args)$(subst --,,$3)$(call _fmt$(firstword $1),$(call _10to$(firstword $1),$(call _mod10,$(call _$(firstword $1)to10,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))),$(call _$(firstword $2)to10,$(call _xpld-num,$(call _nrmlz,$(lastword $2)))))))
_mod10 = $(__gmtt-dbg-args)$(call _sub10,$(call _shift-merge,$1,$(call _mul10,$(call _div10,$1,$2),$2)))

# Negate a number
neg = $(if $(findstring -,$1),$(subst -,,$1),-$1)

to-oct = $(call _to-oct,$(call _analyze,$1),$(findstring -,$1))
_to-oct = $2$(call _fmt8,$(call _$(firstword $1)to8,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))))
to-hex = $(call _to-hex,$(call _analyze,$1),$(findstring -,$1))
_to-hex = $2$(call _fmt16,$(call _$(firstword $1)to16,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))))
to-dec = $(call _to-dec,$(call _analyze,$1),$(findstring -,$1))
_to-dec = $2$(call _fmt10,$(call _$(firstword $1)to10,$(call _xpld-num,$(call _nrmlz,$(lastword $1)))))


# Return a number zero extended to the given number of places. The sign gets dropped.
zero-ext = $(__gmtt-dbg-args)$(call _zero-ext,$(call _nrmlz,$(lastword $(call _analyze,$1))),$(or $2,0))
_zero-ext = $(__gmtt-dbg-args)$(subst $(space),,$(wordlist $(words _ $(call _xpld-num,$1)),$2,$(_all-0)))$1



######################################################################
# Round a number down to the given modulus.
# $1 = number
# $2 = modulus
# Example: round(0x1234,0x100) --> 0x1200
round-dn = $(__gmtt-dbg-args)$(call bit-and,$1,$(call neg,$2))

######################################################################
# Round a number up to the next given modulus.
# $1 = number
# $2 = modulus
# Example: round_up(0x1234,0x100) --> 0x1300
round-up = $(__gmtt-dbg-args)$(call round-dn,$(call add,$1,$(call sub,$2,1)),$2)

######################################################################
# Round a number up to the next given modulus - 1
# $1 = number
# $2 = modulus
# Example: fill_up(0x1234,0x100) --> 0x12ff
fill-up = $(__gmtt-dbg-args)$(call bit-or,$1,$(call sub,$2,1))



######################################################################
# Reorder the records of a table according to the given index list.
# The reorder index is a list of starting indexes of the table records.
# The index list doesn't need to be complete, missing records are dropped.
# $1 = table as list
# $2 = table width
# $3 = reorder index
# Example:
# _reorder-tbl(1 2 3 4 5 6 7 8 9 10 11 12,4,10 1 7) -> 10 11 12 1 2 3 7 8 9
_reorder-tbl = $(__gmtt-dbg-args)$(call __reorder-tbl,$1,$(call _usub10,$2,1),$3)
__reorder-tbl = $(__gmtt-dbg-args)$(foreach i,$3,$(wordlist $(i),$(call _uadd10,$(i),$2),$1))

######################################################################
# Produce a list of zeros which is length floor(log2) of the given number.
# $1 - decimal literal
# Example: _log2-zeros(15)
_log2-zeros = $(__gmtt-dbg-args)$(wordlist 2,$(call log2,$1),$(_all-0))

######################################################################
# Generate a decimal index for the list. The index is a list of decimal
# literals starting from 0 counting upwards and has as many digits as
# necessary to cover the range of elements.
# $1 - list
# $2 - prefix string
# Example: index(a b c d e f..) -->  00 01 02 03 04 05 06 07 08 09 10 11 ..
_bld-ix = $(__gmtt-dbg-args)$(subst $(space), $2, $(wordlist 1,$(words $1),$(call __bld-ix,$(call _xpld-num,$(words $1)))))
__bld-ix = $(__gmtt-dbg-args)$(call ___bld-ix,$(wordlist 2,10,$1),0 $(wordlist 1,$(firstword $1),1 2 3 4 5 6 7 8 9))
___bld-ix = $(__gmtt-dbg-args)$(if $1,$(call ___bld-ix,$(wordlist 2,10,$1),$(foreach i,$2,$(i)0 $(i)1 $(i)2 $(i)3 $(i)4 $(i)5 $(i)6 $(i)7 $(i)8 $(i)9)),$2)


######################################################################
# Retrieve one table line as a comma-separated record
# $1 - table
# $2 - table width
# Example: chop-rec(1 2 3 4 5 6,3) -> 1,2,3  4,5,6
_chop-rec = $(__gmtt-dbg-args)$(call __chop-rec,$1,$2,$(call _uadd10,$2,1))
__chop-rec = $(if $1,$(call list2param,$(wordlist 1,$2,$1)) $(call __chop-rec,$(wordlist $3,$(tbl-limit),$1),$2,$3))

######################################################################
# Generate a list of sorted indexes from the given unsorted keys.
# This means that if the first key would be sorted to position 15,
# then in the returned list at position 15 there will be a 1.
# These numbers can be directly used as index in the $(wordlist) function.
# Character 164('¤') is used as separator and is forbidden in key columns.
# $1 - list of keys before sorting
# $2 - table width
_sort-ix = $(__gmtt-dbg-args)$(foreach i,$(filter-out %¤,$(subst ¤,¤ ,$(sort $(join $1,$(call _bld-ix,$1,¤))))),$(call _uadd10,1,$(call _umul10,$(i),$2)))

######################################################################
# Generate a list of reversely sorted indexes from the given unsorted keys.
# This means that if the Nth key would be sorted to position 15,
# then in the returned list at position 15 there will be a N.
# These numbers can be directly used as index in the $(wordlist) function.
# $1 - list of keys before sorting
# $2 - table width
_rsort-ix = $(__gmtt-dbg-args)$(call rev-list,$(call _sort-ix,$1,$2))

######################################################################
# Sort a table by lines.
# $1 - table
# $2 - keygenerator function (takes one table line as parameters $1,$2,$3,etc.
sort-tbl = $(__gmtt-dbg-args)$(call _sort-tbl,$(strip $1),$2)
_sort-tbl = $(__gmtt-dbg-args)$(call __sort-tbl,$(wordlist 2,$(tbl-limit),$1),$(firstword $1),$2)
__sort-tbl = $(__gmtt-dbg-args)$2 $(call _reorder-tbl,$1,$2,$(call _sort-ix,$(foreach params,$(call _chop-rec,$1,$2),$(call lambda,$3,$(params))),$2))

######################################################################
# Reverse sort a table by lines.
# $1 - table
# $2 - keygenerator function (takes one table line as parameters $1,$2,$3,etc.
rsort-tbl = $(__gmtt-dbg-args)$(call _rsort-tbl,$(strip $1),$2)
_rsort-tbl = $(__gmtt-dbg-args)$(call __rsort-tbl,$(wordlist 2,$(tbl-limit),$1),$(firstword $1),$2)
__rsort-tbl = $(__gmtt-dbg-args)$2 $(call _reorder-tbl,$1,$2,$(call _rsort-ix,$(foreach params,$(call _chop-rec,$1,$2),$(call lambda,$3,$(params))),$2))

######################################################################
# Print (or do sg. else) with a table
# $1 - table
# $2 - output function (takes one table line as parameters $1,$2,$3,etc.
print-tbl = $(__gmtt-dbg-args)$(call _print-tbl,$(strip $1),$2)
_print-tbl = $(__gmtt-dbg-args)$(call __print-tbl,$(wordlist 2,$(tbl-limit),$1),$(firstword $1),$2)
__print-tbl = $(__gmtt-dbg-args)$(call ___print-tbl,$(call _chop-rec,$1,$2),$3)
___print-tbl = $(if $1,$(call lambda,$2,$(firstword $1))$(call ___print-tbl,$(wordlist 2,$(tbl-limit),$1),$2))


######################################################################
# $1=list of numbers (tuple indices)
# $2=list of words from which to choose
# returns: the words from $2 which correspond to indexes (starting with 1) from $1
pick = $(foreach elem,$1,$(word $(elem),$2))

######################################################################
# $1 = table
# $2 = list of column numbers
# $3 = "where-clause", function of $2 parameters returning true (non-null) or false (empty string)
# returns: a list of columns (selection in $3) from the rows in table $1 where condition $4 is non-null
select = $(call _select,$(strip $1),$2,$3)
_select = $(call __select,$(wordlist 2,$(tbl-limit),$1),$(firstword $1),$(call add,$(firstword $1),1),$2,$3)
__select = $(if $1,$(if $(call lambda,$5,$(call list2param,$(wordlist 1,$2,$1))), $(call pick,$4,$1))$(call __select,$(wordlist $3,$(tbl-limit),$1),$2,$3,$4,$5))
#___select = $(if $1,$(call lambda,$2,$(firstword $1))$(call ___print-tbl,$(wordlist 2,$(tbl-limit),$1),$2))

######################################################################
# $1 = table
# $2 = list of column numbers
# $3 = "where-clause", function of $4 parameters returning true (non-null) or false (empty string)
# $4 = a function which processes a list containing the selection from $3 on each iteration of the select
# returns: a list of columns (selection in $2) from the rows in table $1 where condition $3 is non-null
map-select = $(__gmtt-dbg-args)$(call _map-select,$(strip $1),$2,$3,$4)
_map-select = $(call __map-select,$(wordlist 2,$(tbl-limit),$1),$(firstword $1),$(call add,$(firstword $1),1),$2,$3,$4)
__map-select = $(if $1,$(if $(call lambda,$5,$(call list2param,$(wordlist 1,$2,$1))), $(call lambda,$6,$(call list2param,$(call pick,$4,$1))))$(call __map-select,$(wordlist $3,$(tbl-limit),$1),$2,$3,$4,$5,$6))



_compare-result = $(call str-eq,$(strip $1),$(strip $2))
_gmtt-test = $(if $(call _compare-result,$(call lambda,$1),$2),$(info Ok: $1 = $2),$(info Test failed: $1 = $(call lambda,$1), should be: $2))

#$(call _gmtt-test,$$(call fill-up,0x123456,0),0xfffff)

# Performance measurement
# cnt := 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 
# __gmtt-dbg-args :=
# li := $(call listN,X,1000)
# #$(foreach i,$(cnt),$(foreach j,$(cnt),$(foreach k,$(cnt),$(eval foo := $(call pick,1 2 3 4,$(wordlist 2,10,$(li)))))))
# $(foreach i,$(cnt),$(foreach j,$(cnt),$(foreach k,$(cnt),$(eval foo := $(call pick,1 2 3 4,$(li))))))
# $(info $(foo))
# $(error end)
