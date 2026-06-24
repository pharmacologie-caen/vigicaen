# id list checker catches duplicate names

    Code
      check_id_list(dup_names_list)
    Condition
      Error:
      ! `dup_names_list` must have unique names.
      x Redundant name found in `dup_names_list`:
      i "intervention" at position 1 and 2

---

    Code
      check_id_list(dup_names_list2)
    Condition
      Error:
      ! `dup_names_list2` must have unique names.
      x Redundant name found in `dup_names_list2`:
      i "intervention" at position 1 and 3

---

    Code
      check_id_list(dup_names_list3)
    Condition
      Error:
      ! `dup_names_list3` must have unique names.
      x Redundant name found in `dup_names_list3`:
      i "intervention" at position 1 and 3
      i "control" at position 2 and 4

# id list checker catches duplicate names / format is ok [plain]

    Code
      check_id_list(dup_names_list, arg = "x")
    Condition
      Error:
      ! `x` must have unique names.
      x Redundant name found in `x`:
      i "intervention" at position 1 and 2

# id list checker catches duplicate names / format is ok [ansi]

    Code
      check_id_list(dup_names_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must have [1munique[22m names.
      [31mx[39m Redundant name found in `x`:
      [36mi[39m [34m"intervention"[39m at position 1 and 2

# id list checker catches duplicate names / format is ok [unicode]

    Code
      check_id_list(dup_names_list, arg = "x")
    Condition
      Error:
      ! `x` must have unique names.
      ✖ Redundant name found in `x`:
      ℹ "intervention" at position 1 and 2

# id list checker catches duplicate names / format is ok [fancy]

    Code
      check_id_list(dup_names_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must have [1munique[22m names.
      [31m✖[39m Redundant name found in `x`:
      [36mℹ[39m [34m"intervention"[39m at position 1 and 2

# id list checker works for both character and numeric inputs

    Code
      check_id_list(wrong_input)
    Condition
      Error:
      ! `wrong_input` must be a named list.
      x `wrong_input` is of class character.
      > Supply a named list to `wrong_input`. See ?ex_.

---

    Code
      check_id_list(wrong_list)
    Condition
      Error:
      ! `wrong_list` must be a named list.
      x `wrong_list` items have no name.
      > Supply a named list to `wrong_list`. See ?ex_.

---

    Code
      check_id_list(wrong_list2)
    Condition
      Error:
      ! `wrong_list2` items must all be of type character or numeric.
      i Nested `lists` are not allowed.
      > Supply a named list of character or numeric vectors to `wrong_list2`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list)
    Condition
      Error:
      ! Type of `good_list` must be numeric or integer.
      x Non-numeric/integer elements detected in "item1" and "item2".
      i Did you provide a list of drug or adr names, instead of ids?
      > Use `get_*` functions to collect numeric ids.

---

    Code
      check_id_list_numeric(wrong_list)
    Condition
      Error:
      ! `wrong_list` must be a named list.
      x `wrong_list` items have no name.
      > Supply a named list to `wrong_list`. See ?ex_.

---

    Code
      check_id_list_numeric(wrong_list2)
    Condition
      Error:
      ! `wrong_list2` items must all be of type character or numeric.
      i Nested `lists` are not allowed.
      > Supply a named list of character or numeric vectors to `wrong_list2`. See ?ex_.

# id list checker works for both character and numeric inputs / format is ok [plain]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      Error:
      ! `x` must be a named list.
      x `x` items have no name.
      > Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      Error:
      ! Type of `x` must be numeric or integer.
      x Non-numeric/integer elements detected in "item1" and "item2".
      i Did you provide a list of drug or adr names, instead of ids?
      > Use `get_*` functions to collect numeric ids.

# id list checker works for both character and numeric inputs / format is ok [ansi]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a [1mnamed[22m list.
      [31mx[39m `x` items have no name.
      > Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m Type of `x` must be numeric or integer.
      [31mx[39m Non-numeric/integer elements detected in [34m"item1"[39m and [34m"item2"[39m.
      [36mi[39m Did you provide a list of drug or adr [1mnames[22m, instead of [1mids[22m?
      > Use `get_*` functions to collect numeric [1mids[22m.

# id list checker works for both character and numeric inputs / format is ok [unicode]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      Error:
      ! `x` must be a named list.
      ✖ `x` items have no name.
      → Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      Error:
      ! Type of `x` must be numeric or integer.
      ✖ Non-numeric/integer elements detected in "item1" and "item2".
      ℹ Did you provide a list of drug or adr names, instead of ids?
      → Use `get_*` functions to collect numeric ids.

# id list checker works for both character and numeric inputs / format is ok [fancy]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` must be a [1mnamed[22m list.
      [31m✖[39m `x` items have no name.
      → Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m Type of `x` must be numeric or integer.
      [31m✖[39m Non-numeric/integer elements detected in [34m"item1"[39m and [34m"item2"[39m.
      [36mℹ[39m Did you provide a list of drug or adr [1mnames[22m, instead of [1mids[22m?
      → Use `get_*` functions to collect numeric [1mids[22m.

