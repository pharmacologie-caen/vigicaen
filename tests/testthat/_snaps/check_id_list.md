# id list checker works for both character and numeric inputs

    Code
      check_id_list(wrong_input)
    Condition
      Error:
      ! `wrong_input` is not a (named) list.
      i `data.frame`, `data.table`, `Array`, and `Table` are not allowed.
      > Supply a named list to `wrong_input`. See ?ex_.

---

    Code
      check_id_list(wrong_list)
    Condition
      Error:
      ! `wrong_list` is not a named list.
      > Supply a named list to `wrong_list`. See ?ex_.

---

    Code
      check_id_list(wrong_list2)
    Condition
      Error:
      ! `wrong_list2` is not a list of character or numeric vectors.
      i Nested `lists` are not allowed.
      > Supply a named list of character or numeric vectors to `wrong_list2`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list)
    Condition
      Error:
      ! Type of `good_list` is not numeric or integer
      i Did you provide a list of drug or adr names, instead of ids?
      > Use `get_*` functions to collect ids.

---

    Code
      check_id_list_numeric(wrong_list)
    Condition
      Error:
      ! `wrong_list` is not a named list.
      > Supply a named list to `wrong_list`. See ?ex_.

---

    Code
      check_id_list_numeric(wrong_list2)
    Condition
      Error:
      ! `wrong_list2` is not a list of character or numeric vectors.
      i Nested `lists` are not allowed.
      > Supply a named list of character or numeric vectors to `wrong_list2`. See ?ex_.

# format is ok [plain]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      Error:
      ! `x` is not a named list.
      > Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      Error:
      ! Type of `x` is not numeric or integer
      i Did you provide a list of drug or adr names, instead of ids?
      > Use `get_*` functions to collect ids.

# format is ok [ansi]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a [1mnamed[22m list.
      > Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m Type of `x` is not numeric or integer
      [36mi[39m Did you provide a list of drug or adr [1mnames[22m, instead of [1mids[22m?
      > Use `get_*` functions to collect [1mids[22m.

# format is ok [unicode]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      Error:
      ! `x` is not a named list.
      → Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      Error:
      ! Type of `x` is not numeric or integer
      ℹ Did you provide a list of drug or adr names, instead of ids?
      → Use `get_*` functions to collect ids.

# format is ok [fancy]

    Code
      check_id_list(wrong_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m `x` is not a [1mnamed[22m list.
      → Supply a named list to `x`. See ?ex_.

---

    Code
      check_id_list_numeric(good_list, arg = "x")
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m Type of `x` is not numeric or integer
      [36mℹ[39m Did you provide a list of drug or adr [1mnames[22m, instead of [1mids[22m?
      → Use `get_*` functions to collect [1mids[22m.

