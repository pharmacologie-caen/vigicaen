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

