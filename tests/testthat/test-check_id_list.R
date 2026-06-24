test_that("id list checker catches duplicate names", {
  dup_names_list <-
    list(
      intervention = "a",
      intervention = "b"
    )

  dup_names_list2 <-
    list(
      intervention = "a",
      control = "c",
      intervention = "b"
    )

  dup_names_list3 <-
    list(
      intervention = "a",
      control = "c",
      intervention = "b",
      control = "d",
      other = "e"
    )

  expect_snapshot(
    error = TRUE, {
      check_id_list(dup_names_list)
    })

  expect_snapshot(
    error = TRUE, {
      check_id_list(dup_names_list2)
    })

  expect_snapshot(
    error = TRUE, {
      check_id_list(dup_names_list3)
    })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_id_list(dup_names_list, arg = "x")
    })
  })
})

test_that("id list checker works for both character and numeric inputs", {
  good_list <-
    list(
      item1 = c("item1", "item2"),
      item2 = c("item3", "item4")
    )

  good_list_num <-
    list(
      item1 = c(165108, 6684523),
      item2 = c(8845, 6507)
    )

  dataframe_input <-
    data.frame(a = c(1, 2))

  wrong_input <-
    c("item1", "item2")


  wrong_list <-
    list(
      c("item1", "item2")
    )

  wrong_list2 <-
    list(
      item1 = list(itemsub1 = c("item1", "item2"),
                   itemsub2 = c("item3", "item4")),
      item2 = c("item3")
    )

  expect_invisible(check_id_list(good_list))

  expect_invisible(check_id_list(good_list_num))

  # data.frames are allowed
  expect_invisible(check_id_list(dataframe_input))

  expect_snapshot(
    error = TRUE, {
      check_id_list(wrong_input)
    })

  expect_snapshot(
    error = TRUE, {
    check_id_list(wrong_list)
    })

  expect_snapshot(
    error = TRUE, {
    check_id_list(wrong_list2)
      })

  expect_invisible(check_id_list_numeric(good_list_num))

  expect_snapshot(
    error = TRUE, {
    check_id_list_numeric(good_list)
    })

  expect_snapshot(
    error = TRUE, {
      check_id_list_numeric(wrong_list)
    })

  expect_snapshot(
    error = TRUE, {
      check_id_list_numeric(wrong_list2)
    })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_id_list(wrong_list, arg = "x")
    })
  })

  cli::test_that_cli("format is ok", {
    expect_snapshot(error = TRUE, {
      check_id_list_numeric(good_list, arg = "x")
    })
  })
})
