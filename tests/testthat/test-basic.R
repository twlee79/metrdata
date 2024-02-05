test_that("constant tests", {
  # checks values of constants against expectations
  expect_equal(the$attr_name, "metrdata")
  expect_equal(the$filename_suffix, "_metadata.json")
  expect_equal(the$class_name, "metrdata")
})

test_that("helper tests", {
  # default: strict
  expect_error(stop_or_warn("problem"))
  expect_error(strip_metadata(list()))

  {
    # ensure we reset setting
    withr::defer(set_strict())

    # test disable strict
    expect_warning({
      set_strict(FALSE)
      stop_or_warn("problem")
      })

    # test persistence
    expect_warning(stop_or_warn("problem"))
    expect_warning(strip_metadata(list()))

    # test reenable strict
    expect_error({set_strict(TRUE); stop_or_warn("problem")})
    expect_error(stop_or_warn("problem"))
    expect_error(strip_metadata(list()))

    # test strict mode for other functions in their own tests

  }

  # append suffix
  expect_equal(append_metrdata_suffix("/this/is/a/test/path"),
    "/this/is/a/test/path_metadata.json")
  # current implementation does not require path to point to a filename
  expect_equal(append_metrdata_suffix("/this/is/a/test/path/"),
    "/this/is/a/test/path/_metadata.json")


})

test_that("metrdata object basics", {
  # construction of metrdata objs and has/is metadata

  # construction with either list or using ...
  expect_equal(
    metrdata(a=1,b=1,c=list(1,2)),
    metrdata(list(a=1,b=1,c=list(1,2)))
    )

  # naming errors during construction
  expect_error(metrdata(1)) # unnamed
  expect_error(metrdata(a=1,a=1)) # dup names
  expect_error(metrdata(a=1,1)) # mixed
  expect_error(metrdata(metrdata(a=1,b=2))) # already metrdata

  {
    # ensure we reset setting
    withr::defer(set_strict())

    set_strict(FALSE)
    expect_warning(expect_warning(metrdata(1)))
    expect_warning(metrdata(a=1,a=1))
    expect_warning(expect_warning(metrdata(a=1,1)))
    expect_warning(metrdata(metrdata(a=1,b=2)))
  }

  # is_metadata
  expect_true(is_metrdata(metrdata()))
  expect_false(is_metrdata(list()))
  expect_true(is_metrdata(metrdata(note = "Example metadata")))

  # metrdata is equivalent to a list with diff class
  expect_equal(
    list(note = "Example metadata"),
    metrdata(note = "Example metadata"), ignore_attr = TRUE)

  expect_false({
    a_data_frame <- data.frame(index = 1:10)
    has_metadata(a_data_frame)}
  )

  # advanced construction
  {
    form1<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"))
    form2<-metrdata(
      list(`Original data source` = list(Note="Fictional data", Created="2024-01")))
    form3<-metrdata(
      label = "Original data source", list(Note="Fictional data", Created="2024-01"))
    expect_equal(form1, form2)
    expect_equal(form1, form3)
  }

  # nesting
  {
    form1<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"))

    nested<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"),
      Toplevel = metrdata(Note="This is nested metadata")
    )
    expect_equal(
      nested,
      c(form1, list(Toplevel = list(Note="This is nested metadata"))),
      ignore_attr = TRUE
    )
  }

  # include
  {
    form1<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"))

    include1<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"),
      metrdata(Note="This is an included metadata"))

    include2<-metrdata(
      `Original data source` = list(Note="Fictional data", Created="2024-01"),
      include = metrdata(Note="This is an included metadata"))
    expect_equal(include1, include2)
    expect_equal(
      include1,
      c(form1, Note="This is an included metadata"),
      ignore_attr = TRUE
    )
  }




})

test_that("assigning and retrieving metadata basics", {
  # test assign metadata using <- + get_metadata()
  expect_equal({
    a_data_frame <- data.frame(index = 1:10)
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    get_metadata(a_data_frame)},
    list(note = "Indexes 1 to 10"), ignore_attr=TRUE
  )

  # test assign metadata using <- + metadata()
  expect_equal({
    a_data_frame <- data.frame(index = 1:10)
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    metadata(a_data_frame)},
    list(note = "Indexes 1 to 10"), ignore_attr=TRUE
  )

  # test reassign metadata
  expect_error({
    a_data_frame <- data.frame(index = 1:10)
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    }
  )

  # test set_metadata()
  expect_equal({
    a_data_frame2 <- data.frame(index = 1:20)
    a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")))
    get_metadata(a_data_frame2)},
    list(note = "Indexes 1 to 20"), ignore_attr=TRUE
  )

  # trying to replace
  expect_error({
    a_data_frame2 <- data.frame(index = 1:20)
    a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")))
    a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")))
  })

  # allowing replace
  expect_equal({
    a_data_frame2 <- data.frame(index = 1:20)
    a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "To be replaced")))
    a_data_frame2 <- set_metadata(a_data_frame2, metrdata(list(note = "Indexes 1 to 20")),
      TRUE)
    get_metadata(a_data_frame2)},
    list(note = "Indexes 1 to 20"), ignore_attr=TRUE
  )


  # test has_metadata()
  expect_true({
    a_data_frame <- data.frame(index = 1:10)
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    has_metadata(a_data_frame)}
  )

  # test strip_metadata()
  expect_false({
    a_data_frame <- data.frame(index = 1:10)
    metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
    stripped_data_frame <- strip_metadata(a_data_frame)
    has_metadata(stripped_data_frame)}
  )

  # has/is metrdata


  {
    obj_metrdata <- metrdata(list(note = "Indexes 1 to 10"))
    expect_true(has_is_metrdata(obj_metrdata))
    expect_false(is_metrdata(list()))
    a_data_frame <- data.frame(index = 1:10)

    expect_false(has_is_metrdata(a_data_frame))
    metadata(a_data_frame) <- obj_metrdata
    expect_true(has_is_metrdata(a_data_frame))

  }


})
