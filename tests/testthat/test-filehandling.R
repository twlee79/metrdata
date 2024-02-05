test_that("simple reading and writing", {
  {
    if (FALSE) {
      # conde for generating examples
      # not run
      a_data_frame <- data.frame(index = 1:10)
      metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
      write_metadata(a_data_frame, path = test_path("examples","indexes.tsv"))

    }
  }

  # Basic reading
  expect_equal(
    read_metadata(test_path("examples","indexes.tsv")),
    metrdata(list(note = "Indexes 1 to 10"))
  )

  # reading with manual suffix
  expect_equal(
    read_metadata(test_path("examples","indexes.tsv")),
    read_metadata(test_path("examples","indexes.tsv_metadata.json"), add_suffix = FALSE)
  )

  # reading to snapshot
  expect_snapshot(
    read_metadata(test_path("examples","indexes.tsv"))
  )

})


test_that("data+metadata read/write functions", {
  tempdir <- withr::local_tempdir()

  a_data_frame <- data.frame(index = 1:10)
  metadata(a_data_frame) <- metrdata(list(note = "Indexes 1 to 10"))
  tmpfilename<-paste0(tempdir,"delim.txt")

  write_with_metadata(a_data_frame, tmpfilename, write_func = write.csv, row.names = FALSE)
  a_data_frame2<-read_with_metadata(tmpfilename, read_func = read.csv)
  testthat::expect_equal(a_data_frame, a_data_frame2)

  write_tsv_metadata(a_data_frame, tmpfilename)
  a_data_frame3<-read_tsv_metadata(tmpfilename) # reread as tibble, so ignore class
  testthat::expect_equal(a_data_frame, a_data_frame3, ignore_attr = "class")

  write_csv_metadata(a_data_frame, tmpfilename)
  a_data_frame4<-read_csv_metadata(tmpfilename)
  testthat::expect_equal(a_data_frame3, a_data_frame4)

  write_csv2_metadata(a_data_frame, tmpfilename)
  a_data_frame5<-read_csv2_metadata(tmpfilename)
  testthat::expect_equal(a_data_frame3, a_data_frame5)

  write_delim_metadata(a_data_frame, tmpfilename, delim=",")
  a_data_frame6<-read_delim_metadata(tmpfilename, delim=",")
  testthat::expect_equal(a_data_frame3, a_data_frame6)

})
