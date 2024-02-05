test_that("concatenation", {

  expect_equal(
    cat_metadata(
      metrdata(list(note1="Test1")),
      metrdata(list(note2="Test2"))
    ),
    metrdata(list(note1 = "Test1", note2= "Test2"))
  )

  expect_error(
    cat_metadata(
      metrdata(list(note1="Test1")),
      metrdata(list(note1="Test2"))
    )
  )

  {
    set_strict(FALSE)
    withr::defer(set_strict(TRUE))
    expect_warning(expect_warning(
      cat_metadata(
        metrdata(list(note1="Test1")),
        metrdata(list(note1="Test2"))
      )
    ))
  }
})

test_that("append", {
  {
    met1<-list(`Original data source` = list(Note="Fictional data", Created="2024-01"))
    met2<-list(Note="This is an included metadata")

    expect_equal(
      append_metadata(
        metrdata(met1),metrdata(met2)
      ),
      c(met1, met2),
      ignore_attr=TRUE
    )

    expect_equal(
      prepend_metadata(
        metrdata(met1),metrdata(met2)
      ),
      c(met2, met1),
      ignore_attr=TRUE
    )
    expect_equal(
      metadata(append_metadata(
        set_metadata(data.frame(x=1:10),metrdata(met1)),metrdata(met2)
      )),
      append_metadata(metrdata(met1),metrdata(met2))
    )
  }
})


test_that("edit_metadata", {
  {
    edit_test<-metrdata(
      lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
      lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
    )

    # arbitrary function
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2a"),
        stringr::str_replace_all,
        "item","new"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "new1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1b","lvl1b2a","lvl1b2a3a"),
        stringr::str_replace,
        "REPLACE",""
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "xyz"))
      )
    )

    # assign
    # lvl 1
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a"),"<-","item3"
      ),
      metrdata(
        lvl1a = "item3",
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # lvl 2
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a","lvl1a2c"),"<-","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = "item3"),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # lvl 3
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a","lvl1a2c",2),"=","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,"item3",3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # delete
    # lvl 1
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a"),"-"
      ),
      metrdata(
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a"),"-"
      ),
      edit_metadata(
        edit_test,
        list("lvl1a"),"<-",NULL
      )
    )

    # lvl 2
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1a","lvl1a2c"),"-"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2"),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # lvl 3
    expect_equal(
      edit_metadata(
        edit_test,
        list("lvl1b","lvl1b2a","lvl1b2a3a"),"-"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz")[-1])
      )
    )

    # edit_name not used
    expect_warning(
      edit_metadata(
        edit_test,
        list("lvl1a","lvl1a2c"),"-","item3"
      )
    )

    # cannot delete from vector
    expect_error(
      edit_metadata(
        edit_test,
        list("lvl1a","lvl1a2c",2),"-"
      )
    )

    # append
    # at lvl 1
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1z"),"+","item0"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz")),
        lvl1z = "item0"
      )
    )

    # at lvl 2

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2d"),"+","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3),
          lvl1a2d = "item3"),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # at lvl 3
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1b","lvl1b2a","lvl1b2a3b"),"+","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz", lvl1b2a3b = "item3"))
      )
    )

    # + to vector
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2c"),"+",4
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3,4)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2c"),"+","test3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3,"test3")),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # prepend

    # at lvl 1
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1z"),"+0","item0"
      ),
      metrdata(
        lvl1z = "item0",
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )
    # at lvl 2

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2d"),"+0","item3"
      ),
      metrdata(
        lvl1a = list(
          lvl1a2d = "item3",
          lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)
          ),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # at lvl 3
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1b","lvl1b2a","lvl1b2a3b"),"+0","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3b = "item3", lvl1b2a3a = "REPLACExyz"))
      )
    )

    # + to vector
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2c"),"+0",4
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(4,1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2c"),"+0","test3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c("test3",1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    # str append
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2a"),"str+","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1item3", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1b","lvl1b2a","lvl1b2a3a"),"str+","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyzitem3"))
      )
    )

    # str prepend
    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1a","lvl1a2a"),"str+0","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item3item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "REPLACExyz"))
      )
    )

    expect_equal(
      edit_metadata(
        edit_test,
        c("lvl1b","lvl1b2a","lvl1b2a3a"),"str+0","item3"
      ),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "item3REPLACExyz"))
      )
    )


    # edit metadata of an object
    my_data<-set_metadata(
      data.frame(index=1:10),
      metrdata(
        lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3)),
        lvl1b = list(lvl1b2a = list(lvl1b2a3a = "xyz"))
      )
    )

    my_data<-edit_metadata(
      my_data,
      c("lvl1a","lvl1a2d"),"+","item4"
    )

    expect_equal(
      my_data,
      set_metadata(
        data.frame(index=1:10),
        metrdata(
          lvl1a = list(lvl1a2a = "item1", lvl1a2b="item2", lvl1a2c = c(1,2,3),
            lvl1a2d = "item4"),
          lvl1b = list(lvl1b2a = list(lvl1b2a3a = "xyz"))
        )
      )

    )

  }



})
