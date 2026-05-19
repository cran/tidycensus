test_that("variables_from_table_decennial matches underscore tables for 2020 special files", {
  local_mocked_bindings(
    load_variables = function(year, dataset, cache, key = NULL) {
      data.frame(
        name = c("T03001_001N", "T03001_002N", "T03001001"),
        label = NA_character_,
        concept = NA_character_
      )
    }
  )

  expect_equal(
    variables_from_table_decennial("T03001", 2020, "ddhcb", TRUE),
    c("T03001_001N", "T03001_002N")
  )

  expect_equal(
    variables_from_table_decennial("T03001", 2020, "sdhc", TRUE),
    c("T03001_001N", "T03001_002N")
  )
})

test_that("get_decennial normalizes state-prefixed ZCTA GEOIDs", {
  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(
        GEOID = c("5682001", "5682007"),
        NAME = c("ZCTA5 82001, Wyoming", "ZCTA5 82007, Wyoming"),
        P010001 = c(35855, 16460)
      )
    }
  )

  out <- suppressMessages(
    get_decennial(
      geography = "zcta",
      variables = "P010001",
      year = 2000,
      state = "WY",
      key = "test-key",
      output = "wide"
    )
  )

  expect_equal(out$GEOID, c("82001", "82007"))
})
