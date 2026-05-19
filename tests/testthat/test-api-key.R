expect_api_key_arg <- function(args, expected, position = NULL) {
  key <- args$key

  if (is.null(key) && !is.null(position)) {
    key <- args[[position]]
  }

  expect_equal(key, expected)
}

test_that("get_census_api_key requires an API key", {
  skip_on_cran()

  withr::local_envvar(CENSUS_API_KEY = NA)

  expect_error(
    tidycensus:::get_census_api_key(NULL),
    "A Census API key is required"
  )

  expect_equal(tidycensus:::get_census_api_key("direct-key"), "direct-key")

  withr::local_envvar(CENSUS_API_KEY = "env-key")
  expect_equal(tidycensus:::get_census_api_key(NULL), "env-key")
})

test_that("metadata requests include the Census API key", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    GET = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured GET", call. = FALSE)
    }
  )

  expect_error(load_variables(2022, "acs5", key = "metadata-key"), "captured GET")
  expect_equal(captured[[1]]$query$key, "metadata-key")
  expect_match(captured[[1]]$url, "/2022/acs/acs5/variables\\.json$")

  expect_error(get_pop_groups(2020, "ddhca", key = "metadata-key"), "captured GET")
  expect_equal(captured[[2]]$query$key, "metadata-key")
  expect_match(captured[[2]]$url, "/2020/dec/ddhca/variables\\.json$")
})

test_that("load_variables can use a valid cache without an API key", {
  skip_on_cran()

  cache_dir <- tempfile("tidycensus-cache-")
  dir.create(cache_dir)
  readr::write_rds(
    data.frame(
      name = c("B01001_001", "B01001_002"),
      label = c("Estimate!!Total", "Estimate!!Total!!Male"),
      concept = c("Sex by Age", "Sex by Age"),
      geography = c("block group", "block group")
    ),
    file.path(cache_dir, "acs5_2022.rds")
  )

  withr::local_envvar(CENSUS_API_KEY = NA)

  local_mocked_bindings(
    user_cache_dir = function(appname) cache_dir,
    GET = function(...) {
      stop("load_variables should not call the API when a valid cache exists", call. = FALSE)
    }
  )

  vars <- load_variables(2022, "acs5", cache = TRUE)

  expect_equal(vars$name, c("B01001_001", "B01001_002"))
})

test_that("table expansion helpers pass direct keys to load_variables", {
  skip_on_cran()

  seen <- list()

  local_mocked_bindings(
    load_variables = function(year, dataset, cache, key = NULL) {
      seen[[length(seen) + 1]] <<- list(
        year = year,
        dataset = dataset,
        cache = cache,
        key = key
      )

      data.frame(
        name = c("B01001_001", "B01001_002", "P001001", "P001002"),
        label = NA_character_,
        concept = NA_character_
      )
    }
  )

  expect_equal(
    tidycensus:::variables_from_table_acs(
      "B01001",
      2022,
      "acs5",
      cache_table = FALSE,
      key = "table-key"
    ),
    c("B01001_001", "B01001_002")
  )
  expect_equal(seen[[1]]$key, "table-key")

  expect_equal(
    tidycensus:::variables_from_table_decennial(
      "P001",
      2010,
      "sf1",
      cache_table = FALSE,
      key = "table-key"
    ),
    c("P001001", "P001002")
  )
  expect_equal(seen[[2]]$key, "table-key")
})

test_that("public table calls thread direct keys into table expansion", {
  skip_on_cran()

  local_mocked_bindings(
    variables_from_table_acs = function(table, year, survey, cache_table, key = NULL) {
      expect_equal(key, "public-key")
      "B01001_001"
    },
    load_data_acs = function(...) {
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        B01001_001E = 1,
        B01001_001M = 2
      )
    }
  )

  acs <- suppressMessages(
    get_acs(
      geography = "state",
      table = "B01001",
      year = 2022,
      state = "TX",
      key = "public-key",
      output = "wide"
    )
  )

  expect_equal(acs$GEOID, "48")

  local_mocked_bindings(
    variables_from_table_decennial = function(table, year, sumfile, cache_table, key = NULL) {
      expect_equal(key, "public-key")
      "P1_001N"
    },
    load_data_decennial = function(...) {
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        P1_001N = 1
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      table = "P1",
      year = 2020,
      sumfile = "pl",
      state = "TX",
      key = "public-key",
      output = "wide"
    )
  )

  expect_equal(decennial$GEOID, "48")
})

test_that("public variable calls pass direct keys to API data loaders", {
  skip_on_cran()

  local_mocked_bindings(
    load_data_acs = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key", position = 3)
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        B01001_001E = 1,
        B01001_001M = 2
      )
    }
  )

  acs <- suppressMessages(
    get_acs(
      geography = "state",
      variables = "B01001_001",
      year = 2022,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(acs$GEOID, "48")

  local_mocked_bindings(
    load_data_decennial = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key", position = 3)
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        P1_001N = 1
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      variables = "P1_001N",
      year = 2020,
      sumfile = "pl",
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(decennial$GEOID, "48")

  local_mocked_bindings(
    load_data_estimates = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        POP = 1,
        DENSITY = 2
      )
    }
  )

  estimates <- suppressMessages(
    get_estimates(
      geography = "state",
      product = "population",
      year = 2019,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(estimates$GEOID, "48")

  local_mocked_bindings(
    load_data_flows = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        GEOID1 = "48001",
        GEOID2 = "48003",
        FULL1_NAME = "Anderson County, Texas",
        FULL2_NAME = "Andrews County, Texas",
        MOVEDIN = 1,
        MOVEDIN_M = 2,
        MOVEDOUT = 3,
        MOVEDOUT_M = 4,
        MOVEDNET = -2,
        MOVEDNET_M = 5
      )
    }
  )

  flows <- suppressMessages(
    get_flows(
      geography = "county",
      year = 2018,
      state = "TX",
      key = "entry-key",
      output = "wide"
    )
  )
  expect_equal(flows$GEOID1, "48001")

  local_mocked_bindings(
    load_data_pums = function(...) {
      args <- list(...)
      expect_api_key_arg(args, "entry-key")
      data.frame(
        SERIALNO = "1",
        SPORDER = "1",
        WGTP = 1,
        PWGTP = 1,
        ST = "48",
        AGEP = "35"
      )
    }
  )

  pums <- suppressMessages(
    get_pums(
      variables = "AGEP",
      state = "TX",
      year = 2022,
      survey = "acs1",
      key = "entry-key"
    )
  )
  expect_equal(pums$SERIALNO, "1")
})

test_that("population group labels use the resolved Census API key", {
  skip_on_cran()

  local_mocked_bindings(
    load_data_decennial = function(...) {
      data.frame(
        GEOID = "48",
        NAME = "Texas",
        POPGROUP = "0001",
        T01001_001N = 1
      )
    },
    get_pop_groups = function(year, sumfile, key = NULL) {
      expect_equal(key, "label-key")
      data.frame(
        pop_group = "0001",
        pop_group_label = "Total population"
      )
    }
  )

  decennial <- suppressMessages(
    get_decennial(
      geography = "state",
      variables = "T01001_001N",
      year = 2020,
      sumfile = "ddhca",
      state = "TX",
      pop_group = "0001",
      pop_group_label = TRUE,
      key = "label-key",
      output = "wide"
    )
  )

  expect_equal(decennial$pop_group_label, "Total population")
})

test_that("low-level Census API data requests include the API key", {
  skip_on_cran()

  captured <- list()

  local_mocked_bindings(
    GET = function(url, query = list(), ...) {
      captured[[length(captured) + 1]] <<- list(url = url, query = query)
      stop("captured GET", call. = FALSE)
    }
  )

  expect_error(
    tidycensus:::load_data_acs(
      geography = "state",
      formatted_variables = "B01001_001E,B01001_001M",
      key = "data-key",
      year = 2022,
      survey = "acs5"
    ),
    "captured GET"
  )

  expect_error(
    tidycensus:::load_data_decennial(
      geography = "state",
      variables = "P1_001N",
      key = "data-key",
      year = 2020,
      sumfile = "pl",
      pop_group = NULL
    ),
    "captured GET"
  )

  expect_error(
    tidycensus:::load_data_estimates(
      geography = "state",
      product = "population",
      variables = NULL,
      key = "data-key",
      year = 2019,
      time_series = FALSE
    ),
    "captured GET"
  )

  expect_error(
    tidycensus:::load_data_pums(
      variables = "AGEP",
      state = "TX",
      puma = NULL,
      key = "data-key",
      year = 2022,
      survey = "acs1",
      variables_filter = NULL,
      recode = FALSE,
      show_call = FALSE
    ),
    "captured GET"
  )

  expect_error(
    tidycensus:::load_data_pums_vacant(
      variables = "HINCP",
      state = "TX",
      puma = NULL,
      key = "data-key",
      year = 2022,
      survey = "acs1",
      variables_filter = list(TYPEHUGQ = "3"),
      recode = FALSE,
      show_call = FALSE
    ),
    "captured GET"
  )

  expect_error(
    tidycensus:::load_data_flows(
      geography = "county",
      variables = c("GEOID1", "GEOID2", "MOVEDIN"),
      key = "data-key",
      year = 2018,
      state = "TX"
    ),
    "captured GET"
  )

  expect_true(all(vapply(captured, function(x) identical(x$query$key, "data-key"), logical(1))))
})
