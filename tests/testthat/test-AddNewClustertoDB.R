test_that("AddNewClustertoDB fails when it should", {
  expect_error(AddNewClustertoDB(), "cluster object must be supplied")
})

# test_that("AddNewClustertoDB works", {
#   cl <- parallel::makePSOCKcluster(names = 2)
#   doParallel::registerDoParallel(cl)
#   ConnectClusterDB(getDBtestingPath())
#   hash <- AddNewClustertoDB(cl, DBdir = getDBtestingPath())
#
#   archivist::areadLocal("18d1591879e8ecc69d42bc02bd6f9299",md5hash =  )
#
#
#   AddNewClustertoDB(), "cluster object must be supplied")
# })
