test_that("CheckCluster works with an empty database", {
  ConnectClusterDB(ResetDB = T, DBdir = getDBtestingPath())
  expect_error(CheckCluster(DBdir = getDBtestingPath()), NA)
})

test_that("CheckCluster works with a running cluster", {

  ConnectClusterDB(ResetDB = T, DBdir = getDBtestingPath())
  cl <- parallel::makePSOCKcluster(names = 2)
  doParallel::registerDoParallel(cl)

  AddNewClustertoDB(cl, DBdir = getDBtestingPath())

  result <- CheckCluster()
  parallel::stopCluster(cl)

  expect_equal(result[[1]]$KilledPIDS, NULL)
  expect_equal(result[[1]]$RemovedHash, NULL)

})



test_that("CheckCluster works with a stopped cluster", {

  ConnectClusterDB(ResetDB = T, DBdir = getDBtestingPath())
  cl <- parallel::makePSOCKcluster(names = 2)
  doParallel::registerDoParallel(cl)

  AddNewClustertoDB(cl, DBdir = getDBtestingPath())
  parallel::stopCluster(cl)
  result <- CheckCluster()


  expect_equal(result[[1]]$KilledPIDS, NULL)
  expect_true(is.character(result[[1]]$RemovedHash))
  expect_equal(length(result[[1]]$RemovedHash),1L)

})
