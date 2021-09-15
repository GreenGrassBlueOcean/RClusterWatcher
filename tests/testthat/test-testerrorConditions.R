test_that("ConnectClusterDB give errors when no DBdir is present", {

  options(RClusterWatcher.DBdir = NULL)
  expect_error(ConnectClusterDB(DBdir = NA, ResetDB = F), "Please supply DBdir")

})


test_that("VerifyCluster give errors when no cluster hash present", {

  expect_error(VerifyCluster(), "ClusterHash should be provided and is currently NULL")

})


test_that("AssesCluster give errors when no cluster hash present", {

  expect_error(AssesCluster(),"argument \"RetrievedClusterList\" is missing, with no default")
  expect_error(AssesCluster(RetrievedClusterList = list()), "RetrievedClusterList must be result of VerifyCluster but is not")

})


test_that("TakeActionOnCluster give errors when no cluster hash present", {

  expect_error(TakeActionOnCluster(),"argument \"AssessedClusterObject\" is missing, with no default")
  expect_error(TakeActionOnCluster(AssessedClusterObject = list()), "TakeActionOnCluster can only take AssessedClusterObject")

})

