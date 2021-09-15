test_that("KillPID works", {

  cl <- parallel::makePSOCKcluster(names = 2)
  doParallel::registerDoParallel(cl)
  ClusterData <- GetStartedClusterPIDData(cl)
  KilledPIDs <-  KillPID(ClusterData[Role == "worker",]$PID)
  try(parallel::stopCluster(cl))


  expect_equal(lapply(KilledPIDs, class), list(PID = "integer", ProcesState = "character"))
  expect_equal(unique(KilledPIDs$ProcesState), "down")
  expect_equal(sort(ClusterData[Role == "worker",]$PID), sort(KilledPIDs$PID))
})
