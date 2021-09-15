test_that("VerifyCluster works if cluster still active", {


  ConnectClusterDB(ResetDB = T, DBdir = getDBtestingPath())
  cl <- parallel::makePSOCKcluster(names = 2)
  doParallel::registerDoParallel(cl)
  hash <- suppressMessages(AddNewClustertoDB(cl, DBdir = getDBtestingPath()))

  TestOutcome <- VerifyCluster(ClusterHash = hash, DBdir = getDBtestingPath())
  expect_equal(class(TestOutcome), "verfiedClusterObject")
  expect_equal(names(TestOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster"))
  expect_equal(summary(TestOutcome$ClusterHash), structure(c(Length = "1", Class = "character", Mode = "character"
  ), class = c("summaryDefault", "table")))
  expect_equal(lapply(TestOutcome$RegisteredCluster,class),
               list(`Image Name` = "character", PID = "integer"
                   , `Session Name` = "character",`Session#` = "integer"
                   , `Mem Usage` = "character"
                   , ProcessStartTime = c("POSIXct","POSIXt"), SystemBootTime = c("POSIXct","POSIXt")
                   , Role = "character")
               )

  expect_equal(lapply(TestOutcome$ActiveCluster,class),
               list(`Image Name` = "character", PID = "integer", `Session Name` = "character",
                    `Session#` = "integer", `Mem Usage` = "character"
                    , ProcessStartTime = c("POSIXct","POSIXt"), SystemBootTime = c("POSIXct","POSIXt")
                    , ProcesState = "character"))


  expect_equal(sort(TestOutcome$ActiveCluster$PID), sort(TestOutcome$RegisteredCluster$PID))
  expect_equal(unique(TestOutcome$ActiveCluster$ProcesState), "Running")

  parallel::stopCluster(cl)
  })

test_that("VerifyCluster works if cluster is not active anymore", {


  ConnectClusterDB(ResetDB = T, DBdir = getDBtestingPath())
  cl <- parallel::makePSOCKcluster(names = 2)
  doParallel::registerDoParallel(cl)
  hash <- suppressMessages(AddNewClustertoDB(cl, DBdir = getDBtestingPath()))
  parallel::stopCluster(cl)

  TestOutcome <- VerifyCluster(ClusterHash = hash, DBdir = getDBtestingPath())

  expect_equal(names(TestOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster"))
  expect_equal(summary(TestOutcome$ClusterHash), structure(c(Length = "1", Class = "character", Mode = "character"
  ), class = c("summaryDefault", "table")))
  expect_equal(lapply(TestOutcome$RegisteredCluster,class),
               list(`Image Name` = "character", PID = "integer"
                    , `Session Name` = "character",`Session#` = "integer"
                    , `Mem Usage` = "character"
                    , ProcessStartTime = c("POSIXct","POSIXt"), SystemBootTime = c("POSIXct","POSIXt")
                    , Role = "character")
  )

  expect_equal(lapply(TestOutcome$ActiveCluster,class),
               list(`Image Name` = "character", PID = "integer", `Session Name` = "character",
                    `Session#` = "integer", `Mem Usage` = "character"
                    , ProcessStartTime = c("POSIXct","POSIXt"), SystemBootTime = c("POSIXct","POSIXt")
                    , ProcesState = "character"))


  expect_equal(sort(TestOutcome$ActiveCluster$PID), sort(TestOutcome$RegisteredCluster$PID))
  expect_equal(c("Running", "down", "down"), sort(TestOutcome$ActiveCluster$ProcesState))


})

