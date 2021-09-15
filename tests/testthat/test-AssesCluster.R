test_that("AssesCluster works when the cluster is normally running", {

 #normal running Verfied cluster cluster object
 NormalRunningCluster <- structure(
   list(ClusterHash = "112c0130c131fcef9bc860b04ed1bab3",
        RegisteredCluster = structure(list( `Image Name` = c("rsession.exe","Rscript.exe", "Rscript.exe")
                                          , PID = c(18212L, 10264L, 6780L)
                                          , `Session Name` = c("Console", "Console", "Console")
                                          , `Session#` = c(1L, 1L, 1L)
                                          , `Mem Usage` = c("293.588 K", "63.304 K", "63.304 K")
                                          , ProcessStartTime = structure(c(1631521819, 1631501240,1631501240)
                                                                         , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                          , SystemBootTime = structure(c(1631353170, 1631353170, 1631353170)
                                                                       , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                          , Role = c("Host", "worker", "worker")), row.names = c(NA,-3L)
                                          , class = c("data.table", "data.frame")),
        ActiveCluster = structure(list(`Image Name` = c("rsession.exe", "Rscript.exe", "Rscript.exe")
                                       , PID = c(18212L, 10264L, 6780L)
                                       , `Session Name` = c("Console", "Console", "Console")
                                       , `Session#` = c(1L,1L, 1L)
                                       , `Mem Usage` = c("330.360 K", "63.616 K", "63.616 K")
                                       , ProcessStartTime = structure(c(1631521819, 1631501240,1631501240)
                                                                      , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                       , SystemBootTime = structure(c(1631353170, 1631353170,
                                                                    1631353170), class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                       , ProcesState = c("Running", "Running", "Running"))
                                  , row.names = c(NA,-3L), class = c("data.table", "data.frame")
                                  )), class = "verfiedClusterObject")

  AssementOutcome <- AssesCluster(NormalRunningCluster, verbose = T)
  expect_equal(names(AssementOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster", "Assesment", "PIDStoBeTerminated"))
  expect_equal(AssementOutcome$Assesment, "Running")
  expect_equal(AssementOutcome$PIDStoBeTerminated, NA)

})

test_that("AssesCluster works when the cluster is Completely down", {

  #normal running Verfied cluster cluster object
  NormalShutDownCluster <- structure(list(ClusterHash = "034e6ca9f62894fa41f97541edadfc88",
                                          RegisteredCluster = structure(list(`Image Name` = c("rsession.exe","Rscript.exe", "Rscript.exe")
                                                                             , PID = c(18212L, 14032L, 16700L)
                                                                             , `Session Name` = c("Console", "Console", "Console")
                                                                             , `Session#` = c(1L, 1L, 1L)
                                                                             , `Mem Usage` = c("448.104 K", "63.324 K", "63.340 K")
                                                                             , ProcessStartTime = structure(c(1631521819, 1631504814, 1631504814)
                                                                                                            , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                             , SystemBootTime = structure(c(1631353170, 1631353170, 1631353170)
                                                                                                          , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")

                                                                             , Role = c("Host", "worker", "worker")), row.names = c(NA,-3L)
                                                                        , class = c("data.table", "data.frame")),
                                          ActiveCluster = structure(list(`Image Name` = c("rsession.exe", NA, NA)
                                                                         , PID = c(18212L, 14032L, 16700L)
                                                                         , `Session Name` = c("Console", NA, NA)
                                                                         , `Session#` = c(1L, NA, NA)
                                                                         , `Mem Usage` = c("448.432 K", NA, NA)
                                                                         , ProcessStartTime = structure(c(1631521819, NA, NA)
                                                                                                        , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                         , SystemBootTime = structure(c(1631353170, 1631353170, 1631353170)
                                                                                                      , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")

                                                                         , ProcesState = c("Running", "down", "down")), row.names = c(NA, -3L)
                                                                    , class = c("data.table", "data.frame")
                                                                    )), class = "verfiedClusterObject")
  AssementOutcome <- AssesCluster(NormalShutDownCluster, verbose = T)
  expect_equal(names(AssementOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster", "Assesment", "PIDStoBeTerminated"))
  expect_equal(AssementOutcome$Assesment, "down")
  expect_equal(AssementOutcome$PIDStoBeTerminated, NA)

})


test_that("AssesCluster works when the system has been reboot", {

  #normal running Verfied cluster cluster object
  RebootCluster <- structure(list(ClusterHash = "034e6ca9f62894fa41f97541edadfc88",
                                  RegisteredCluster = structure(list(`Image Name` = c("rsession.exe","Rscript.exe", "Rscript.exe")
                                                                     , PID = c(18212L, 14032L, 16700L)
                                                                     , `Session Name` = c("Console", "Console", "Console")
                                                                     , `Session#` = c(1L, 1L, 1L)
                                                                     , `Mem Usage` = c("448.104 K", "63.324 K", "63.340 K")
                                                                     , ProcessStartTime = structure(c(1631521819, 1631504814, 1631504814)
                                                                                                    , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                     , SystemBootTime = structure(c(1631353170, 1631353170, 1631353170)
                                                                                                  , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")

                                                                     , Role = c("Host", "worker", "worker")), row.names = c(NA,-3L)
                                                                , class = c("data.table", "data.frame")),
                                  ActiveCluster = structure(list(`Image Name` = c(NA, NA, NA)
                                                                 , PID = c(18212L, 14032L, 16700L)
                                                                 , `Session Name` = c(NA, NA, NA)
                                                                 , `Session#` = c(NA, NA, NA)
                                                                 , `Mem Usage` = c(NA, NA, NA)
                                                                 , ProcessStartTime = structure(c(NA, NA, NA)
                                                                                                , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                 , SystemBootTime = structure(c(1631522819, 1631522819, 1631522819)
                                                                                              , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")

                                                                 , ProcesState = c("down", "down", "down")), row.names = c(NA, -3L)
                                                            , class = c("data.table", "data.frame")
                                  )), class = "verfiedClusterObject")
  AssementOutcome <- AssesCluster(RebootCluster, verbose = T)
  expect_equal(names(AssementOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster", "Assesment", "PIDStoBeTerminated"))
  expect_equal(AssementOutcome$Assesment, "System has had reboot")
  expect_equal(AssementOutcome$PIDStoBeTerminated, NA)

})


test_that("AssesCluster works when the is in Zombie mode", {

  #normal running Verfied cluster cluster object
  ZombieCluster <- structure(
    list(ClusterHash = "112c0130c131fcef9bc860b04ed1bab3",
         RegisteredCluster = structure(list( `Image Name` = c("rsession.exe","Rscript.exe", "Rscript.exe")
                                             , PID = c(18212L, 10264L, 6780L)
                                             , `Session Name` = c("Console", "Console", "Console")
                                             , `Session#` = c(1L, 1L, 1L)
                                             , `Mem Usage` = c("293.588 K", "63.304 K", "63.304 K")
                                             , ProcessStartTime = structure(c(1631521819, 1631501240,1631501240)
                                                                            , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                             , SystemBootTime = structure(c(1631353170, 1631353170, 1631353170)
                                                                          , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                             , Role = c("Host", "worker", "worker")), row.names = c(NA,-3L)
                                       , class = c("data.table", "data.frame")),
         ActiveCluster = structure(list(`Image Name` = c(NA, "Rscript.exe", "Rscript.exe")
                                        , PID = c(18212L, 10264L, 6780L)
                                        , `Session Name` = c(NA, "Console", "Console")
                                        , `Session#` = c(NA,1L, 1L)
                                        , `Mem Usage` = c(NA, "63.616 K", "63.616 K")
                                        , ProcessStartTime = structure(c(NA, 1631501240,1631501240)
                                                                       , class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                        , SystemBootTime = structure(c(1631353170, 1631353170,
                                                                       1631353170), class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                        , ProcesState = c("down", "Running", "Running"))
                                   , row.names = c(NA,-3L), class = c("data.table", "data.frame")
         )), class = "verfiedClusterObject")

  AssementOutcome <- AssesCluster(ZombieCluster, verbose = T)
  expect_equal(names(AssementOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster", "Assesment", "PIDStoBeTerminated"))
  expect_equal(AssementOutcome$Assesment, "Zombie Cluster")
  expect_equal(sort(AssementOutcome$PIDStoBeTerminated), sort(c(6780L, 10264L)))
})



