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


test_that("AssesCluster works when some pids already have been reassigned", {

  PartiallyreassignedCluster <- structure(list(ClusterHash = "661a375e4830bab4d4928e39541cfdfb",
                                     RegisteredCluster = structure(list(`Image Name` = c("Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe", "Rscript.exe", "Rscript.exe",
                                                                                          "Rscript.exe", "Rscript.exe")
                                                                        , PID = c( 22272L, 24020L, 26296L,11044L, 28824L, 27508L, 30568L, 27284L, 11872L, 17976L, 27796L
                                                                                 , 29436L, 25260L, 27968L, 21996L, 23444L, 30288L, 27872L, 9600L
                                                                                 , 24696L, 9480L, 28880L, 5560L, 30688L, 27036L, 13840L, 12084L
                                                                                 , 28172L, 8068L, 29084L, 27652L)
                                                                        , `Session Name` = c("RDP-Tcp#2","RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2"
                                                                                            ,"RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2"
                                                                                            , "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2","RDP-Tcp#2", "RDP-Tcp#2"
                                                                                            , "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2"
                                                                                            , "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2", "RDP-Tcp#2"
                                                                                            , "RDP-Tcp#2")
                                                                        , `Session#` = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
                                                                                         , 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,2L, 2L, 2L, 2L, 2L, 2L)
                                                                        , `Mem Usage` = c("1.765.224 K", "63.476 K","63.460 K", "63.476 K", "63.496 K", "63.464 K", "63.468 K"
                                                                                         ,"63.468 K", "63.480 K", "63.452 K", "63.464 K", "63.468 K"
                                                                                         ,"63.452 K", "63.488 K", "63.460 K", "63.488 K", "63.476 K"
                                                                                         ,"63.460 K", "63.468 K", "63.472 K", "63.456 K", "63.476 K"
                                                                                         ,"63.476 K", "63.472 K", "63.464 K", "63.452 K", "63.480 K"
                                                                                         ,"63.492 K", "63.488 K", "63.460 K", "63.488 K")
                                                                        , ProcessStartTime = structure(c( 1631776294,1631780168, 1631780168, 1631780168, 1631780168, 1631780168
                                                                                                        , 1631780168, 1631780168, 1631780169, 1631780169, 1631780169, 1631780169
                                                                                                        , 1631780169, 1631780169, 1631780170, 1631780170, 1631780170, 1631780170
                                                                                                        , 1631780170, 1631780170, 1631780171, 1631780171, 1631780171, 1631780171
                                                                                                        , 1631780171, 1631780171, 1631780172, 1631780172, 1631780172, 1631780172
                                                                                                        , 1631780172), class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                        , SystemBootTime = structure(c(1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056, 1631686056,
                                                                                                           1631686056, 1631686056, 1631686056, 1631686056)
                                                                                                     , class = c("POSIXct","POSIXt"), tzone = "Europe/Berlin")
                                                                        , Role = c("Host","worker", "worker", "worker", "worker", "worker", "worker","worker"
                                                                                   , "worker", "worker", "worker", "worker", "worker","worker", "worker", "worker", "worker", "worker"
                                                                                   , "worker", "worker", "worker", "worker", "worker", "worker", "worker", "worker", "worker", "worker"
                                                                                   , "worker", "worker", "worker")), row.names = c(NA, -31L), class = c("data.table","data.frame"))
                                     , ActiveCluster = structure(list(PID = c( 22272L, 24020L, 26296L, 11044L, 28824L, 27508L,30568L, 27284L, 11872L, 17976L, 27796L, 29436L, 25260L
                                                                             , 27968L, 21996L, 23444L, 30288L, 27872L, 9600L, 24696L, 9480L, 28880L, 5560L, 30688L, 27036L, 13840L, 12084L
                                                                             , 28172L, 8068L, 29084L, 27652L)
                                                                      , ProcesState = c("down", "down", "Running", "down", "down", "Running", "down", "down", "down", "down"
                                                                                       , "down", "down", "down", "down", "Running", "down", "down", "down", "down", "down", "down"
                                                                                       , "down", "down", "down", "down", "down", "down", "down", "down", "down", "down")
                                                                      , SystemBootTime = structure(c(1631686056,1631686056, 1631686056, 1631686056, 1631686056, 1631686056
                                                                                                    , 1631686056, 1631686056, 1631686056, 1631686056, 1631686056
                                                                                                    , 1631686056, 1631686056, 1631686056, 1631686056, 1631686056
                                                                                                    , 1631686056, 1631686056, 1631686056, 1631686056, 1631686056
                                                                                                    , 1631686056, 1631686056, 1631686056, 1631686056, 1631686056
                                                                                                    , 1631686056, 1631686056, 1631686056, 1631686056, 1631686056), class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin")
                                                                      , `Image Name` = c(NA, NA, "epmd", NA, NA, "epmd", NA, NA, NA, NA, NA, NA, NA, NA
                                                                                         , "epmd", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                                                      , `Session Name` = c( NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
                                                                                          , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                                                      , `Session#` = c(NA, NA, 2L, NA, NA, 2L, NA, NA, NA, NA, NA, NA, NA, NA, 2L, NA, NA, NA, NA, NA, NA, NA
                                                                                      , NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                                                      , `Mem Usage` = c( NA, NA, "392 K", NA, NA, "392 K", NA, NA, NA, NA, NA, NA, NA, NA, "392 K", NA, NA, NA, NA
                                                                                       , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
                                                                      , ProcessStartTime = structure(c(NA, NA, 1631785005, NA, NA, 1631787725, NA, NA, NA, NA, NA, NA, NA, NA, 1631785489
                                                                                                      , NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), class = c("POSIXct", "POSIXt"), tzone = "Europe/Berlin"))
                                                                 , row.names = c(NA, -31L), class = c("data.table", "data.frame"))), class = "verfiedClusterObject")
  AssementOutcome <- AssesCluster(  PartiallyreassignedCluster, verbose = T)
  expect_equal(names(AssementOutcome), c("ClusterHash", "RegisteredCluster", "ActiveCluster", "Assesment", "PIDStoBeTerminated"))
  expect_equal(AssementOutcome$Assesment, "down")
  expect_equal(AssementOutcome$PIDStoBeTerminated, NA)


  })
