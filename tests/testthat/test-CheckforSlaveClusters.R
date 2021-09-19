test_that("Check for Slave Clusters works when slave clusters are present", {

  #' Build a nested cluster
  #'
  #' @return list containing
  #' @importFrom foreach %dopar%
  #' @examples
  #' BuildNestedCluster()
  BuildNestedCluster <- function(){

    `%dopar%` <- foreach::`%dopar%`

    dbir <- dirname(tempfile())
    ConnectClusterDB(DBdir = dbir, ResetDB = TRUE)

    # `%dopar%` <- foreach::`%dopar%`()
    cl <- parallel::makePSOCKcluster(names = 2)
    doParallel::registerDoParallel(cl)
    MainCluster <-  GetStartedClusterPIDData(cl)

    MasterClusterHash <- AddNewClustertoDB(cl = cl, DBdir = dirname(tempfile()))
    print(MasterClusterHash)

    SlaveClusterHashes <- foreach::foreach(cl = cl, .errorhandling = 'pass', .export = c("AddNewClustertoDB", "GetStartedClusterPIDData", "GetProcessData")) %dopar% {
      rl <- parallel::makePSOCKcluster(names = 2)
      doParallel::registerDoParallel(rl)
      # MainCluster <-  GetStartedClusterPIDData(rl)
      clusterhash <- AddNewClustertoDB(cl = rl, DBdir = dbir, MasterCluster = MasterClusterHash )
      # parallel::clusterCall(cl = rl, fun = Sys.sleep, time = 10L )
      parallel::stopCluster(rl)
      clusterhash
    }
    parallel::stopCluster(cl)

    # archivist::showLocalRepo()
    # archivist::loadFromLocalRepo("2384764f5ba7166206dddf7a3d7c3793")
    return(list("master" =  MasterClusterHash, "slave" = SlaveClusterHashes, Dbdir = dbir))
  }

  hashes <- BuildNestedCluster()
  CheckedSlaveCluster <- CheckforSlaveClusters(MasterclusterHash = hashes$master, DBdir = hashes$Dbdir)
  print(CheckedSlaveCluster)
  print(hashes$slave)
  expect_equal(sort(as.vector(CheckedSlaveCluster)), expected =  sort(unlist(hashes$slave)))
  NoSlaveCluster <- CheckforSlaveClusters(MasterclusterHash = unlist(hashes$slave[1]), DBdir = hashes$Dbdir)
  expect_equal(NoSlaveCluster,NA)


  StopSlaveClusters(MasterclusterHash = CheckedSlaveCluster, DBdir = hashes$Dbdir)


})





