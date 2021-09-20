test_that("Check for Slave Clusters works when slave clusters are present", {
library(RClusterWatcher)

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

    SlaveClusterHashes <- foreach::foreach(cl = cl, .errorhandling = 'pass', .packages = c("data.table", "archivist", "RClusterWatcher"),.export = c("AddNewClustertoDB", "GetStartedClusterPIDData", "GetProcessData")) %dopar% {

      #0. load functions that can not be available in github workflows
      AddNewClustertoDB <- function( cl = NULL
                                     , clusterName = "super cluster"
                                     , MasterCluster = NA
                                     , DBdir = .Options$RClusterWatcher.DBdir
                                     , verbose = FALSE
      ){
        if(is.null(cl)){
          stop("cluster object must be supplied")
        }

        ClusterData <- GetStartedClusterPIDData(cl)
        if (verbose){
          print(ClusterData)
        }

        ClusterData$ProcesState <- NULL
        attr(ClusterData, "tags" ) = c(paste0("clusterName:",clusterName), paste0("MasterCluster:",MasterCluster))
        hash <- archivist::saveToLocalRepo(repoDir = DBdir
                                           , artifact = ClusterData
                                           # , userTags =  #MasterCluster
                                           , use_flocks = T
        )

        message(paste0(capture.output(ClusterData), collapse = "\n"))

        return(hash)
      }

      GetStartedClusterPIDData <- function(cluster){
        Host <- GetProcessData(PID = Sys.getpid())
        Host$Role <- "Host"
        Nodes <- parallel::clusterCall(cl = cluster, fun = GetProcessData) %>%
          data.table::rbindlist()

        Nodes$Role <- "worker"
        RETURNtable <- data.table::rbindlist(list(Host,Nodes))
        return(RETURNtable)
      }

      GetProcessData <- function(PID = Sys.getpid()){

        if(!identical(Sys.info()['sysname'][[1]], "Windows")){
          stop("only implemented for Windows")
        }

        ExecPS <- function(x){system2("powershell", args = x , stdout = T)}

        ConvertPSTimeDate <- function(PSCommand){
          as.POSIXct(PSCommand,format="%m/%d/%Y %H:%M:%S %p",tz=Sys.timezone())
        }


        GetProcessDataCommand <- paste0("tasklist /FI 'pid eq ",PID,"' /FO CSV")
        GetProcessStartTime <- paste0("Get-Process -ID ",PID," | select starttime")
        GetLastBootTimeCommand <- "Get-CimInstance -ClassName win32_operatingsystem | select lastbootuptime"


        PSProcessOutput <- ExecPS(GetProcessDataCommand)
        LastBootTime <- ConvertPSTimeDate(ExecPS(GetLastBootTimeCommand)[4])

        if(identical(PSProcessOutput, "INFO: No tasks are running which match the specified criteria.")){
          return(data.table::data.table( PID = PID
                                         , ProcesState = "down"
                                         , SystemBootTime = LastBootTime))
        } else {
          ProcessData <- data.table::fread(text = PSProcessOutput)
          ProcessData$ProcessStartTime <- ConvertPSTimeDate(ExecPS(GetProcessStartTime)[4])
          ProcessData$SystemBootTime = LastBootTime
          ProcessData$ProcesState = "Running"
          ProcessData
        }
      }

      ## End of loading functions

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





