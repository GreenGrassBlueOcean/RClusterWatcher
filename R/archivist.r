
#' Create an Archivist DataBase to
#'
#' @param ResetDB boolean to reset DataBase, Use True to reset DB, defaults to False
#' @param DBdir defaults to "C://Database//dir"
#'
#' @return invisible
#' @export
#'
#' @examples
#' ConnectClusterDB()
ConnectClusterDB <- function(DBdir = "C://Database//dir", ResetDB = F){

  # 1. check input ----
  if (is.na(DBdir)){
    try(DBdir <- getOption("RClusterWatcher.DBdir") )
    if(is.null(DBdir)){stop("Please supply DBdir")}
  }

  try(suppressMessages(archivist::createLocalRepo(repoDir =  DBdir, force = ResetDB)), silent = T)
  options(RClusterWatcher.DBdir = DBdir)
  message(paste0("Connecting to ClusterDB on ", DBdir))

  invisible(DBdir)
}










#' StoreProcessData
#'
#' @param DBdir database location defaults to .Options$RClusterWatcher.DBdir
#' @param cl parallel cluster object from parallel::makePSOCKcluster(names = 2)
#' @param clusterName character: optional name for cluster
#' @param verbose boolean, defaults to FALSE
#'
#' @return md5 heash location in database
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' cl <- parallel::makePSOCKcluster(names = 2)
#' doParallel::registerDoParallel(cl)
#' ConnectClusterDB()
#' AddNewClustertoDB(cl)
#'
#' #necessary to satisfy cran check
#' parallel::stopCluster(cl)
AddNewClustertoDB <- function( cl = NULL
                             , clusterName = NA
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
  hash <- archivist::saveToLocalRepo(repoDir = DBdir
                             , artifact = ClusterData
                             , userTags = c(clusterName, max(ClusterData$ProcessStartTime)))

  message(paste0(capture.output(ClusterData), collapse = "\n"))

  return(hash)
}


#' Check which parallel clusters are registerd in the database
#'
#' @param DBdir database location defaults to .Options$RClusterWatcher.DBdir
#'
#' @importFrom data.table :=
#'
#' @return data.table containing the registered clusters
#' @export
#'
#' @examples
#' ConnectClusterDB()
#' CheckCluster()
CheckCluster <- function( DBdir =  .Options$RClusterWatcher.DBdir){

  # Read In database with exisiting Clusters ---
  RegisteredClusters <- archivist::showLocalRepo(repo = DBdir) %>%
                        data.table::as.data.table()
  name <- NULL
  RegisteredClusters <- RegisteredClusters[ name == "ClusterData",]

  if(identical(nrow(RegisteredClusters),0L)){
    message("No Clusters registered with RClusterWatcher")
    return(list())
  }


  CheckOutcome <- lapply( X = RegisteredClusters$md5hash
                        , FUN = function(hash){  hash %>%
                                                 VerifyCluster() %>%
                                                 AssesCluster() %>%
                                                 TakeActionOnCluster()})

}

#' Verify if a registered Cluster is also an active Cluster
#'
#' @param DBdir database location defaults to .Options$RClusterWatcher.DBdir
#' @param ClusterHash md5 has of Database record
#'
#' @return verfiedClusterObject: list Containing, RegisterdCluster, ActiveCluster and md5 hash db location
#' @keywords Internal
#'
#' @examples
#' \dontrun{
#' ConnectClusterDB(ResetDB = T)
#' cl <- parallel::makePSOCKcluster(names = 2)
#' doParallel::registerDoParallel(cl)
#' hash <- AddNewClustertoDB(cl)
#' test <- VerifyCluster(ClusterHash = hash)
#' }
VerifyCluster <- function(ClusterHash = NULL, DBdir = .Options$RClusterWatcher.DBdir){

  if(is.null(ClusterHash)){
    stop("ClusterHash should be provided and is currently NULL")
  }

  RegisteredCluster <- archivist::areadLocal(repo = DBdir, md5hash = ClusterHash)
  ActiveCluster <- NULL
  if(identical(class(RegisteredCluster), c("data.table", "data.frame"))){
    ActiveCluster <- data.table::rbindlist(lapply(RegisteredCluster$PID, FUN = GetProcessData), fill = T)
   }

  ReturnList <- list(ClusterHash = ClusterHash, RegisteredCluster = RegisteredCluster, ActiveCluster = ActiveCluster)
  class(ReturnList) <- "verfiedClusterObject"
  return(ReturnList)
}




#' Once verified,  Assess the health of cluster and return status and PIDs to be terminated
#'
#' @param RetrievedClusterList Output from VerifyCluster
#' @param verbose print output, defaults to FALSE
#'
#' @return a AssessedClusterObject with additional Assesment and PIDStoBeTerminated field
#' @keywords Internal
#' @seealso VerifyCluster
#'
#' @examples
#' \dontrun{
#' ConnectClusterDB(ResetDB = T)
#' cl <- parallel::makePSOCKcluster(names = 2)
#' doParallel::registerDoParallel(cl)
#' hash <- AddNewClustertoDB(cl)
#' ClusterData <- VerifyCluster(ClusterHash = hash)
#' AssesCluster(ClusterData)
#' }
AssesCluster <- function(RetrievedClusterList, verbose = F){
  if(!identical(class(RetrievedClusterList),'verfiedClusterObject')){
    stop("RetrievedClusterList must be result of VerifyCluster but is not")
  }

  `Image Name` <- PID <- ProcesState <- SystemBootTime <- Role <- ProcessStartTime <- NULL
  MergedCluster <- data.table::merge.data.table( x = RetrievedClusterList$ActiveCluster[, list(`Image Name`, PID, ProcesState, ProcessStartTime, SystemBootTime )]
                                                , y = RetrievedClusterList$RegisteredCluster[, list(`Image Name`, PID, Role, ProcessStartTime)]
                                                , by = c("PID"), suffixes = c(".Active", ".Retrieved")
                                                , all = T)
  Selector <- ifelse(test = ("ProcessStartTime.Retrieved" %in% names(MergedCluster)), yes = "ProcessStartTime.Retrieved", no = "ProcessStartTime")

  #1. Check if cluster is running normally
  if(  identical(unique(MergedCluster$ProcesState), "Running") && #1. Check if all nodes are running
       identical(nrow(MergedCluster), nrow(RetrievedClusterList$ActiveCluster)) &&  #2 check if no new nodes appeared
       ( ("Host" %in%   MergedCluster$Role) & !duplicated(MergedCluster[Role == "Host"]))){
         RetrievedClusterList$Assesment <- "Running"
         RetrievedClusterList$PIDStoBeTerminated <- NA
         if(verbose){message("Cluster is running normally")}

       }

  #3. Check if PC has been reboot



  else if(identical(nrow(MergedCluster[eval(get(Selector)) > SystemBootTime,]),0L )){
    RetrievedClusterList$Assesment <- "System has had reboot"
    RetrievedClusterList$PIDStoBeTerminated <- NA
    if(verbose){message("Cluster has been removed due to reboot")}
  }

  #2. Check if cluster is fully down
  else if(  identical(unique(MergedCluster[Role == "worker",]$ProcesState), "down")){
       RetrievedClusterList$Assesment <- "down"
       RetrievedClusterList$PIDStoBeTerminated <- NA
       if(verbose){message("Cluster has been fully shutdown")}
  }

  #4. Check if cluster is a zombie cluster
  else if(identical(MergedCluster[Role == "Host",]$ProcesState, "down") &&
          identical(MergedCluster[Role == "worker",]$`Image Name.Active`, MergedCluster[Role == "worker",]$`Image Name.Retrieved`) &&
          identical(MergedCluster[Role == "worker",]$`ProcessStartTime.Active`, MergedCluster[Role == "worker",]$`ProcessStartTime.Retrieved`)
          ){
    RetrievedClusterList$Assesment <- "Zombie Cluster"
    RetrievedClusterList$PIDStoBeTerminated <- MergedCluster[Role == "worker",]$PID
    if(verbose){message(paste0("Zombie Cluster found!, PIDs ",RetrievedClusterList$PIDStoBeTerminated, " will be terminated") )}
  } else {
    log <- tempfile("log", fileext = ".rds")
    saveRDS(object = list(RetrievedClusterList = RetrievedClusterList), file = log )
    stop(paste("Undefined Condition in function AssesCluster cluster data.table Can be found here", log))
  }

class(RetrievedClusterList) <- "AssessedClusterObject"
return(RetrievedClusterList)
}




#' Once Verified and Assessed take Action on Cluster
#'
#' @param AssessedClusterObject output from AssesCluster function
#' @param DBdir database location defaults to .Options$RClusterWatcher.DBdir
#'
#' @return a list containing the KilledPids and the RemovedHash.
#' @keywords Internal
#' @seealso VerifyCluster
#' @seealso AssesCluster
#'
#' @examples
#' \dontrun{"internal function, dont use directly"}
#'
TakeActionOnCluster <- function(AssessedClusterObject, DBdir = .Options$RClusterWatcher.DBdir ){

  if(!identical(class(AssessedClusterObject), "AssessedClusterObject")){
      stop("TakeActionOnCluster can only take AssessedClusterObject")
  }

  KilledPIDS <- NULL
  #1. eliminate PIDs that should be elimaited
  if(!is.na(AssessedClusterObject$PIDStoBeTerminated)){
    KilledPIDS <- KillPID(AssessedClusterObject$PIDStoBeTerminated)
  }

  RemovedHash <- NULL
  if(AssessedClusterObject$Assesment %in% c("System has had reboot", "Zombie Cluster", "down")){
    archivist::rmFromLocalRepo(md5hash = AssessedClusterObject$ClusterHash,  repoDir = DBdir )
    RemovedHash <- AssessedClusterObject$ClusterHash
  }
  return(list(KilledPIDS = KilledPIDS, RemovedHash = RemovedHash))
}



#
