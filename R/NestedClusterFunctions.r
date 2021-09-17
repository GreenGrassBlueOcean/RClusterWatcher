# parallell cluster of clusters

#' Function to check if a Cluster also has attached slave clusters
#'
#' @param MasterclusterHash character clusterhash of a db
#' @param DBdir defaults to .Options$RClusterWatcher.DBdir
#'
#' @return the md5hash codes of these clusters in the database
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  # internal function
#'  }
CheckforSlaveClusters <- function(MasterclusterHash, DBdir = .Options$RClusterWatcher.DBdir){
  SlaveHashes <- archivist::searchInLocalRepo(fixed = T, repoDir = DBdir
                              , pattern = paste0("MasterCluster:", MasterclusterHash))
  if(identical(character(0), SlaveHashes)){
    return(NA)
  } else {
    return(SlaveHashes)
  }
}


#' Stop slave clusters of internal Clusters.
#'
#' @param MasterclusterHash ms5h hash of master cluster
#' @param DBdir defaults to .Options$RClusterWatcher.DBdir
#' @param verbose boolean defaults to FALSE
#'
#' @return nothing
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  #internal function
#' }
StopSlaveClusters <- function(MasterclusterHash, DBdir = .Options$RClusterWatcher.DBdir, verbose = F){

  VerifiedSlaveClusters <- lapply(X = MasterclusterHash, FUN = VerifyCluster) %>%
                           lapply(FUN = AssesSlaveCluster)  %>%
                           lapply(FUN = TakeActionOnCluster)

}



#' Asses state of a slave cluster
#'
#' @param RetrievedClusterList output of Verify Cluster function @seealso StopSlaveClusters
#' @param verbose boolean be verbose or not
#'
#' @return an AssessedCluster Object
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' #internal function @seealso StopSlaveClusters
#' }
AssesSlaveCluster <- function(RetrievedClusterList, verbose = FALSE){
if(!identical(class(RetrievedClusterList),'verfiedClusterObject')){
  stop("RetrievedClusterList must be result of VerifyCluster but is not")
}

`Image Name` <- PID <- ProcesState <- SystemBootTime <- Role <- ProcessStartTime <- NULL
MergedCluster <- data.table::merge.data.table( x = RetrievedClusterList$ActiveCluster[, list(`Image Name`, PID, ProcesState, ProcessStartTime, SystemBootTime )]
                                               , y = RetrievedClusterList$RegisteredCluster[, list(`Image Name`, PID, Role, ProcessStartTime)]
                                               , by = c("PID"), suffixes = c(".Active", ".Retrieved")
                                               , all = T)


PIDStoBeTerminated <- MergedCluster[ProcesState == "Running" & `Image Name` == "Rscript.exe" & Role == "worker",]$PID
if(nrow(PIDStoBeTerminated) == 0L || identical(PIDStoBeTerminated, integer(0))){
    PIDStoBeTerminated <- NA
    Assesment <- NA
  } else {
    Assesment <- "ZombieCluster Slave"
    if(verbose){message("shutting down Zombie Slave Cluster")}
  }

RetrievedClusterList$PIDStoBeTerminated <- PIDStoBeTerminated
RetrievedClusterList$Assesment <- Assesment
class(RetrievedClusterList) <- "AssessedClusterObject"

return(RetrievedClusterList)


}

#
# Nodes <- parallel::clusterCall(cl = cl, fun = test)
#
# Nodes1 <- parallel::clusterCall(cl = Nodes[[1]], fun = GetStartedClusterPIDData, cluster = Nodes[[1]]) %>%
#     data.table::rbindlist()
#
# Nodes2 <- parallel::clusterCall(cl = Nodes[[2]], fun = GetStartedClusterPIDData, cluster = Nodes[[1]]) %>%
#   data.table::rbindlist()
