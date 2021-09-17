


#' Function that Returns PID of current R session
#'
#' Currently only works on Windows
#'
#' @param PID process identification number, defaults to Sys.getpid()
#'
#' @return data.table with current PID, name and startup time
#' @export
#'
#' @examples GetProcessData()
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


#' Obtain ALL PID and startup information for a cluster
#'
#' @param cluster a cluster made by e.g. parallell:makecluster
#'
#' @return data.table with information about role and status of PID in a parallel cluster
#' @export
#'
#' @examples
#' \dontrun{
#' cl <- parallel::makePSOCKcluster(names = 2)
#' doParallel::registerDoParallel(cl)
#' GetStartedClusterPIDData(cl)
#' }
GetStartedClusterPIDData <- function(cluster){
  Host <- GetProcessData(PID = Sys.getpid())
  Host$Role <- "Host"
  Nodes <- parallel::clusterCall(cl = cluster, fun = GetProcessData) %>%
           data.table::rbindlist()

  Nodes$Role <- "worker"
  RETURNtable <- data.table::rbindlist(list(Host,Nodes))
  return(RETURNtable)
}


#' Kill a list of PIDs in Windows
#'
#' @param PIDlist array of PIDs that needs to be killed
#'
#' @return data.table with killed PID and there status
#' @export
#'
#' @examples
#' \dontrun{
#' cl <- parallel::makePSOCKcluster(names = 2)
#' doParallel::registerDoParallel(cl)
#' ClusterData <- GetStartedClusterPIDData(cl)
#'
#'
#' KillPID(ClusterData[Role == "worker",]$PID)
#' try(parallel::stopCluster(cl))}
KillPID <- function(PIDlist){

  ExecPS <- function(x){system2("powershell", args = x , stdout = T)}

  if(!identical(Sys.info()['sysname'][[1]], "Windows")){
    stop("only implemented for Windows")
  }

  # try to force close
  lapply(PIDlist, function(x){ExecPS(paste0("taskkill /pid ", x," /F"))})

  # then check if PID is really removed
  TerminatedPIDS <- data.table::rbindlist(lapply(PIDlist, GetProcessData),fill = TRUE)

  SystemBootTime <- NULL
  TerminatedPIDS <- TerminatedPIDS[, SystemBootTime := NULL]
  if(!all(TerminatedPIDS$ProcesState == "down")){
    warning("Not all requested PIDs where terminated")
  }

  return(TerminatedPIDS)
}

