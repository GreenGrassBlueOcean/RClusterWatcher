#'
#' #' Title
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' MakeRegisterCluster <- function(){
#'   cl <- parallel::makePSOCKcluster(names = 2)
#'   doParallel::registerDoParallel(cl)
#'   print(cl)
#'   hash <- RClusterWatcher::AddNewClustertoDB(cl, verbose = T)
#'   parallel::stopCluster(cl)
#'   hash
#' }
#'
#' #
#' # callr::r(MakeRegisterCluster, package = T)
