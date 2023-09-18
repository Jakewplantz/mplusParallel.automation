#' Remove Parallel Processing Folders from mplusParallel_automation
#'
#' This function is used to delete all parallel processing folders (with names
#' containing the word "session") that were created by the `mplusParallel_automation`
#' function.
#'
#' @param folder The root directory to search for parallel processing folders.
#'
#'
#' @return Invisible NULL. The function is called for its side effect of
#'   deleting folders.
#' @export
#' @examples
#' \dontrun{
#' # Assuming you have parallel processing folders in your current RStudio
#' # document's directory
#' removeParFolders()
#' }
#' @seealso
#' \code{\link{mplusParallel_automation}} for the function that creates
#' these folders.




removeParFolders <- function(folder =NULL){
  subdirs <- list.dirs(folder, full.names = TRUE, recursive = FALSE)
  for (subdir in subdirs) {
    if (grepl("session", basename(subdir))) {
      unlink(subdir, recursive = TRUE)
    }
  }}
