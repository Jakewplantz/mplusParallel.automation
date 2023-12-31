% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeParFolders.R
\name{removeParFolders}
\alias{removeParFolders}
\title{Remove Parallel Processing Folders from mplusParallel_automation}
\usage{
removeParFolders(folder = NULL)
}
\arguments{
\item{folder}{The root directory to search for parallel processing folders.}
}
\value{
Invisible NULL. The function is called for its side effect of
deleting folders.
}
\description{
This function is used to delete all parallel processing folders (with names
containing the word "session") that were created by the \code{mplusParallel_automation}
function.
}
\examples{
\dontrun{
# Assuming you have parallel processing folders in your current RStudio
# document's directory
removeParFolders()
}
}
\seealso{
\code{\link{mplusParallel_automation}} for the function that creates
these folders.
}
