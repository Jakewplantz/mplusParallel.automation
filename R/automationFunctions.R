#' Print base automation functions for single or multi conditions
#'
#' This function is used to print the mplus_automation code used in the
#' mplusParallel_automation function. This is done as convenience to ease custom
#' function generation.
#'
#'
#' @return Invisible NULL. The function is called for its side effect of
#'   deleting folders.
#' @export
#' @examples
#' \dontrun{
#' # Print the single condition function
#' Base_AutomationFunc_single()
#'
#' # Print the multi-condition function
#' Base_AutomationFunc_multi()
#' }
#' @seealso
#' \code{\link{mplusParallel_automation}} for the function that creates
#' these folders.



Base_AutomationFunc_single <- function(){
std_MFun<-
  "
      retries <- 0
max_retries <- max_retry
df_ready <- FALSE

while (!df_ready && retries < max_retries) {
    filepath <- file.path(session_folder, 'exdat.csv')
    write.table(data, filepath, col.names = FALSE, row.names = FALSE, sep = ',')

    if (run == TRUE) {
        runModels(session_folder, showOutput = F, recursive = rec)
    }

    test_res <- assign(paste0('sums_', pid), readModels(session_folder, what = 'summaries', recursive = rec))
    t <- test_res[[2]]
    chi_value <- t$ChiSqM_Value

    if (is.null(chi_value)) {
        set.seed(k*10000)
        retries <- retries + 1
        next
    }

    if (retries == max_retries) {
        stop('Max retries reached. Could not get a non-null chi value.')
    }

    df_ready <- TRUE
}

if (results == 'summaries') {
    models_result <- assign(paste0('sums_', pid), readModels(session_folder, what = results, recursive = rec))
    res <- models_result[[2]]
    res <- res %>%
        mutate(Rep = k) %>%
        select(Rep, everything())
    new_row <- res

for (con in con_index) {
    new_row[[con]] <- eval(parse(text = con))
}
        rows_list[[length(rows_list) + 1]] <- new_row

}

else if (results == 'parameters') {
    models_result <- assign(paste0('sums_', pid), readModels(session_folder, what = results, recursive = rec))
    res <- models_result
    new_data <- res[[2]][[params_ext]]
    new_data <- new_data %>%
        mutate(Rep = k, ParType = params_ext) %>%
        select(Rep, everything())
    new_row <- new_data

for (con in con_index) {
    new_row[[con]] <- eval(parse(text = con))
}
        rows_list[[length(rows_list) + 1]] <- new_row

}

else if (results == 'mod_indices') {
    models_result <- assign(paste0('sums_', pid), readModels(session_folder, recursive = rec))
    res <- models_result[['mod_indices']]
    new_data <- res
    new_data <- new_data %>%
        mutate(Rep = k) %>%
        select(Rep, everything())
    new_row <- new_data

for (con in con_index) {
    new_row[[con]] <- eval(parse(text = con))
}
        rows_list[[length(rows_list) + 1]] <- new_row

}

df <- do.call(rbind, rows_list)
rows_list <- list()
df_list[[length(df_list) + 1]] <- df
     "
return(cat(std_MFun))}




Base_AutomationFunc_multi <- function(){
  std_MFun<- "


  if (is.null(custom_auto)){
    retries <- 0  # A counter to avoid infinite loops, consider a limit like 10 retries
    max_retries <- max_retry
    df_ready <- FALSE  # A flag to indicate when the data frame is ready
    #-------------------------------------------------------#
    # START
    # The following function runs all models in the folders set above, then extracts the relevant stats
    #-------------------------------------------------------#
    while (!df_ready && retries < max_retries) {

      filepath <- file.path(session_folder, 'exdat.csv')

      write.table(data, filepath, col.names = FALSE, row.names = FALSE, sep = ',')

      if (run == T){
        runModels(session_folder, showOutput = F, recursive = rec)
      }
      test_res <- assign(paste0('sums_', pid),readModels(session_folder, what = 'summaries', recursive = rec))
      t <- test_res[[2]]


      # Extract the chi value (assuming it is in models_result, adapt as needed)
      chi_value <- t$ChiSqM_Value  # Change this line to accurately capture the chi value from your results

      if (is.null(chi_value)) {
        # Reset seed (adapt the way you want to reset it)
        set.seed(k*10000)

        retries <- retries + 1
        next  # Skip the rest of the loop and retry
      }

      if (retries == max_retries) {
        stop('Max retries reached. Could not get a non-null chi value.')
      }
      df_ready <- TRUE
    }



    if(results == 'summaries'){
      models_result <- assign(paste0('sums_', pid),readModels(session_folder, what = results, recursive = rec))
      res <- models_result[[2]]
      res <- res %>%
        mutate(Rep = k) %>%
        select(Rep, everything())
      new_row <- res
      rows_list[[length(rows_list) + 1]] <- new_row

    } else if (results == 'parameters'){
      models_result <- assign(paste0('sums_', pid),readModels(session_folder, what = results, recursive = rec))
      res <- models_result
      new_data <- res[[2]][[params_ext]]

      new_data <- new_data %>%
        mutate(Rep = k, ParType = params_ext) %>%
        select(Rep, everything())
      new_row <- new_data
      rows_list[[length(rows_list) + 1]] <- new_row

    } else if (results == 'mod_indices'){
      models_result <-  assign(paste0('sums_', pid),readModels(session_folder, recursive = rec))
      res <- models_result[['mod_indices']]
      new_data <- res
      new_data <- new_data %>%
        mutate(Rep = k) %>%
        select(Rep, everything())
      new_row <- new_data
      rows_list[[length(rows_list) + 1]] <- new_row

    } else {
      models_result <-  assign(paste0('sums_', pid),readModels(session_folder, recursive = rec))
      res <- models_result[[results]]
      new_data <- res
      new_data <- new_data %>%
        mutate(Rep = k) %>%
        select(Rep, everything())
      new_row <- new_data
      rows_list[[length(rows_list) + 1]] <- new_row
    }

    df <- do.call(rbind,rows_list)
    return(df)
  } else {
    eval(parse(text = custom_auto))
  }"
  return(cat(std_MFun))}
