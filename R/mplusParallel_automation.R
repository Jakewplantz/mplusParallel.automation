#' Parallel automation of running Mplus models using R.
#'
#' This function provides a parallelized automation for Mplus when using R as the data generation method. When `data_gen`
#' is specified, include any arguments from the function that need to be set in the global enviornment.
#'
#' @import dplyr
#' @import doParallel
#' @import furrr
#' @import future
#' @import MplusAutomation
#' @param k Number of replications desired.
#' @param k.start Defaults to 1. Specifies the replication to start on.
#'    Useful if the simulation stopped on a specific replication and resuming without loss of work.
#' @param data_gen Can take either a single dataframe in the 'folder' or a data
#'   generation function. When it is a data generation function any arguments
#'   for data generation should be specified in this function.
#' @param seed Seed defaults to 123 but can be any integer. This ensures every
#'   replication's data is generated using a different seed but is reproducible.
#' @param ncores Defaults to the number of cores on the machine - 1.
#' @param useCores Logical. When TRUE, the mplus files will be adjusted to use
#'   the number of cores on the machine. This can speed up simulation run times.
#' @param cores_per_analysis Applies when `useCores` is TRUE. Default is `ncores/2`.
#'   If you experience issues or crashes due to memory or core use, set this lower.
#'   In testing the default will use most of a computer's CPU power but no break the simulation.
#' @param Par_plan Plan for parallel processing. Defaults to 'cluster'. Can take any argument from the 'future' package
#' @param rec Logical. Indicates if the files are in subdirectories.
#' @param folder Defaults to the pathway of your R script. Path to the root folder
#'   of where your mplus files are located.
#' @param run Logical. Defaults to T. When T the Mplus models will be run. When F models will not be run and the output files will be read in only.
#' @param results Indicates which results to collect. Supports summaries, parameters,
#'   and modindicies or any named list argument output by mplus automation. When using summaries, parameters, or modindicies
#'   mnore specific output is available.
#' @param multi_con Logical. Indicates whether multiple conditions are run in a singular instance. Default is F.
#' @param con_index A character vector. Specifies the indices for conditions to be tracked.
#' @param params_ext When `results` is 'parameters', specifies parameter type for
#'   extraction. Can take any type but defaults to 'unstandardized'.
#'   If you do not desire unstandardized parameters read in an output file to
#'   determine the name of the parameters of interest and use this as the named argument.
#' @param specific_sums Extracts specific output when `results` is 'summaries'.
#' @param specific_params Extracts specific parameters when `results` is 'parameters'.
#' @param item Extracts specific items when `results` is 'parameters'.
#' @param modV1s Used for specific output when `results` is 'mod_indicies'.
#' @param ops Operator for `modV1s`, e.g., 'BY' for factor loadings.
#' @param modV2s Second variable for `modV1s`.
#' @param custom_auto User-defined function for running and reading in models.
#' Only functions that return single dataframes each run are currently supported.
#' @param retry Logical. Defaults to TRUE. Retries with a new seed if chi is not
#'   returned by the model.
#' @param max_retry Defaults to 5. Specifies how many times a new seed should
#'   be attempted.
#'
#' @return Function returns a dataframe of all the desired parameters for each replication.
#' @examples
#' \dontrun{
#' # Loading the package
#' library(mplusParallel.automation)
#'
#' # Data Generation
#' n_people <- 500
#' n_items <- 12
#' data <- matrix(sample(1:5, n_people * n_items, replace = TRUE), ncol = n_items)
#'
#' # Writing an example input file
#' inp_content <- "
#' TITLE: TEST
#' DATA: FILE IS exdat.csv;
#' VARIABLE:
#'   Names ARE
#' i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12;
#' USEVARIABLES ARE i1-i12;
#' ANALYSIS:
#' TYPE = GENERAL;
#' PROCESSORS=6;
#' OUTPUT:
#' STANDARDIZED;
#' MODINDICES (ALL);
#' MODEL:
#'   trait1 BY
#' i1 (a1)
#' i2 (a2)
#' i3 (a3)
#' i4 (a4)
#' i5 (a5)
#' i6 (a6);
#' trait2 BY
#' i7 (a7)
#' i8 (a8)
#' i9 (a9)
#' i10 (a10)
#' i11 (a11)
#' i12 (a12);
#' i1-i12 (e);
#' trait1 @ 1
#' trait2 @ 1
#' "
#' writeLines(inp_content, "example_model_simple.inp")
#'
#' # Running the function
#' res <- mplusParallel_automation(k=5, data_gen = data_gen,
#' results = 'parameters', specific_params = c('trait1.by', 'trait2.by'))
#'
#' # Clean up
#' removeParFolders()
#' }
#' @export


mplusParallel_automation <- function(k, k.start = 1,  data_gen = NA, seed = 123, ncores = 'default', run = T,
                                     useCores = T, cores_per_analysis = 'default', Par_plan = 'cluster',
                                     rec = F, results = NULL,
                                     multi_con = F, con_index = c(),
                                     specific_sums = NULL,
                                     specific_params = NULL, item = NULL,params_ext = c('unstandardized'),
                                     modV1s = NULL,ops = NULL, modV2s = NULL,
                                     custom_auto = NULL, retry = T, max_retry = 5,
                                     folder = (dirname(rstudioapi::getActiveDocumentContext()$path))){
  ### --- Parallel Processing --- ###
  k.start = k.start
  k.range = k
  if (ncores == 'default'){
    ncores = detectCores()
  } else {
    ncores = ncores
  }
  Par_plan <- as.vector(Par_plan)
  plan(Par_plan, workers = ncores-1)
  set.seed(seed)
  all_seeds <- sample.int(100000, 100000, replace = TRUE)
  seeds <- all_seeds[1:k.range]

  ### --- Internal Functions ----##

  # --- Function to leverage cores in mplus analyses ---- #
  # -----------------------------------------------------
  # Description:
  # This function detects the number of cores available
  # and tells mplus to use this amount of cores.
  # If cores_per_analysis is set to 'default' it will use ncores/2 for each analysis
  # Otherwise, it will use the specified amount.
  # -----------------------------------------------------
  replace_line <- function(file_path) {
    lines <- readLines(file_path)
    # Check if cores_per_analysis is neither 'default' nor an integer
    if(tolower(cores_per_analysis) != 'default' && (!is.numeric(cores_per_analysis) || cores_per_analysis != floor(cores_per_analysis))) {
      stop("cores_per_analysis is not set to 'default' or an integer.")
    }
    # Throw an error if cores_per_analysis exceeds ncores
    if(is.numeric(cores_per_analysis) && cores_per_analysis > ncores) {
      stop("cores_per_analysis exceeds ncores available.")
    }

    # Determine replacement_line based on the conditions provided
    if(tolower(cores_per_analysis) == 'default') {
      replacement_line <- paste0("PROCESSORS=", ceiling(ncores/2), ";")
    } else {
      replacement_line <- paste0("PROCESSORS=", ceiling(cores_per_analysis), ";")
    }

    # Check if a line containing "PROCESSORS" exists
    if(any(grepl("^\\s*PROCESSORS.*$", lines, ignore.case = TRUE))) {
      # Replace the line containing "PROCESSORS"
      lines <- gsub("^\\s*PROCESSORS.*$", replacement_line, lines)
    } else {
      # If no replacement was done
      analysis_position <- which(grepl("analysis", lines, ignore.case = TRUE))
      if (length(analysis_position) > 0) {
        lines <- append(lines, replacement_line, after = analysis_position[1])
      } else {
        lines <- c(replacement_line, lines)
      }
    }

    writeLines(lines, file_path)
  }
  # --- Function to rewrite data being used to function call ---- #
  # -----------------------------------------------------
  # Description:
  # replaces the File is line in mplus files with exdat.csv
  # -----------------------------------------------------
  rewrite_data_in_inp_files <- function(file_path) {

    # Read the file
    file_content <- readLines(file_path, warn = FALSE)

    # Combine all lines into a single string to handle multi-line patterns
    full_text <- paste(file_content, collapse = "\n")

    # Replace the string after "DATA:" and "FILE IS" until the semicolon with "exdat.csv"
    modified_text <- gsub("(DATA:[[:space:]\n]*FILE IS[[:space:]]*).+?;", "\\1exdat.csv;", full_text)

    # Split the modified text back into lines
    modified_lines <- unlist(strsplit(modified_text, split = "\n"))

    # Write the modified content back to the file
    writeLines(modified_lines, file_path)
  }
  ### ---- Function to copy mplus models to processing folder --- ###

  # -----------------------------------------------------
  # Description:
  # This function copies the contents of the root folder 'folder'
  # into each process folder
  # -----------------------------------------------------
  copy_folder_contents <- function(source_folder, destination_folder) {
    # List all files in the source folder
    all_contents <- list.files(source_folder, full.names = TRUE)

    # Filter out directories
    files <- all_contents[!sapply(all_contents, function(path) { isTRUE(file.info(path)$isdir) })]

    # Copy each file to the destination folder
    sapply(files, function(file) {
      file.copy(file, file.path(destination_folder, basename(file)), overwrite = TRUE)
    })
  }

  ### ---- Function to create processing folders and data to use in parallel --- ###


  # -----------------------------------------------------
  # Description:
  # This function is used to automate the folder creation for paralell procesing
  # -----------------------------------------------------
  autoFun <- function(k,seed) {

    # Changing files to use exdat.csv
    path_to_files <- folder
    files <- list.files(path_to_files, pattern = "\\.inp$", full.names = TRUE)
    lapply(files, rewrite_data_in_inp_files)


    #--- Changing the cores used by each .inp file to make use of locally available maximums
    #--- Path to the directory containing the .inp files
    if(useCores == TRUE) {

      lapply(files, replace_line)
    }

    ## Begin parallel processing
    #### Error coding for failure of seed to produce results.
    ### Errors are stored, then new seed is selected and it is ran again


    pid <- as.numeric(Sys.getpid())

    session_folder <- file.path(folder, paste0("session_", pid, "_k_", k))

    if (!dir.exists(session_folder)) {
      dir.create(session_folder)
    }

    copy_folder_contents(folder, session_folder)
    subdirs <- list.dirs(folder, full.names = TRUE, recursive = rec)
    if (is.character(data_gen) && length(data_gen) == 1) {
      if (grepl("\\bseed\\b|\\bseeds\\b", data_gen, ignore.case = TRUE)) {
        warning("The text contains 'seed' or 'seeds', which may alter reproducibility.")
      }

      # Use tryCatch to detect errors
      result <- tryCatch({
        # Try to evaluate the expression
        eval(parse(text = data_gen))
      }, error = function(e) {
        # If an error occurs, stop the function with a specific message
        stop("Error during evaluation: ", e$message, '\nPlease ensure all objects used by the function are defined within it. The function also expects packages to be directly called i.e. mirt::simdata')
      })

      # Check if the "data" object exists after the evaluation
      if (!exists("data", inherits = FALSE)) {
        stop("The function passed to data_gen did not result in an object named 'data'. Halting execution.")
      }
    }

    rows_list <- list()

    if(multi_con == F){
    #-------------------------------------------------------#
    # Analyze data using mplus automation - when custom auto is entered this section is replaced
    #-------------------------------------------------------#
    if (is.null(custom_auto)){
      retries <- 0  # A counter to avoid infinite loops, consider a limit like 10 retries
      max_retries <- max_retry
      df_ready <- FALSE  # A flag to indicate when the data frame is ready
      #-------------------------------------------------------#
      # START
      # The following function runs all models in the folders set above, then extracts the relevant stats
      #-------------------------------------------------------#
      while (!df_ready && retries < max_retries) {

        filepath <- file.path(session_folder, "exdat.csv")

        write.table(data, filepath, col.names = FALSE, row.names = FALSE, sep = ",")

        if (run == T){
          runModels(session_folder, showOutput = F, recursive = rec)
        }
        test_res <- assign(paste0("sums_", pid),readModels(session_folder, what = 'summaries', recursive = rec))
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
          stop("Max retries reached. Could not get a non-null chi value.")
        }
        df_ready <- TRUE
      }



      if(results == 'summaries'){
        models_result <- assign(paste0("sums_", pid),readModels(session_folder, what = results, recursive = rec))
        res <- models_result[[2]]
        res <- res %>%
          mutate(Rep = k) %>%
          select(Rep, everything())
        new_row <- res
        rows_list[[length(rows_list) + 1]] <- new_row

      } else if (results == 'parameters'){
        models_result <- assign(paste0("sums_", pid),readModels(session_folder, what = results, recursive = rec))
        res <- models_result
        new_data <- res[[2]][[params_ext]]

        new_data <- new_data %>%
          mutate(Rep = k, ParType = params_ext) %>%
          select(Rep, everything())
        new_row <- new_data
        rows_list[[length(rows_list) + 1]] <- new_row

      } else if (results == 'mod_indices'){
        models_result <-  assign(paste0("sums_", pid),readModels(session_folder, recursive = rec))
        res <- models_result[["mod_indices"]]
        new_data <- res
        new_data <- new_data %>%
          mutate(Rep = k) %>%
          select(Rep, everything())
        new_row <- new_data
        rows_list[[length(rows_list) + 1]] <- new_row

      } else {
        models_result <-  assign(paste0("sums_", pid),readModels(session_folder, recursive = rec))
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
}
    }
    else {
      #-------------------------------------------------------#
      # Analyze data using mplus automation - when custom auto is entered this section is replaced
      #-------------------------------------------------------#
      if (is.null(custom_auto)){
        df_list <- list()
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
      # Identify the line to split the 'gen' string after using regex
      pattern <- "data"

      # Combine the parts, adding std_MFun in between
      # Split 'da' into individual lines
      lines <- strsplit(data_gen, "\n")[[1]]

      # Find all lines that match the pattern
      matching_lines <- which(grepl(pattern, lines, perl = TRUE))

      # Identify the last matching line
      last_matching_line <- tail(matching_lines, 1)

      # Insert 'std_MFun' after the last matching line
      new_lines <- c(lines[1:last_matching_line], std_MFun, lines[(last_matching_line + 1):length(lines)])
      # Recombine the lines
      combined_string <- paste(new_lines, collapse = "\n")


      # The new code to be added
      new_code <- c("", "df_final <- do.call(rbind, df_list)", "return(df_final)")
      lines2 <- strsplit(combined_string, "\n")[[1]]
      brace_lines <- which(grepl("}", lines2))
      # Identify the last line that contains }
      last_brace_line <- tail(brace_lines, 1)
      # Insert the 'new_code' after the last brace line
      updated_lines <- c(lines2[1:last_brace_line], new_code)
      # Recombine the lines
      updated_combined_string <- paste(updated_lines, collapse = "\n")

      eval(parse(text = updated_combined_string))

      } else {
        std_MFun <- custom_auto
        # Identify the line to split the 'gen' string after using regex
        # Identify the line to split the 'gen' string after using regex
        pattern <- "data"

        # Combine the parts, adding std_MFun in between
        # Split 'da' into individual lines
        lines <- strsplit(data_gen, "\n")[[1]]

        # Find all lines that match the pattern
        matching_lines <- which(grepl(pattern, lines, perl = TRUE))

        # Identify the last matching line
        last_matching_line <- tail(matching_lines, 1)

        # Insert 'std_MFun' after the last matching line
        new_lines <- c(lines[1:last_matching_line], std_MFun, lines[(last_matching_line + 1):length(lines)])
        # Recombine the lines
        combined_string <- paste(new_lines, collapse = "\n")


        # The new code to be added
        new_code <- c("", "df_final <- do.call(rbind, df_list)", "return(df_final)")
        lines2 <- strsplit(combined_string, "\n")[[1]]
        brace_lines <- which(grepl("}", lines2))
        # Identify the last line that contains }
        last_brace_line <- tail(brace_lines, 1)
        # Insert the 'new_code' after the last brace line
        updated_lines <- c(lines2[1:last_brace_line], new_code)
        # Recombine the lines
        updated_combined_string <- paste(updated_lines, collapse = "\n")

        eval(parse(text = updated_combined_string))

      }

    }
  }

  ### --- Function for parallel processing --- ####
  options(future.rng.onMisuse= 'ignore')
  df.all <- future_map2(1:k.range, seeds, autoFun)



  ### --- Final Dataframe partioning --- ###
  df.final <- as.data.frame(do.call(rbind, df.all))
if (length(custom_auto) > 0 && is.na(custom_auto)) {

  if (results == 'summaries' && !is.null(specific_sums)) {

      # Ensure 'Rep' is always included
      specific_sums <- unique(c("Rep", specific_sums))

      # Subset df.final by the columns present in specific_sums
      # Note: select() will automatically retain the "Rep" column
      df.final <- df.final %>%
        select(all_of(specific_sums))

      # Check for missing columns after subsetting
      missing_cols <- setdiff(specific_sums, colnames(df.final))

      # Throw a warning if some columns are missing
      # Warning for missing columns, only for 'summaries'
      if (results == 'summaries' && length(missing_cols) > 0) {
        warning(paste("The following columns were not found in df.final:",
                      paste(missing_cols, collapse = ", "),
                      '\n Make sure you are not requesting information from a different Results call'))
      }
    } else if (results == 'parameters' && !is.null(specific_params)) {

        df.fin <- df.final[df.final$paramHeader %in% toupper(specific_params),]
        df.final = df.fin


      if (!is.null(item)) {
        df. <- df.fin[df.fin$param %in% toupper(item),]
        df.final <- df.
      }


    } else if (results == 'mod_indices' && is.null(modV1s)) {


        df.fin <- df.final[df.final$modV1 %in% toupper(modV1s)]
        df.final <- df.fin


      if (!is.null(ops)) {
        df. <- df.fin[df.fin$op %in% toupper(ops)]
        df.final <- df.
      }

      if (!is.null(modV2s)) {
        df.fi <- df.[df.$modV2 %in% toupper(modV2)]
        df.final = df.fi
      }


    } else {

    }
} else{
  df.final = df.final
}

  ### --- Removing of subdirectories created --- ###

  return(df.final)
}
