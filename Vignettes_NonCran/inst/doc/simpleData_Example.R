## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  root.dir = 'C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/simpleData_Example'
)
folder = "C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/simpleData_Example"


## ----results='hide',message=FALSE,warning=FALSE, echo = T---------------------
library(mplusParallel.automation)

## -----------------------------------------------------------------------------

data_genFunk <- 
"
n_people <- 500
n_items <- 12

# Generate the data
data <- matrix(sample(1:5, n_people * n_items, replace = TRUE), ncol = n_items)
"

## -----------------------------------------------------------------------------
inp_content <- "
TITLE: TEST
DATA: FILE IS exdat.csv;
VARIABLE:
  Names ARE
i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12;
USEVARIABLES ARE i1-i12;
ANALYSIS:
TYPE = GENERAL;
PROCESSORS=6;
OUTPUT:
STANDARDIZED;
MODINDICES (ALL);
MODEL:
  trait1 BY
i1 (a1)
i2 (a2)
i3 (a3)
i4 (a4)
i5 (a5)
i6 (a6);
trait2 BY
i7 (a7)
i8 (a8)
i9 (a9)
i10 (a10)
i11 (a11)
i12 (a12);
i1-i12 (e);
trait1 @ 1
trait2 @ 1
"
full_path1 <- file.path(folder, "example_model_simple.inp")
# Write to an inp file
writeLines(inp_content, full_path1)

## -----------------------------------------------------------------------------
res <- mplusParallel_automation(k=5, data_gen = data_genFunk, results = 'parameters', specific_params = c('trait1.by', 'trait2.by'), folder = folder)

head(res)

## ---- eval=F------------------------------------------------------------------
#  #removeParFolders()

