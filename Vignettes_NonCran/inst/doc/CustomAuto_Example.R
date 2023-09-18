## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  root.dir = 'C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/CustomAuto_Example'
)
folder = "C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/CustomAuto_Example"



## ----setup, results='hide',message=FALSE,warning=FALSE, echo = T--------------
library(mplusParallel.automation)

# folder = your path here

## ----datagen------------------------------------------------------------------

data_gen<-"

N = c(500,1000)
dif_effect_sizes = c(0.6, 1)

# Initial item parameters
a <- rep(1, 12) # Assuming all items have equal discrimination
b <- seq(-2, 2, length.out = 12) # Assuming items have varying difficulty levels
for(s in N){
for (es in dif_effect_sizes) {
    # Update discrimination of item 1 for Group B
    a_groupB <- a
    a_groupB[1] <- a[3] + es
    
    # Simulate data for both groups
    group_A <- mirt::simdata(a, b, s/2, itemtype = 'dich')

    group_B <- mirt::simdata(a_groupB, b, s/2, itemtype = 'dich')
    group1 <- matrix(rep(1, s/2))
    group2 <- matrix(rep(2, s/2))
    group <- rbind(group1,group2)
    
    
    # Combine the groups' data
    combined_data <- rbind(group_A, group_B)
    
    # Append to all_data
    data <- cbind(group,combined_data)
}}
"


## -----------------------------------------------------------------------------
inp_content <- "
TITLE: TEST
DATA: FILE IS exdat.csv;
VARIABLE:
  Names ARE
group
i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12;
USEVARIABLES ARE i1-i12;
GROUPING IS group(1=G1, 2=G2)
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
MODEL G1:
MODEL G2:
  trait1 BY 
i1 (b1)
i2 (b2)
i3 (b3)
i4 (b4)
i5 (b5)
i6 (a6);
trait2 BY 
i7 (a7)
i8 (b8)
i9 (b9)
i10 (b10)
i11 (b11)
i12 (b12);

MODEL TEST:
! Testing Item 2 for DIF
0 = a3 - b3; 
"

# Write to an inp file
full_path1 <- file.path(folder, "exampleCustom_model_item3.inp")
writeLines(inp_content, full_path1)
inp_content <- "
TITLE: TEST
DATA: FILE IS exdat.csv;
VARIABLE:
  Names ARE
group
i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12;
USEVARIABLES ARE i1-i12;
GROUPING IS group(1=G1, 2=G2)
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
MODEL G1:
MODEL G2:
  trait1 BY 
i1 (b1)
i2 (b2)
i3 (b3)
i4 (b4)
i5 (b5)
i6 (a6);
trait2 BY 
i7 (a7)
i8 (b8)
i9 (b9)
i10 (b10)
i11 (b11)
i12 (b12);

MODEL TEST:
! Testing Item 2 for DIF
0 = a2 - b2; 
"
full_path2 <- file.path(folder, "exampleCustom_model_item2.inp")
# Write to an inp file
writeLines(inp_content, full_path2)


## -----------------------------------------------------------------------------

custom_auto<-"

filepath <- file.path(session_folder, 'exdat.csv')
write.table(data, filepath, col.names = FALSE, row.names = FALSE, sep = ',')

runModels(session_folder, showOutput = TRUE, recursive = F)

assign(paste0('sums_', pid), f <- readModels(folder, what = 'summaries', recursive = T))

for (i in 1:length(get(paste0('sums_', pid)))) {
  # Get the relevant data
  filename <- get(paste0('sums_', pid))[[i]][['summaries']][['Filename']]
  
  # This extracts the item numbers from the .inp files
itemNumber <- as.integer(gsub(\".*item(\\\\d+).*\", \"\\\\1\", filename, ignore.case = TRUE))


  
  if (itemNumber== 3) {
    itemType <- 'DIF'
  } else {
    itemType <- 'NoDIF'
  }
  
  waldchisq_value <- get(paste0('sums_', pid))[[i]][[2]][['WaldChiSq_Value']]
  waldchisq_pvalue <- get(paste0('sums_', pid))[[i]][[2]][['WaldChiSq_PValue']]
  
  
  # Add this data as a new row to the dataframe
  new_row <- c(Rep = k, samSize = s, effect = es, 
               itemNumber = itemNumber, itemType = itemType,
               WaldChiSq_Value = waldchisq_value, 
               WaldChiSq_PValue = waldchisq_pvalue)
  
  
  # Append the new row to the data frame
  rows_list[[i]] <- new_row
  
}


df <- do.call(rbind,rows_list)
return(df)

"


## ----results='hide',message=FALSE,warning=FALSE, echo = T---------------------

res <- mplusParallel_automation(k=5,seed = 4, custom_auto = custom_auto, multi_con = T, data_gen = data_gen, folder = folder ) 


## ---- comment= NA-------------------------------------------------------------
head(res)

#removeParFolders()

