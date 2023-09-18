## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",  root.dir = 'C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/MultiCon_Example'
)


folder = "C:/Users/jakep/OneDrive - McGill University/EMA Partnership/Papers/mplusParallel.automation/vignettes/MultiCon_Example"

## ----setup, results='hide',message=FALSE,warning=FALSE, echo = T--------------
library(mplusParallel.automation)

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
    a_groupB[1] <- a[1] + es
    
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
i1 (a1)
i2 (b2)
i3 (b3)
i4 (b4)
i5 (b5)
i6 (b6);
trait2 BY 
i7 (a7)
i8 (b8)
i9 (b9)
i10 (b10)
i11 (b11)
i12 (b12);
"
full_path1 <- file.path(folder, "example_model.inp")
# Write to an inp file
writeLines(inp_content, full_path1)


## -----------------------------------------------------------------------------

res <- mplusParallel_automation(k=5, multi_con = T, data_gen = data_gen, con_index = c('es','s'), results = 'summaries', folder = folder) 


## ---- comment= NA-------------------------------------------------------------
res[c(1,6,11,16,20), c(30:31,33:34)]

#removeParFolders()

