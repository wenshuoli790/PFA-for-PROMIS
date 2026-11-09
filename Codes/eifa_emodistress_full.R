# EIFA model for PROMIS Wave 1 data

# Should not include EDANX56 per Josh's recommendation
# i.e., that item used different response options

# load in full PROMIS data (Wave 1)
library(haven)
library(dplyr)
library(psych)
library(readxl)

dat <- read_spss("PROMIS 1 Wave 1.sav")

# anxiety (V1.0)
anxcols<-c("EDANX01","EDANX02","EDANX03","EDANX05","EDANX07","EDANX08",
           paste0("EDANX",c(12,13,16,18,20,21,24,26,27,30,33,37,40,
                            41,44,46,47,48,49,51,53,54,55)))
# depression (V1.0)
depcols<-c("EDDEP04","EDDEP05","EDDEP06","EDDEP07","EDDEP09",
           paste0("EDDEP",c(14,17,19,21,22,23,26,27,28,29,30,31,
                            35,36,39,41,42,44,45,46,48,50,54)))
# anger (V1.1)
angcols<-c("EDANG01","EDANG03","EDANG04","EDANG05","EDANG09",
           "EDANG10","EDANG11","EDANG15","EDANG16",
           "EDANG17","EDANG18","EDANG21","EDANG22","EDANG26",
           "EDANG30","EDANG31","EDANG35","EDANG37","EDANG42",
           "EDANG48","EDANG55")

cols<-c(anxcols,depcols,angcols)
dats<-dat[,cols]

# remove people that have missing data on all cases
dats <- dats %>% filter(if_any(everything(), ~ !is.na(.x)))

# sanity check
describe(dats)


###############################################################################
# Code to do exploratory item factor analysis model

library(mirt)
eifa <- mirt(dats, 3, method="QMCEM", SE=FALSE,
             technical = list(NCYCLES = 5000))

# remove data so that saving out workspace is smaller...
rm(dat)
rm(dats)

# save out workspace
save.image(file = "eifa_emodistress_full.RData")


###############################################################################
# Load results and summarize
library(readxl)
library(mirt)

load("eifa_emodistress_full.RData")

df <- read.csv("text_emodistress_full.csv")
target <- matrix(rep(0, 78*3), nrow=78, ncol=3)
target[df$Factor=="anx",1] <- 1
target[df$Factor=="dep",2] <- 1
target[df$Factor=="ang",3] <- 1

# oblique rotation or target rotation?
promax.rot <- summary(eifa, rotate="promax")
target.rot <- summary(eifa, rotate="targetQ", Target=target)

# export results, similar format to what Nigel used
id <- df[c("Label", "Item", "Factor")]
output_file <- "eifa_emodistress_full.xlsx"
openxlsx::write.xlsx(list(
  Loadings = cbind(id, data.frame(F1 = promax.rot$rotF[,1],
                                  F2 = promax.rot$rotF[,2],
                                  F3 = promax.rot$rotF[,3])),
  Phi = cbind(Factor = rownames(promax.rot$fcor), promax.rot$fcor),
  Item_statistics = data.frame(Label = id$Label, Communality = promax.rot$h2)
), output_file, overwrite = TRUE)
