# EIFA model for PROMIS Wave 1 data

# Here, we pick emotional distress items that appear on Form H

# load in full PROMIS data (Wave 1)
library(haven)
library(dplyr)
library(psych)
library(readxl)

dat <- read_spss("PROMIS 1 Wave 1.sav")

# anxiety (V1.0)
anxcols<-c("EDANX01","EDANX02","EDANX03","EDANX05","EDANX07")

# depression (V1.0)
depcols<-c("EDDEP04","EDDEP05","EDDEP06","EDDEP07")

# anger (V1.1)
angcols<-c("EDANG01","EDANG03","EDANG04","EDANG05")

cols<-c(anxcols,depcols,angcols)
dats<-dat[,cols]

# remove people that have missing data on all cases
dats <- dats %>% filter(if_any(everything(), ~ !is.na(.x)))

# sanity check
describe(dats)


###############################################################################
# Code to do exploratory item factor analysis model

library(mirt)

# Used QMCEM since it seemed not too slow, and might give tighter converged
# solution than MHRM w/o additional tuning, at least
eifa <- mirt(dats, 3, method="QMCEM", SE=FALSE,
              technical = list(NCYCLES = 5000))

# remove data so that saving out workspace is smaller...
rm(dat)
rm(dats)

# save out workspace
save.image(file = "eifa_emodistress_formH.RData")

###############################################################################
# Load results and summarize
library(readxl)
library(mirt)

load("eifa_emodistress_formH.RData")

df <- read.csv("text_emodistress_formH.csv")
target <- matrix(rep(0, 13*3), nrow=13, ncol=3)
target[df$Factor=="anx",1] <- 1
target[df$Factor=="dep",2] <- 1
target[df$Factor=="ang",3] <- 1

# oblique rotation or target rotation?
promax.rot <- summary(eifa, rotate="promax")
target.rot <- summary(eifa, rotate="targetQ", Target=target)

# export results, similar format to what Nigel used
id <- df[c("Label", "Item", "Factor")]
# r <- abs(factor_fit$residual[lower.tri(factor_fit$residual)])
output_file <- "eifa_emodistress_formH.xlsx"
openxlsx::write.xlsx(list(
  Loadings = cbind(id, data.frame(F1 = promax.rot$rotF[,1],
                                  F2 = promax.rot$rotF[,2],
                                  F3 = promax.rot$rotF[,3])),
  Phi = cbind(Factor = rownames(promax.rot$fcor), promax.rot$fcor),
  Item_statistics = data.frame(Label = id$Label, Communality = promax.rot$h2)
), output_file, overwrite = TRUE)