# EIFA model for PROMIS Wave 1 data

# Here, we pick emotional distress items that appear on Form H

# load in full PROMIS data (Wave 1)
library(haven)
library(dplyr)
library(psych)
library(readxl)

dat <- read_spss("PROMIS 1 Wave 1.sav")

# Physical function (V1.2)
PFcols<-c(paste0("PFC",32:38),paste0("PFA",32:38),
          paste0("PFB",32:38))
# Fatigue (V1.0)
FAcols<-c(paste0("FATIMP",32:38),paste0("FATEXP",32:38))
# Social (V1.0)
SRcols<-c(paste0("SRPSAT",32:38))
# Pain (V1.1)
PAcols<-c(paste0("PAININ",32:38))

cols<-c(PFcols,FAcols,SRcols,PAcols)
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
eifa <- mirt(dats, 4, method="QMCEM", SE=FALSE,
              technical = list(NCYCLES = 5000))

# remove data so that saving out workspace is smaller...
rm(dat)
rm(dats)

# save out workspace
save.image(file = "eifa_formQ.RData")

###############################################################################
# Load results and summarize
library(readxl)
library(mirt)

load("eifa_formQ.RData")

df <- read.csv("text_formQ.csv")
# oblique rotation
promax.rot <- summary(eifa, rotate="promax")
# export results, similar format to what Nigel used
id <- df[c("Label", "Item", "Factor")]
# r <- abs(factor_fit$residual[lower.tri(factor_fit$residual)])
output_file <- "eifa_formQ.xlsx"
openxlsx::write.xlsx(list(
  Loadings = cbind(id, data.frame(F1 = promax.rot$rotF[,1],
                                  F2 = promax.rot$rotF[,2],
                                  F3 = promax.rot$rotF[,3],
                                  F4 = promax.rot$rotF[,4])),
  Phi = cbind(Factor = rownames(promax.rot$fcor), promax.rot$fcor),
  Item_statistics = data.frame(Label = id$Label, Communality = promax.rot$h2)
), output_file, overwrite = TRUE)