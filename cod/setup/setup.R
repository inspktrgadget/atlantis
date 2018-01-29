## general setup for gadget models
library(plyr)
library(dplyr)
library(mfdb)
library(Rgadget)


setwd("/home/pfrater/gadget/models/atlantis")
source("../functions/gadgetUtils.R")

# some basic name and directory structure defaults
setup_d <- "cod/setup"
gd <- gadget_directory("cod/codModel")
species_name <- "cod"
stock0 <- "cod0"
stock <- "cod"
stocknames <- c(stock0, stock)

# connect to mfdb database
mdb <- mfdb("atlantis_logisticsurvey")

# fetch defaults
source(sprintf("%s/setupDefaults.R", setup_d))

# setup and write out time and area files
source(sprintf("%s/setupTimeArea.R", setup_d))

# setup fleets and model
source(sprintf("%s/setupFleet.R", setup_d))
source(sprintf("%s/setupModel.R", setup_d))

# setup params and likelihood components
source(sprintf("%s/setupCatchDistribution.R", setup_d))
source(sprintf("%s/setupIndices.R", setup_d))
source(sprintf("%s/setupLikelihood.R", setup_d))
source(sprintf("%s/setupParams.R", setup_d))

file.copy(sprintf("%s/run.R", setup_d), gd$dir)
file.copy(sprintf("%s/mfrun.R", setup_d), gd$dir)
file.copy(sprintf("%s/optinfofile", setup_d), gd$dir)

