## general setup for gadget model on greater silver smelt (Argentina silus)
library(plyr)
library(dplyr)
library(mfdb)
library(Rgadget)


setwd('/home/pfrater/gadget/models/atlantis')
source('../functions/gadgetUtils.R')
setup.d <- 'haddock/setup'

# connect to mfdb database
mdb <- mfdb('Atlantis-Iceland')

# fetch defaults
source(sprintf('%s/setupDefaults.R', setup.d))

# setup and write out time and area files
source(sprintf('%s/setupTimeArea.R', setup.d))

# update fleets and/or models
source(sprintf('%s/setupFleet.R', setup.d))
source(sprintf('%s/setupModel.R', setup.d))

# run gadget -s -log logfile.txt from terminal

# update params and likelihood components
source(sprintf('%s/setupParams.R', setup.d))
source(sprintf('%s/setupCatchDistribution.R', setup.d))
source(sprintf('%s/setupIndices.R', setup.d))
source(sprintf('%s/setupLikelihood.R', setup.d))

file.copy(sprintf('%s/itterfitter.sh', setup.d), gd$dir)
file.copy(sprintf('%s/run.R', setup.d), gd$dir)
file.copy(sprintf('%s/mfrun.R', setup.d), gd$dir)
file.copy(sprintf('%s/optinfofile', setup.d), gd$dir)

