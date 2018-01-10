## this code is exactly the same as initdb.R in the initdb directory
## just without the mfdb import lines.
library(plyr)
library(dplyr)
library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)

setwd('~/gadget/models/atlantis')
# source files for both functions and outside data
source('functions/stripAgeLength.R')
source('functions/pauls_atlantis_tracer.R')
source('functions/getCodDiscards.R')
source('functions/commCatchAges.R')
source('functions/discardAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('cod/initdb/getCodLengthVar.R') # source cod length sd at age group

is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF')
is_run_options <- atlantis_run_options(is_dir)

is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# assemble and import cod 
fgName <- 'Cod'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)
is_fg_count <- 
    is_fg_count %>% 
    group_by(year, month) %>%
    mutate(fd = min(day)) %>%
    ungroup() %>%
    filter(day == fd)


