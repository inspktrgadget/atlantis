library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
setwd('~/gadget/models/atlantis')
gadget_st_year <- 1983

is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF')

is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
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


# compare biomass by year in gadget to atlantis
atl.biomass <- 
    read.table('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutBiomIndx.txt', 
               header=T) %>%
    mutate(year = 1948:2013) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    mutate(atl.biomass = FCD*1000)
