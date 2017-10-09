library(ggplot2)
library(tidyr)
library(dplyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
setwd('~/gadget/models/atlantis')
source('../functions/vbParams.R')
is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF')

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
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
atl.had <- atlantis_fg_tracer(is_dir, is_area_data, fg_group) %>% filter(count >= 1)

# calculate growth parameters
atl.sub <- 
    atl.had %>%
    group_by(month, length, age)

# compute growth parameters using nlm on functions in vbParams.R
vbMin <- 
    atl.sub %>%
    #group_by(month) %>%
    summarize(linf = nlm(vb.sse, c(100, 0.05, -1), length, age)$estimate[1],
              k = nlm(vb.sse, c(100, 0.05, -1), length, age)$estimate[2],
              t0 = nlm(vb.sse, c(100, 0.05, -1), length, age)$estimate[3])


# looks pretty good, use parameters in vbMin for growth
gr.plot <- 
    ggplot(data=atl.sub, aes(x=age, y=length)) + geom_point() + 
    stat_function(fun=function (x) vb(vbMin$estimate[1], 
                                      vbMin$estimate[2], x,
                                      vbMin$estimate[3])) + 
    theme_bw()