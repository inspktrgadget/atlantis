library(plyr)
library(dplyr)
library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)

setwd('~/gadget/models/atlantis')

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
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)

fy <- filter(is_fg_count, year==1948)

init <- fy %>% group_by(age) %>% summarize(init.vals = sum(count))

init.val <- init$init.vals[1]
age <- 0:18
init.func <- function(m) {
    init.val*exp(-m*(age+1))
} 

init.vals.sse <- function(params) {
    m <- params[1]
    init.vals <- init.func(m);
    init.vals.sse <- sum((init.vals - init$init.vals)^2);
    return(init.vals.sse)
}

    init.plot <- function(age,m) {
        init.val*exp(-m*(age+1))
    }
    
    plot(init.vals ~ age, data=init)
    curve(init.plot(x, 0.3), add=T, col='red')
