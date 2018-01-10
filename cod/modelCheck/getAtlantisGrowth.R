library(tidyverse)
library(mfdb)
library(mfdbatlantis)
setwd('~/gadget/models/atlantis')
source('../functions/vbParams.R')
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
atl_cod <- atlantis_fg_tracer(is_dir, is_area_data, fg_group) %>% filter(count > 0)

# calculate growth parameters
atl_sub <- 
    atl_cod %>%
    select(year, month, age, length, count) %>%
    group_by(year, month, age, length) %>%
    summarize(count = sum(count))


# compute growth parameters using nlm on functions in vbParams.R
length <- atl_sub$length
age <- atl_sub$age
vbMin <- nlm(vb_sse, c(160, 0.05, -1), length, age)

# trying the same as above but with nls and adding counts as a weight
nls_growth <- nls(length ~ vb(linf, k, t0, age), data=atl_sub, 
                  start = c(linf = 125, k = 0.15, t0 = 0),
                  weights = count)

# looks pretty good, use parameters in vbMin for growth
grPlot <- 
    ggplot(data=atl_sub, aes(x=age, y=length)) + geom_point() +
    stat_function(fun = vb, 
                  args = list(linf = coef(nls_growth)["linf"],
                              k = coef(nls_growth)["k"],
                              t0 = coef(nls_growth)["t0"]))

# trying the nls option for each year/month combo to obtain median values
nls_to_opt <- function(data) {
    return(nls(length ~ vb(linf, k, t0, age), data=data, 
               start = c(linf = 125, k = 0.15, t0 = 0),
               weights = count))
}

atl_sub_ym <- 
    atl_sub %>%
    group_by(year, month) %>%
    do(model = coef(nls_to_opt(.))) %>%
    group_by(year, month) %>%
    mutate(linf = model[[1]][1],
           k = model[[1]][2],
           t0 = model[[1]][3]) %>%
    select(-model)


medianParams <- 
    atl_sub_ym %>%
    ungroup() %>%
    summarize(linf = median(linf),
              k = median(k),
              t0 = median(t0))

paramHist <- 
    ggplot(data=gather(atl_sub_ym, key = key, value, -year, -month),
           aes(x=value)) + geom_histogram() + facet_wrap(~key, scales = "free_x") + 
    geom_vline(data=gather(medianParams), aes(xintercept = value))

medParamGrPlot <- 
    ggplot(data=atl_sub, aes(x=age, y=length)) + geom_point() + 
    stat_function(fun = vb, 
                  args = list(linf = medianParams$linf,
                              k = medianParams$k,
                              t0 = medianParams$t0))













