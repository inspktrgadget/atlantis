library(tidyverse)
library(mfdb)
library(mfdbatlantis)
setwd("~/gadget/models/atlantis")
source("../functions/vbParams.R")
is_dir <- atlantis_directory("~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF")

is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)["name",],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# assemble and import cod 
fgName <- "Cod"
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group) %>% filter(count > 0)

#---------------------------------------------------------------------------------------
# calculate growth parameters for original atlantis data
atl_sub <- 
    is_fg_count %>%
    select(year, month, age, length, count) %>%
    group_by(year, month, age, length) %>%
    summarize(count = sum(count))


# compute growth parameters using nlm on functions in vbParams.R
vb_min <- nlm(vb_sse, c(130, 0.15, -0.5), atl_sub$length, atl_sub$age)

# trying the same as above but with nls and adding counts as a weight
nls_growth <- nls(length ~ vb(linf, k, t0, age), data=atl_sub, 
                  start = c(linf = 125, k = 0.15, t0 = 0),
                  weights = count)

#---------------------------------------------------------------------------------------
# now check growth for length at age of fish parsed to single year cohorts
# distribute 2 year atlantis age groups to single year classes
source("functions/calcGrowth.R")
source("functions/parseAges.R")
#source("cod/modelCheck/getAtlantisMort3.R")
m_vals <- data.frame(age = 0:19, m = 0.338)
# add mortality and parse ages based on m
age_count <- 
    left_join(is_fg_count, m_vals) %>%
    parseAges(.) %>%
    arrange(year, month, day, area, depth, age)

vbMin <- 
    data.frame(year = 1948:2013, 
               month = sort(rep(1:12, 66))) %>%
    mutate(linf = 134,
           k = 0.15,
           t0 = -0.45) %>%
    arrange(year, month)


# redistribute lengths based on growth params
smooth_len <- 
    age_count %>% 
    filter(count >= 1) %>%
    mutate(length = vb(linf=134, k=0.13, t0=0, 
                       age = age + (month / 12))) %>%
    # mutate(length = ifelse(age == 0, vb(linf, k, (t0), age),
    #                        vb(linf, k+0.01, t0, age))) %>%
    select(depth, area, year, month, day, group, cohort, weight, length, 
           maturity_stage, age, count)

# compute vb growth parameters
parsed_vb_min <- nlm(vb_sse, c(130, 0.14, -0.5), smooth_len$length, smooth_len$age)

# doing the same but with nls
parsed_nls_growth <- 
    lapply(1:100, function(x) {
        growth <- 
            nls(length ~ vb(linf, k, t0, age), data=sample_n(smooth_len, 1e4), 
                start = c(linf = 125, k = 0.15, t0 = 0),
                weights = count)
        return(data.frame(t(coef(growth))))
    }) %>%
    do.call("rbind", .)
parsed_nls_params <- apply(parsed_nls_growth, 2, mean)

# plot to compare
plot(length ~ age, data=atl_sub, xlab = "Age", ylab = "Length (cm)")
points(length ~ age, data=sample_n(smooth_len, 1e4), col = rgb(1,0,0,0.01))
curve(vb_optimizer(parsed_vb_min$estimate, x), add = TRUE)
curve(vb_optimizer(parsed_nls_params, x), add = TRUE, col = "red")


#-----------------------------------------------------------------------------------
# so the two growth curves are different; now we need to get recl parameter for gadget
source("../functions/vbSimple.R")
parsed_vb_simple_params <- nlm(vb_simple_sse, c(128, 0.13, 25), 
                               smooth_len$length, smooth_len$age)
parsed_vb_simple_nls <- 
    lapply(1:100, function(x) {
        growth <- 
            nls(length ~ vb_simple(linf, k, recl, age), data=sample_n(smooth_len, 1e4), 
                start = c(linf = 125, k = 0.15, recl = 23),
                weights = count)
        return(data.frame(t(coef(growth))))
    }) %>%
    do.call("rbind", .)
parsed_vb_simple_nls_params <- apply(parsed_vb_simple_nls, 2, mean)
