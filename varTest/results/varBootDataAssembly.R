load("varMod/varMod.RData")
load("bootMod/bootMod.RData")
load("halfBoot/halfBoot.RData")
load("fullBoot/fullBoot.RData")

# assemble and merge numbers data
models <- c("varMod", "bootMod", "halfBoot", "fullBoot")

bmn_summary <- 
    lapply(models, function(x) {
        get(sprintf("%s_summary", x)) %>%
            select(year, step, area, contains("total.number"),  contains("total.biomass")) %>%
            mutate_at(vars(contains("total.number"), contains("total.biomass")), funs(./1e6)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .)

bmn_summary_by_age <- 
    lapply(models, function(x) {
        get(sprintf("%s_summary_by_age", x)) %>%
            select(year, step, area, age, contains("total.number"),  contains("total.biomass")) %>%
            mutate_at(vars(contains("total.number"), contains("total.biomass")), funs(./1e6)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(model, year, step, area, age) %>%
    summarize_all(funs(sum))


# merge res_by_year data.frames together
res_by_step <- 
    lapply(models, function(x) {
        get(sprintf("%s_resByStep", x)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .)

res_by_step_age <- 
    lapply(models, function(x) {
        get(sprintf("%s_resByStep_age", x)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .)


# calculate ssb in gadget
gadget_ssb <- 
    res_by_step_age %>%
    filter(age >= 4) %>%
    group_by(model, model_index, year, step, area) %>%
    summarize(ss.number = sum(total.number)/1e6, 
              ssb = sum(total.biomass)/1e6)

gadget_ssb_summary <- 
    gadget_ssb %>%
    group_by(model, year, step, area) %>%
    summarize(ssb_median = median(ssb),
              ssb_se = sd(ssb) / sqrt(n()),
              ssb_ci_lower = sort(ssb)[ceiling(n() * 0.025)],
              ssb_ci_upper = sort(ssb)[floor(n() * 0.975)])

gadget_ssb_by_age <- 
    res_by_step_age %>%
    filter(age >= 4) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(model, model_index, year, step, area, age) %>%
    summarize(ss.number = sum(total.number)/1e6, 
              ssb = sum(total.biomass)/1e6)
    
gadget_ssb_summary_by_age <- 
    gadget_ssb_by_age %>%
    group_by(model, year, step, area, age) %>%
    summarize(ssb_median = median(ssb),
              ssb_se = sd(ssb) / sqrt(n()),
              ssb_ci_lower = sort(ssb)[ceiling(n() * 0.025)],
              ssb_ci_upper = sort(ssb)[floor(n() * 0.975)])

#-------------------------------------------------------------------------
# read in atlantis data
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


# compute number at year for atlantis
atl_numbers <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    group_by(year, month) %>%
    summarize(atl.number = sum(count) / 1e6)

atl_numbers_by_age <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    group_by(year, month, age) %>%
    summarize(atl.number = sum(count) / 1e6)


# compute atlantis biomass data
atl_biomass <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    mutate(biomass = count * weight)

atl_total_biomass <- 
    atl_biomass %>%
    group_by(year, month) %>%
    summarize(atl.biomass = sum(biomass) / 1e9)

atl_ssb <- 
    atl_biomass %>%
    filter(maturity_stage > 1) %>%
    group_by(year, month) %>%
    summarize(atl.ssb = sum(biomass) / 1e9)
    
# compute biomass by age
atl_total_biomass_by_age <- 
    atl_biomass %>%
    group_by(year, month, age) %>%
    summarize(atl.biomass = sum(biomass) / 1e9)

atl_ssb_by_age <- 
    atl_biomass %>%
    filter(maturity_stage > 1) %>%
    group_by(year, month, age) %>%
    summarize(atl.ssb = sum(biomass) / 1e9)
