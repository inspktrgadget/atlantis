load("varMod/varMod_resByYear.RData")
load("bootMod/bootMod_resByYear.RData")
load("halfBoot/halfBoot_resByYear.RData")
load("fullBoot/fullBoot_resByYear.RData")

# assemble and merge numbers data
models <- c("varMod", "bootMod", "halfBoot", "fullBoot")

bmn_summary <- 
    lapply(models, function(x) {
        get(sprintf("%s_res_by_year_summary", x)) %>%
            select(year, area, contains("total.number"),  contains("total.biomass")) %>%
            mutate_at(vars(contains("total.number"), contains("total.biomass")), funs(./1e6)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .)


# merge res_by_year data.frames together
res_by_year <- 
    lapply(models, function(x) {
        get(sprintf("%s_res_by_year", x)) %>%
            mutate(model = x)
    }) %>%
    do.call("rbind", .)


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


# read in atlantis biomass index data
atl_biomass <- 
    read.table('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutBiomIndx.txt', 
               header=T) %>%
    mutate(year = 1948:2013) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    mutate(atl.biomass = FCD / 1000)

# compute number at year for atlantis
atl_numbers <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    group_by(year, month) %>%
    summarize(total.number = sum(count) / 1e6)