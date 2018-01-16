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


mfdb_import <- TRUE

if (mfdb_import) {
    # mfdb('atlantis_constSurvey_0.001', destroy_schema=T)
    mfdb("atlantis_logisticsurvey", destroy_schema=TRUE)
    mdb <- mfdb('atlantis_logisticsurvey')
}

# read in dir and options
is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF')
is_run_options <- atlantis_run_options(is_dir)

# read in area/temp info
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

if (mfdb_import) {
    # import above to database
    mfdb_import_area(mdb, is_area_data)
    mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])
}

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

if (mfdb_import) {
    # Set up sampling types
    mfdb_import_sampling_type(mdb, 
                              data.frame(id = 1:9, 
                                         name = c("Bio", "Cat", 
                                                  "SprSurvey", "AutSurvey",
                                                  "SprSurveyTotals", "AutSurveyTotals",
                                                  "CommSurvey", "Discard",
                                                  "DiscardSurvey")))
    print('Sampling types imported')
}
# assemble and import cod 
fgName <- 'Cod'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)

# distribute 2 year atlantis age groups to single year classes
source('functions/calcGrowth.R')
#source('functions/calcWtGrowth.R')
source('functions/parseAges.R')
#source('functions/calcCodMort.R')
source('cod/modelCheck/getAtlantisMort3.R')
# add mortality and parse ages based on m
age_count <- 
    left_join(is_fg_count, m_vals) %>%
    parseAges(.) %>%
    arrange(year, month, day, area, depth, age)

# redistribute lengths based on growth params
smooth_len <- 
    age_count %>% 
    filter(count >= 1) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb(linf, k, (t0), age),
                           vb(linf, k, t0, age))) %>%
    select(depth, area, year, month, day, group, cohort, weight, length, 
           maturity_stage, age, count)


# set up length groups and survey parameters
length_group <-  seq(0.5, 200.5, by=1)
#length_group <-  seq(0,max(is_fg_count$length, na.rm=T),by=10)
sigma_per_cohort <- c(cod.length.mn.sd$length.sd)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 1.5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 0 # 8.37e-06

# Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
#is_fg_tracer <- is_fg_count[
#    #is_fg_count$year == attr(is_dir, 'start_year') &
#    is_fg_count$month %in% c(1),]
#is_fg_tracer$species <- fg_group$MfdbCode
#is_fg_tracer$areacell <- is_fg_tracer$area
#is_fg_tracer$sampling_type <- 'Bio'
#mfdb_import_survey(mdb, is_fg_tracer, data_source = paste0('atlantis_tracer_', fg_group$Name))
#print('Tracer data imported')
#
# create survey from tracer values
# keep spr survey as 4 because of gadget order of operations
# keep aut survey as 9 because of recruitment jumps in atlantis
is_fg_survey <- smooth_len[
    smooth_len$area %in% paste('Box', 0:52, sep='') &
        smooth_len$month %in% c(4,9),] %>%
		mutate(sampling_type = ifelse(month == 4,
                                  "SprSurveyTotals",
                                  "AutSurveyTotals")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma) 

survey <- filter(is_fg_survey, count >= 1)
survey$species <- fg_group$MfdbCode
survey$areacell <- survey$area

if (mfdb_import) {
    mfdb_import_survey(mdb, 
                       survey, 
                       data_source = paste0('atlantis_survey_totals_', fg_group$Name))
    print('Survey index data imported')
}

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
al_survey <- stripAgeLength(survey, 0.7, 0.1)
al_survey$length <- round(al_survey$length)
al_survey$weight <- round(al_survey$weight)

# Throw away empty rows
al_survey <- filter(al_survey, count >= 1)
al_survey$species <- fg_group$MfdbCode
al_survey$areacell <- al_survey$area
al_survey <- mutate(al_survey, 
                    sampling_type = ifelse(month == 4, 
                                           'SprSurvey',
                                           'AutSurvey'))
if (mfdb_import) {
    mfdb_import_survey(mdb, al_survey, data_source = paste0('atlantis_survey_', fg_group$Name))
    print('Survey age-length data imported')
}

##############################
# turning to importing catches
##############################

## don't want to do stomach contents yet
# # Fetch consumption and tracer indexes for functional group
# consumption <- atlantis_fg_tracer(
#     is_dir,
#     is_area_data,
#     fg_group = fg_group,
#     consumption = TRUE)
# 
# # Only survey the first quarter, and in 3 boxes
# consumption <- consumption[consumption$month == 1 & consumption$area %in% c("Box20", "Box21", "Box22"),]
# # Assume we only catch 0.0001% of possible available
# consumption$count <- round(consumption$count * 0.000001)
# 
# # Convert this into the 2 data.frames import_stomach requires
# stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FCD = 'COD'
# ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]]
# ))
# mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Cod"))
# 
# stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FHA = 'HAD'
# ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]],
#     PWN = mfdb_find_species('Pandalus borealis')['name',][[1]],
#     ZL = mfdb_find_species('euphausia')['name',][[1]]
# ))
# mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Haddock"))


is_fisheries <- atlantis_fisheries(is_dir)

if (mfdb_import) {
    mfdb_import_vessel_taxonomy(mdb, data.frame(
        id = is_fisheries$Index,
        name = is_fisheries$Code,
        full_name = is_fisheries$Name,
        stringsAsFactors = FALSE))
}

fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data - note that this returns values in kg, not tons
age_catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    mutate(area = as.character(area)) %>%
    rename(group = functional_group)
wl <- getStructN(is_dir, is_area_data, fg_group)

age_catch_wl <- left_join(age_catch, wl)

# parse the catch age-length data to single year classes
age_catch_wl <- left_join(age_catch_wl, m.func.vals)
parsed_age_catch_wl <- 
    parseCatchAges(age_catch_wl) %>% 
    arrange(year, area, month, age)

smooth_len_catch <- 
    parsed_age_catch_wl %>%
    filter(count >= 1) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb(linf, k, (t0), age),
                           vb(linf, k, t0, age))) %>%
    select(area, year, month, fishery, group, cohort, weight, length, 
           age, count)

# see codSampleNumber.R - line 61 to EOF
fleet_suitability <- rep(0.02, length(length_group))
fleet_sigma <- 5.7e-07

# trying to avoid adding error in lengths here as well

comm_catch_samples <- 
    smooth_len_catch %>%
    filter(month %in% c(2,5,8,10)) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet_suitability, 0) %>%
    filter(count >= 1)

# strip age data out
comm_al_samples <- stripFleetAges(comm_catch_samples, 0.05)
comm_al_samples$species <- "COD"
comm_al_samples$sampling_type <- 'CommSurvey'
comm_al_samples$gear <- "BMT"
comm_al_samples <- rename(comm_al_samples, areacell = area, vessel = fishery)
comm_al_samples <- filter(comm_al_samples, count >= 1)

if (mfdb_import) {
    mfdb_import_survey(mdb,
                       comm_al_samples,
                       data_source=paste0("atlantisFishery_", fisheryCode, "_commSamples"))
    print('Commercial survey data imported')
}

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == 'FCD')
is_catch$weight_total <- is_catch$weight_total*1000


# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight_total > 0)
is_catch$gear <- 'BMT'

if (mfdb_import) {
    mfdb_import_survey(mdb, 
                       is_catch, 
                       data_source = paste0("atlantisFishery_", fisheryCode))
    print('Total landings imported')
}

# ##############################
# # get discards data
# ##############################
# 
# codDiscards <- getCodDiscards(is_dir, is_area_data, fishery)
# codDiscards <- mutate(codDiscards, weight_total = weight_total * 1000)
# codDiscards <- filter(codDiscards, functional_group == 'FCD')
# 
# # Species column that maps to MFDB code
# codDiscards$species <- codDiscards$functional_group
# levels(codDiscards$species) <- is_functional_groups[match(
#     levels(codDiscards$functional_group),
#     is_functional_groups$GroupCode), 'Mfdif (mfdb_import) {bCode']
# 
# codDiscards$sampling_type <- "Discard"
# codDiscards <- rename(codDiscards, areacell = area, vessel = fishery, weight = weight_total)
# codDiscards <- filter(codDiscards, weight > 0)
# codDiscards$gear <- 'BMT'
# 
# if (mfdb_import) {
#   mfdb_import_survey(mdb,
#                     codDiscard,
#                     data_source = paste0("atlantisFishery_", fisheryCode, "_Discard"))
#   print('Discards data imported')
# }