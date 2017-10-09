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
source('functions/getCodDiscards.R')
source('functions/commCatchAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('cod/initdb/getCodLengthVar.R') # source cod length sd at age group

mfdb('Atlantis-Iceland', destroy_schema = TRUE)
mdb <- mfdb('Atlantis-Iceland')

is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF')
is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)
mfdb_import_area(mdb, is_area_data)
mfdb_import_temperature(mdb, is_temp[is_temp$depth == 1,])

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# Set up sampling types
mfdb_import_sampling_type(mdb, 
                          data.frame(id = 1:8, 
                                     name = c("Bio", "Cat", 
                                              "SprSurvey", "AutSurvey",
											  "SprSurveyTotals", "AutSurveyTotals",
                                              "CommSurvey", "Discard")))


# assemble and import cod 
fgName <- 'Cod'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)

length_group <-  seq(0.5, 200.5, by=1)
sigma_per_cohort <- sqrt(cod.length.mn.sd$length.sd)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 1.5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 8.37e-06

# Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
is_fg_tracer <- is_fg_count[
    #is_fg_count$year == attr(is_dir, 'start_year') &
        is_fg_count$month %in% c(1),]
is_fg_tracer$species <- fg_group$MfdbCode
is_fg_tracer$areacell <- is_fg_tracer$area
is_fg_tracer$sampling_type <- 'Bio'
mfdb_import_survey(mdb, is_fg_tracer, data_source = paste0('atlantis_tracer_', fg_group$Name))

# create survey from tracer values
is_fg_survey <- is_fg_count[
    is_fg_count$area %in% paste('Box', 0:52, sep='') &
        is_fg_count$month %in% c(3,9),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma)

survey <- filter(is_fg_survey, count > 0)

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
al.survey <- stripAgeLength(survey, 0.7072256, 0.07072157)
al.survey$length <- round(al.survey$length)
al.survey$weight <- round(al.survey$weight)

# Throw away empty rows
al.survey <- al.survey[al.survey$count > 0,]

al.survey$species <- fg_group$MfdbCode
al.survey$areacell <- al.survey$area

# adding in survey totals with all lengths and ages
survey.total <-
	survey %>%
	mutate(sampling_type = ifelse(month == 3, "SprSurveyTotals", "AutSurveyTotals"),
		   species = fg_group$MfdbCode) %>%
	rename(areacell = area)

mfdb_import_survey(mdb, survey.total, data_source = paste0('atlantis_total_survey_', fg_group$Name))
mfdb_import_survey(mdb, al.survey, data_source = paste0('atlantis_survey_', fg_group$Name))


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
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = is_fisheries$Index,
    name = is_fisheries$Code,
    full_name = is_fisheries$Name,
    stringsAsFactors = FALSE))

fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data - note that this returns values in kg, not tons
age.catch <- commCatchAges(is_dir, is_area_data, fg_group, fishery)
wl <- getStructN(is_dir, is_area_data, fg_group)

age.catch.wl <- left_join(age.catch, wl)

# see codSampleNumber.R - line 61 to EOF
fleet.suitability <- rep(0.001444, length(length_group))
fleet.sigma <- 5.7e-07

# trying to avoid adding error in lengths here as well

comm.catch.samples <-
    age.catch.wl %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma)

comm.catch.samples <- filter(comm.catch.samples, count > 0)

# comm.catch.samples <-
#     age.catch.wl %>%
#     mutate(count = round(count * 0.001444),
#            length = round(length),
#            weight = round(weight)) %>%
#     filter(count > 0)

# strip age data out
comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
comm.al.samples$species <- "COD"
comm.al.samples$sampling_type <- 'CommSurvey'
comm.al.samples$gear <- "BMT"
comm.al.samples <- rename(comm.al.samples, areacell = area, vessel = fishery)
comm.al.samples <- filter(comm.al.samples, count > 0)

mfdb_import_survey(mdb,
                   comm.al.samples,
                   data_source=paste0("atlantisFishery_", fisheryCode, "_commSamples"))

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == 'FCD')
is_catch$weight_total <- is_catch$weight_total*1000


########################################
## the following code is to correct
## the spikes that occur every 7-8 years
########################################
weird.yrs <- data.frame(year = sort(c(seq(1951,2011,15), seq(1959, 2004, 15))),
                        months = c(10,4,10,4,10,4,10,4,10))
annual.wt <-
    is_catch %>% 
    filter(!(month %in% c(4,10))) %>%
    group_by(year, month) %>%
    summarize(wt = sum(weight_total)) %>%
    group_by(year) %>% summarize(mn.ann.wt = mean(wt))
overcatch.rate <-
    is_catch %>%
    group_by(year, month) %>%
    summarize(monthly.wt = sum(weight_total)) %>%
    left_join(annual.wt) %>%
    mutate(oc.rate = monthly.wt / mn.ann.wt) %>%
    filter(oc.rate > 1.6) %>% select(year, month, oc.rate)
is_catch <-
    is_catch %>%
    left_join(overcatch.rate) %>%
    mutate(weight = ifelse(is.na(oc.rate), weight_total, weight_total / oc.rate)) %>%
    select(-weight_total, -oc.rate)


# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight > 0)
is_catch$gear <- 'BMT'

mfdb_import_survey(mdb, 
                   is_catch, 
                   data_source = paste0("atlantisFishery_", fisheryCode))

##############################
# get discards data
##############################

codDiscards <- getCodDiscards(is_dir, is_area_data, fishery)
codDiscards <- mutate(codDiscards, weight_total = weight_total * 1000)
codDiscards <- filter(codDiscards, functional_group == 'FCD')

# Species column that maps to MFDB code
codDiscards$species <- codDiscards$functional_group
levels(codDiscards$species) <- is_functional_groups[match(
    levels(codDiscards$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

codDiscards$sampling_type <- "Discard"
codDiscards <- rename(codDiscards, areacell = area, vessel = fishery, weight = weight_total)
codDiscards <- filter(codDiscards, weight > 0)
codDiscards$gear <- 'BMT'

mfdb_import_survey(mdb, 
                   codDiscards, 
                   data_source = paste0("atlantisFishery_", fisheryCode, "_Discard"))
