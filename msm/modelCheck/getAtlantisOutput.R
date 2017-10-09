
# devtools::install_github("mareframe/mfdb", ref="5.x")
# devtools::install_github("mareframe/mfdbatlantis")
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
source('functions/commCatchAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('cod/initdb/getCodLengthVar.R') # source cod length sd at age group
source('haddock/initdb/getHadLengthVar.R') # source cod length sd at age group

# read in data
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


# Ingest survey data
for (fgName in c("Cod", "Haddock")) {
    fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
    cat("Importing functional group", fg_group$Name, "\n")
    
    if (fg_group$Name == "Cod") {
        length_group <-  c(0, seq(15, 155, by = 10), 205)
        sigma_per_cohort <- cod.length.mn.sd$length.sd
        sel_lsm <- 49
        sel_b <- 0.046 # Controls the shape of the curve
        survey_suitability <- 1.5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
        survey_sigma <- 8.37e-06
    } else if (fg_group$Name == "Haddock") {
        length_group <-  c(0, seq(10, 100, by = 10), 120)
        sigma_per_cohort <- had.length.mn.sd$length.sd
        sel_lsm <- 49
        sel_b <- 0.046 # Controls the shape of the curve
        survey_suitability <- 5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
        survey_sigma <- 8.37e-06
    } else {
        stop("Unknown group")
    }
    
    # uses structN + resN as weight instead of structN*3.65
    is_fg_count <- pauls_atlantis_tracer(is_dir, is_area_data, fg_group)
    
    # Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
    is_fg_tracer <- is_fg_count[
        is_fg_count$year == attr(is_dir, 'start_year') &
            is_fg_count$month %in% c(1),]
    is_fg_tracer$species <- fg_group$MfdbCode
    is_fg_tracer$areacell <- is_fg_tracer$area
    is_fg_tracer$sampling_type <- 'Bio'
    
    # create survey from atlantis data
    is_fg_survey <- is_fg_count[
        is_fg_count$area %in% paste('Box', 0:52, sep='') &
            is_fg_count$month %in% c(3,10),] %>%
        mutate(sampling_type = ifelse(month == 3,
                                      "SprSurvey",
                                      "AutSurvey")) %>%
        atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
        atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma)
    
    survey <- filter(is_fg_survey, count > 0)
    
    # strip ages and lengths from survey to mimic real world data
    # see '~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
    al.survey <- stripAgeLength(survey, 0.44, 0.023)
    
    # Throw away empty rows
    is_fg_survey <- al.survey[al.survey$count > 0,]
    
    is_fg_survey$species <- fg_group$MfdbCode
    is_fg_survey$areacell <- is_fg_survey$area
    
}

####################################
# not using stomach content data yet
####################################

# for (fgName in c("Cod", "Haddock")) {
#   fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
#   cat("Importing consumption by functional group", fg_group$Name, "\n")
#   
#   # Fetch consumption and tracer indexes for functional group
#   consumption <- atlantis_fg_tracer(
#     is_dir,
#     is_area_data,
#     fg_group = fg_group,
#     consumption = TRUE)
#   
#   # Only survey the first quarter, and in 3 boxes
#   consumption <- consumption[consumption$month == 1 & consumption$area %in% c("Box20", "Box21", "Box22"),]
#   # Assume we only catch 0.0001% of possible available
#   consumption$count <- round(consumption$count * 0.000001)
#   # TODO: Sample every nth fish (that the survey takes?)
#   #   => Don't bother with this, only generating proportions anyway
#   
#   # Convert this into the 2 data.frames import_stomach requires
#   stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FCD = 'COD'
#   ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]]
#   ))
#   mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Cod"))
#   
#   stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FHA = 'HAD'
#   ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]],
#     PWN = mfdb_find_species('Pandalus borealis')['name',][[1]],
#     ZL = mfdb_find_species('euphausia')['name',][[1]]
#   ))
#   mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Haddock"))
# } 

is_fisheries <- atlantis_fisheries(is_dir)

for (fisheryCode in is_fisheries$Code) {
    fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]
    cat("Importing fishery", fisheryCode, "\n")
    
    # to set up as age structured data - note that this returns values in kg, not tons
    age.catch <- commCatchAges(is_dir, is_area_data, fg_group, fishery)
    wl <- getStructN(is_dir, is_area_data, fg_group)
    
    age.catch.wl <- left_join(age.catch, wl)
    
    # see haddockSampleNumber.R - line 61 to EOF
    fleet.suitability <- rep(0.001, length(length_group))
    fleet.sigma <- 4.3e-07
    
    comm.catch.samples <- 
        age.catch.wl %>%
        atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
        atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma) %>%
        filter(count > 0)
    
    # strip age data out
    comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
    comm.al.samples$species <- "HAD"
    comm.al.samples$sampling_type <- 'CommSurvey'
    comm.al.samples$gear <- "BMT"
    comm.al.samples <- rename(comm.al.samples, areacell = area, vessel = fishery)
    comm.al.samples <- filter(comm.al.samples, count > 0)

    is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
    if (nrow(is_catch) == 0) next
    
    # Species column that maps to MFDB code
    is_catch$species <- is_catch$functional_group
    levels(is_catch$species) <- is_functional_groups[match(
        levels(is_catch$functional_group),
        is_functional_groups$GroupCode), 'MfdbCode']
    
 }
