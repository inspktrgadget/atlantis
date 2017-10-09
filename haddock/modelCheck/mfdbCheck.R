## sorry, this is a big hairy file where you were just trying to run some 
## diagnostics on the mfdb database to make sure everything was importing correctly
## by comparing it to atlantis output
library(plyr)
library(dplyr)
library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(Rgadget)

setwd('/home/pfrater/gadget/models/atlantis')

# create a gadget directory and define some defaults to use with queries below
gd <- gadget_directory('haddock/hadModel')
setup.d <- 'haddock/setup'
mdb <- mfdb('Atlantis-Iceland')
stocknames <- 'had'

areas <- read.csv('atlantisInfo/boxInfo.csv', header=T)
#boxes <- filter(areas, boundary == 0)$box_id
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st.year <- 1948
end.year <- 2013
defaults <- list(   
    areacell = mfdb_group("1" = boxes),
    timestep = mfdb_timestep_quarterly,
    year = st.year:end.year,
    species = 'HAD')

maxlength <- 120


#####################################################################################
#####################################################################################
## import atlantis annual biomass
#####################################################################################
#####################################################################################
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

# assemble and import haddock 
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


#####################################################################################
#####################################################################################
## first checking overall biomass
#####################################################################################
#####################################################################################

## importing numbers and biomass from mfdb
mfdb.numbers <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type ='Bio',
    length = mfdb_interval('len', seq(0, 200, by=2)),
    age = mfdb_interval('age', seq(0, 18, by = 2)),
    defaults)))[[1]]

mfdb.biomass <- mfdb_sample_rawdata(mdb, NULL, c(list(
    sampling_type='Bio'),
    defaults))[[1]]

ann.biomass <- 
    filter(mfdb.biomass, number > 0) %>%
    mutate(biomass = number * (weight / 1000)) %>%
    group_by(year) %>%
    summarize(ann.biomass = sum(biomass))


# compare biomass by year in gadget to atlantis
atl.biomass <- 
    read.table('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF/OutBiomIndx.txt', 
               header=T) %>%
    mutate(year = 1948:2013) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    mutate(atl.biomass = FHA*1000)


## plot output to make sure biomasses are accurate
atl.ann.biomass <- left_join(atl.biomass, ann.biomass)

atl.bio.plot <- 
    ggplot(data=atl.ann.biomass, aes(x=year)) + 
    geom_line(aes(y=atl.biomass), color='red') +
    geom_line(aes(y=ann.biomass), color='black')


#####################################################################################
#####################################################################################
## now checking landings to see if they match
#####################################################################################
#####################################################################################
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'long'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- 
    filter(is_catch, functional_group == 'FHA') %>%
    rename(weight = weight_total) %>%
    mutate(weight.kg = weight * 1000)

atl.landings <- 
    is_catch %>%
    group_by(year) %>%
    summarize(landings = sum(weight),
              landings.kg = sum(weight.kg))

# get landings from mfdb
lln.landings <- mfdb_sample_totalweight(mdb, c('age', 'length'),
                                        c(list(
                                            gear = 'LLN',
                                            sampling_type='Cat',
                                            species=defaults$species), defaults))[[1]]
ann.landings <- 
    lln.landings %>%
    group_by(year) %>%
    summarize(landings = sum(total_weight / 1000),
              landings.kg = sum(total_weight))

# get landings from the text file
out.catch <- 
    read.table('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF/OutCatch.txt', 
               header=T) %>%
    mutate(year = 1948:2012) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    rename(landings = FHA) %>%
    mutate(landings.kg = landings * 1000)


## plot the landings side by each
landings.plot <- 
    ggplot(data=atl.landings, aes(x=year, y=landings)) + geom_line(color='red') +
    geom_line(data=ann.landings, aes(x=year, y=landings), color='black') + 
    geom_line(data=out.catch, aes(x=year, y=landings), color='blue')



#####################################################################################
#####################################################################################
## now doing age and length distributions
#####################################################################################
#####################################################################################


### NOTE: THIS TAKES NEARLY 5 MINUTES WITH THE WAY THE LENGTH GROUPS ARE SETUP


### set up the survey sampling from atlantis first
source('haddock/initdb/getHadLengthVar.R') # source haddock length sd at age group
length_group <-  seq(0.5,120.5,1)
sigma_per_cohort <- sqrt(had.length.mn.sd$length.sd)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 8.37e-06

is_fg_survey <- is_fg_count[
    is_fg_count$area %in% paste('Box', 0:52, sep='') &
        is_fg_count$month %in% c(3,9),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma)

survey <- 
    filter(is_fg_survey, count > 0)

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/haddock/initdb/hadSampleNumbers.R
source('functions/stripAgeLength.R')
al.survey <- stripAgeLength(survey, 0.44, 0.023)
al.survey$length <- round(al.survey$length)
al.survey$weight <- round(al.survey$weight)
al.survey <- filter(al.survey, count > 0)

survey.ldist <- 
    filter(al.survey, !is.na(length)) %>% 
    group_by(year, month, length) %>%
    summarize(count = sum(count))

survey.aldist <-
    filter(al.survey, !is.na(age)) %>%
    group_by(year, month, age) %>%
    summarize(count = sum(count))


#########################################
### now do the various ldists and aldists

### spring first

### spring length distributions
spr.ldata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SprSurvey',
    species = defaults$species,
    length = mfdb_interval("len", seq(0, maxlength, by=1))),
    defaults))[[1]]

spr.ldist <-
    spr.ldata %>%
    mutate(length = as.numeric(as.character(gsub('len', '', length))))

spr.ldist.plot <-
    ggplot(data=spr.ldist, aes(x=length, y=number)) + geom_line(size=1.1) +
    geom_line(data=filter(survey.ldist, month == 3), 
              aes(x=length, y=count), color='red', linetype='dashed') +
    facet_wrap(~year)


# spring age distributions
spr.aldata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SprSurvey',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             species=defaults$species,
                             length = mfdb_interval("len", seq(0, maxlength, by = 1))),
                        defaults))[[1]]

spr.aldist <-
    spr.aldata %>%
    mutate(age = as.numeric(as.character(gsub('age', '', age)))) %>%
    group_by(year, age) %>%
    summarize(count = sum(number))

spr.aldist.plot <-
    ggplot(data=spr.aldist, aes(x=age, y=count)) + geom_line(size=1.1) +
    geom_line(data=filter(survey.aldist, month == 3), 
              aes(x=age, y=count), color='red', linetype='dashed') +
    facet_wrap(~year)


### autumn is next

# length distributions  
aut.ldata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'AutSurvey',
    species = defaults$species,
    length = mfdb_interval("len", seq(0, maxlength, by=1))),
    defaults))[[1]]

aut.ldist <-
    aut.ldata %>%
    mutate(length = as.numeric(as.character(gsub('len', '', length))))

aut.ldist.plot <-
    ggplot(data=aut.ldist, aes(x=length, y=number)) + geom_line(size=1.5) +
    geom_line(data=filter(survey.ldist, month == 9), 
              aes(x=length, y=count), color='red', linetype='dashed') +
    facet_wrap(~year)


## autumn age distributions
aut.aldata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'AutSurvey',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             species=defaults$species,
                             length = mfdb_interval("len", seq(0, maxlength, by = 1))),
                        defaults))[[1]]

aut.aldist <-
    aut.aldata %>%
    mutate(age = as.numeric(as.character(gsub('age', '', age)))) %>%
    group_by(year, age) %>%
    summarize(count = sum(number))

aut.aldist.plot <-
    ggplot(data=aut.aldist, aes(x=age, y=count)) + geom_line(size=1.5) +
    geom_line(data=filter(survey.aldist, month == 9), 
              aes(x=age, y=count), color='red', linetype='dashed') +
    facet_wrap(~year)


### commercial catch is last


########################################################
## must import the commercial catch survey samples first
########################################################

## NOTE: THIS TAKES A BIT AS WELL

source('functions/commCatchAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data - note that this returns values in kg, not tons
age.catch <- commCatchAges(is_dir, is_area_data, fg_group, fishery)
wl <- getStructN(is_dir, is_area_data, fg_group)
age.catch.wl <- left_join(age.catch, wl)
# see hadSampleNumber.R - line 61 to EOF
fleet.suitability <- rep(0.001, length(length_group))
fleet.sigma <- 4.3e-07

comm.catch.samples <-
    age.catch.wl %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma)

comm.catch.samples <- filter(comm.catch.samples, count > 0)

# strip age data out
comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
comm.al.samples <- filter(comm.al.samples, count > 0)

comm.survey.ldist <- 
    comm.al.samples %>%
    filter(!is.na(length)) %>%
    mutate(step = ceiling(month / 3)) %>%
    group_by(year, step, length) %>%
    summarize(count = sum(count))

comm.survey.aldist <-
    filter(comm.al.samples, !is.na(age)) %>%
    mutate(step = ceiling(month / 3)) %>%
    group_by(year, step, age) %>%
    summarize(count = sum(count))
    
### now fetch data from mfdb and plot the output

# Query length data to create bmt catchdistribution components
catch.ldata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'CommSurvey',
    species = defaults$species,
    gear = c('LLN'),
    length = mfdb_interval("len", c(0, seq(20, maxlength, by = 1)))),
    defaults))[[1]]

catch.ldist <- 
    catch.ldata %>%
    mutate(length = as.numeric(as.character(gsub('len', '', length))))

catch.ldist.plot <-
    ggplot(data=catch.ldist, aes(x=length, y=number)) + geom_line(size=1.2) +
    geom_line(data=filter(comm.survey.ldist), 
              aes(x=length, y=count), color='red', linetype='dashed') +
    facet_wrap(~year+step) +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())

## if you want to double check to make sure, run the following
# test <- left_join(mutate(catch.ldist, step = as.numeric(step)), comm.survey.ldist)
# plot(number ~ count, test)
# abline(0,1)

## Age bottom.trawl fleet
catch.aldata<-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'CommSurvey',
                             gear = 'LLN',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             length = mfdb_interval("len", c(0, seq(20, maxlength, by=1)))),
                        defaults))[[1]]

catch.aldist <-
    catch.aldata %>%
    mutate(age = as.numeric(as.character(gsub('age', '', age)))) %>%
    group_by(year, step, age) %>%
    summarize(count = sum(number))

catch.aldist.plot <-
    ggplot(data=catch.aldist, aes(x=age, y=count)) + geom_line(size=1.2) +
    geom_line(data=filter(comm.survey.aldist), 
              aes(x=age, y=count), color='red', linetype='dashed') +
    facet_wrap(~year+step) +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())


#####################################################################################
#####################################################################################
## now checking survey numbers
#####################################################################################
#####################################################################################

survey.index <- 
    survey %>% 
    group_by(year, month) %>%
    summarize(count = sum(count))

spr.survey <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurvey'),
    defaults))[[1]]

aut.survey <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurvey'),
    defaults))[[1]]

spr.survey.plot <-
    ggplot(data=spr.survey, aes(x=year, y=number)) + geom_line(size=1.2) +
    geom_line(data=filter(survey.index, month==3), aes(x=year, y=count),
              color='red', linetype='dashed')

aut.survey.plot <-
    ggplot(data=aut.survey, aes(x=year, y=number)) + geom_line(size=1.2) +
    geom_line(data=filter(survey.index, month==9), aes(x=year, y=count),
              color='red', linetype='dashed')


### now checking by length breakdown

survey.index.len <- 
    al.survey %>%
    filter(!is.na(length)) %>%
    mutate(length = paste0('len', (length %/% 10)*10)) %>%
    group_by(year, month, length) %>%
    summarize(count = sum(count))

spr.survey.len <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurvey',
    length = mfdb_interval("len", seq(0,maxlength, by=10))),
    defaults))[[1]]

spr.survey.len.plot <- 
    ggplot(data=spr.survey.len, aes(x=year, y=number)) + geom_line(size=1.2) +
    geom_line(data=filter(survey.index.len, month == 3), 
              aes(x=year, y=count), color='red', linetype='dashed') +
    facet_wrap(~length)


aut.survey.len <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurvey',
    length = mfdb_interval("len", seq(0,maxlength, by=10))),
    defaults))[[1]]


aut.survey.len.plot <- 
    ggplot(data=spr.survey.len, aes(x=year, y=number)) + geom_line(size=1.2) +
    geom_line(data=filter(survey.index.len, month == 9), 
              aes(x=year, y=count), color='red', linetype='dashed') +
    facet_wrap(~length)




#####################################################################################
#####################################################################################
### let's check some of the other setup files (i.e. fleet, model setup stuff, etc)
### it makes sense to do this now because everything is up already
#####################################################################################
#####################################################################################