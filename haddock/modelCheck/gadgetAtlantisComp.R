library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(Rgadget)
setwd('~/gadget/models/atlantis/haddock/hadModel')
fit <- gadget.fit(wgts="WGTS", main.file='WGTS/main.final',
                  fleet.predict = data.frame(fleet = 'lln.comm', ratio=1))


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

# assemble and import haddock 
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


# compare biomass by year in gadget to atlantis
atl.biomass <- 
    read.table('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF/OutBiomIndx.txt', 
                          header=T) %>%
    mutate(year = 1948:2013) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    mutate(atl.biomass = FHA*1000)

# plot gadget biomass against atlantis
atl.gad.biomass <- left_join(fit$res.by.year, atl.biomass)

atl.gad.plot <- 
    ggplot(data=atl.gad.biomass, aes(x=atl.biomass, y=total.biomass)) + geom_point() +
    geom_abline(intercept=0, slope=1) + theme_bw() + 
    xlab('Atlantis Annual Biomass') + ylab('Gadget Annual Biomass')

atl.gad.ts <-
    ggplot(data=atl.gad.biomass, aes(x=year)) + 
    geom_line(aes(y=total.biomass, color='Gadget')) +
    geom_line(aes(y=atl.biomass, color='Atlantis')) + 
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Biomass')



#######################################
## to check numbers instead of biomass
#######################################
had.numbers <- 
    is_fg_count %>% 
    filter(month == 2) %>%
    group_by(year) %>% 
    summarize(atl.number = sum(count))
atl.gad.numbers <- left_join(fit$res.by.year, had.numbers)

had.numbers.plot <- 
    ggplot(data=atl.gad.numbers, aes(x=year)) + 
    geom_line(aes(y=total.number, color='Gadget')) +
    geom_line(aes(y=atl.number, color='Atlantis')) +
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Numbers')


#######################################
## check landings
#######################################
atl.landings <- 
    read.table('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF/OutCatch.txt', 
               header=T) %>%
    mutate(year = 1948:2012) %>%
    select(year, starts_with(fg_group$GroupCode))

atl.catch.plot <- catch.plot + geom_line(data=atl.landings, aes(x=year, y=FHA))


#######################################
## check numbers by age
#######################################
age.numbers <- 
    fit$stock.std %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age) %>%
    summarize(total = sum(number))
    
atl.age.numbers <- 
    is_fg_count %>%
    filter(month == 3, count >= 1) %>%
    group_by(year, age) %>%
    summarize(total = sum(count))

age.numbers.plot <-
    ggplot(data=age.numbers, aes(x=year, y=total, color='Gadget')) + geom_line() +
    geom_line(data=atl.age.numbers, aes(x=year, y=total, color='Atlantis')) + 
    facet_wrap(~age, scales='free_y') + 
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Numbers')

#######################################
## check growth
#######################################
had.growth <- 
    is_fg_count %>%
    filter(count > 0) %>%
    sample_n(10000)

gr.check <- 
    ggplot(data=had.growth, aes(x=age, y=length)) + geom_point() +
    geom_line(data=fit$stock.growth, aes(x=age, y=length)) +
    theme_bw()






# comparing initial values from atlantis to gadget initial values
atl.init <- 
    is_fg_count %>% 
    filter(year == 1948, month == 1) %>%
    group_by(age) %>%
    summarize(init.ind = sum(count))

params <- read.gadget.parameters('haddock/hadModel/WGTS/params.final')
age.alpha <- params['had.age.alpha', ]$value
age.beta <- params['had.age.beta', ]$value
age.gamma <- params['had.age.gamma', ]$value
age.delta <- params['had.age.delta', ]$value
mult <- params[grep('had.mult', rownames(params), value=T), ]$value
init.abund <- params[grep('init.abund', rownames(params), value=T), ]$value
source('~/gadget/models/functions/andersenAge.R')

gad.init.ages <- 10e03 * age.params * (mult * init.abund)
gad.init <- data.frame(age = 1:19, init = gad.init.ages)
gad.init <- 
    gad.init %>% 
    mutate(age.group = ifelse(age %% 2 == 0, age, age-1)) %>% 
    group_by(age.group) %>% 
    summarize(init=sum(init))

init.comp <- 
    ggplot(data=gad.init, aes(x=age.group, y=init)) + geom_line() +
    geom_line(data=atl.init, aes(x=age, y=init.ind))


## playing around with biomass in different months to see if that makes a difference
monthly.biomass <-
    is_fg_count %>%
    mutate(biomass = weight * count / 1e3) %>%
    filter(!is.na(biomass)) %>% 
    group_by(year, month) %>% 
    summarize(biomass = sum(biomass))

monthly.gad.biomass <- 
    select(atl.gad.biomass, year, total.biomass) %>%
    rename(gadget.biomass = total.biomass) %>% 
    mutate(month = 3)

monthly.biomass <- left_join(monthly.biomass, monthly.gad.biomass)

bm.by.month <-
    ggplot(data=monthly.biomass, aes(x=year, y=biomass/1e3,
                                     color=factor(month))) + 
    geom_line() + 
    geom_line(aes(x=year, y=gadget.biomass/1e3)) +
    theme_bw() + xlab('Year') + ylab('Biomass (tons)') 

