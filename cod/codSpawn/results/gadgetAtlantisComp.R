library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
setwd('~/gadget/models/atlantis')
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


# compare biomass by year in gadget to atlantis
atl.biomass <- 
    read.table('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutBiomIndx.txt', 
                          header=T) %>%
    mutate(year = 1948:2013) %>%
    select(year, starts_with(fg_group$GroupCode)) %>%
    mutate(atl.biomass = FCD*1000)
    

# plot gadget biomass against atlantis
gad.biomass <- 
    fit$stock.std %>%
    filter(step == 1) %>%
    mutate(total.biomass = mean.weight * number) %>%
    group_by(year) %>%
    summarize(total.biomass = sum(total.biomass))

atl.gad.biomass <- 
    left_join(gad.biomass, atl.biomass) %>%
    mutate(scale.diff = total.biomass / atl.biomass)

atl.gad.plot <- 
    ggplot(data=atl.gad.biomass, aes(x=atl.biomass, y=total.biomass)) + geom_point() +
    geom_abline(intercept=0, slope=1) + theme_bw() + 
    xlab('Atlantis Annual Biomass') + ylab('Gadget Annual Biomass')

biomass.comp.plot <-
    ggplot(data=atl.gad.biomass, aes(x=year)) + 
    geom_line(aes(y=total.biomass/1e6, color='Gadget')) +
    geom_line(aes(y=atl.biomass/1e6, color='Atlantis')) + 
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Biomass (thousand tons)') + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

bm.scale.diff.plot <- 
    ggplot(data=atl.gad.biomass, aes(x=year, y=scale.diff)) + 
    geom_line() + geom_hline(yintercept = 1, linetype='dashed') + 
    ylim(0,pmax(1.5, max(atl.gad.biomass$scale.diff, na.rm=T))) +
    theme_bw() + xlab('Year') + ylab('Relative difference in biomass') + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

#######################################
## to check numbers instead of biomass
#######################################
cod.numbers <- 
    is_fg_count %>% 
    filter(month == 9) %>%
    group_by(year) %>% 
    summarize(atl.number = sum(count))
gad.numbers <- 
    fit$stock.std %>%
    filter(step == 3) %>%
    group_by(year) %>%
    summarize(total.number = sum(number))
atl.gad.numbers <- 
    left_join(gad.numbers, cod.numbers) %>%
    mutate(scale.diff = total.number / atl.number)

numbers.comp.plot <- 
    ggplot(data=filter(atl.gad.numbers, year < 2013), aes(x=year)) + 
    geom_line(aes(y=total.number/1e6, color='Gadget')) +
    geom_line(aes(y=atl.number/1e6, color='Atlantis')) +
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Numbers (millions of fish)') + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

nmb.scale.diff.plot <- 
    ggplot(data=atl.gad.numbers, aes(x=year, y=scale.diff)) + geom_line() +
    geom_hline(yintercept = 1, linetype='dashed') +
    ylim(0,pmax(1.5, max(atl.gad.numbers$scale.diff, na.rm=T))) +
    theme_bw() + xlab('Year') + ylab('Relative difference in numbers') + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
## check landings
#######################################
atl.landings <- 
    read.table('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutCatch.txt', 
               header=T) %>%
    mutate(year = 1948:2012) %>%
    select(year, starts_with(fg_group$GroupCode))

atl.catch.plot <- catch.plot + geom_line(data=atl.landings, aes(x=year, y=FCD))


#######################################
## check numbers by age
#######################################
gad.age.numbers <- 
    fit$stock.std %>%
    filter(step == 2) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age) %>%
    summarize(gad.number = sum(number))

atl.age.numbers <- 
    is_fg_count %>%
    filter(month == 3, count >= 1) %>%
    group_by(year, age) %>%
    summarize(atl.number = sum(count))

atl.gad.age.numbers <- left_join(gad.age.numbers, atl.age.numbers)

age.numbers.plot <-
    ggplot(data=filter(atl.gad.age.numbers), 
           aes(x=year, y=gad.number/1e6, color='Gadget')) + geom_line() + 
    geom_line(aes(x=year, y=atl.number/1e6, color='Atlantis')) + 
    facet_wrap(~age, scales='free_y') + 
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), 
                       values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Numbers (millions of fish)') + 
    theme(axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

# ----------------------------------------------------------
# age numbers by parsed out by step
# Note: this only works when all timesteps are printed by gadget
gad.step.age.numbers <- 
    fit$stock.std %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age, step) %>%
    summarize(gad.number = sum(number))

monthToStep <- sort(rep(1:4,3))
atl.step.age.numbers <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    mutate(step = monthToStep[month]) %>%
    filter(month %in% c(1,4,7,10)) %>%
    group_by(year, age, step) %>%
    summarize(atl.number = sum(count))

atl.gad.step.age.numbers <- 
    left_join(gad.step.age.numbers, atl.step.age.numbers)

step.age.numbers.plot <-
    ggplot(data=atl.gad.step.age.numbers, 
           aes(x=year, y=gad.number/1e6, color ='Gadget', 
               linetype=factor(step))) + 
    geom_line() + 
    geom_line(data=atl.gad.step.age.numbers,
              aes(x=year, y=atl.number/1e6, color='Atlantis', 
                  linetype=factor(step))) + 
    facet_wrap(~age, scales='free_y') + 
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), 
                       values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Numbers (millions of fish)') + 
    theme(axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#-----------------------------------------------------------
# visualize the contribution of each age group to biomass
gad.age.biomass <- 
    fit$stock.std %>%
    filter(step == 3) %>%
    mutate(age = age - (age %% 2),
           biomass = mean.weight * number) %>%
    group_by(year, age) %>%
    summarize(total.biomass = sum(biomass))

atl.age.biomass <- 
    is_fg_count %>%
    filter(month == 9, count >= 1) %>%
    mutate(biomass = (count * weight)/1e3) %>%
    group_by(year, age) %>%
    summarize(atl.biomass = sum(biomass))

atl.gad.age.biomass <- 
    left_join(gad.age.biomass, atl.age.biomass) %>%
    mutate(diff = total.biomass - atl.biomass,
           scale.diff = total.biomass / atl.biomass)

age.biomass.comp.plot <-
    ggplot(data=atl.gad.age.biomass, aes(x=year)) + 
    geom_line(aes(y=total.biomass/1e3, color='Gadget')) +
    geom_line(aes(y=atl.biomass/1e3, color='Atlantis')) + 
    facet_wrap(~age, scales = "free_y") +
    scale_color_manual('', breaks=c('Gadget', 'Atlantis'), values=c('red', 'black')) +
    theme_bw() + xlab('Year') + ylab('Biomass (tons)') + 
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

diff.by.age <- 
    atl.gad.age.biomass %>%
    filter(year >= gadget_st_year) %>%
    group_by(age) %>%
    summarize(mn.diff = mean(diff/1e3),
              se.diff = sd(diff/1e3) / sqrt(n()))

diff.by.age.plot <- 
    ggplot(data=diff.by.age, aes(x=age, y=mn.diff)) + geom_point(size = 4) +
    geom_errorbar(aes(ymin = mn.diff - se.diff, 
                      ymax = mn.diff + se.diff), width = 0) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme_bw() + xlab('Age') + ylab('Difference in Biomass (tons)') + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
## check growth
#######################################
cod.growth <- 
    is_fg_count %>%
    filter(count > 0) %>%
    sample_n(10000)

gr.check <- 
    ggplot(data=cod.growth, aes(x=age, y=length)) + 
    geom_point(size = 3, shape=1) +
    geom_line(data=fit$stock.growth, aes(x=age, y=length)) +
    theme_bw() + xlab("Age") + ylab("Length (cm)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
# compare gadget initial values to 
# atlantis initial values
#######################################
gad.init.year <- min(fit$stock.std$year)
atl.init <- 
    is_fg_count %>%
    filter(year == gad.init.year,
           month == 3,
           count >= 1) %>%
    group_by(age) %>%
    summarize(atl.init = sum(count))

gad.init <- 
    fit$stock.std %>%
    filter(year == min(year)) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(age) %>% 
    summarize(gad.init = sum(number))

gad.atl.init.plot <- 
    ggplot(data=gad.init, aes(x=age, y=gad.init)) + 
    geom_bar(stat='identity') + 
    geom_line(data=atl.init, aes(x=age, y=atl.init))