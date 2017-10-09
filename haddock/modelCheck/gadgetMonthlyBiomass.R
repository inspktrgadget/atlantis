## testing out to see if biomass is different at different timesteps in gadget
## or, rather, how much it differs
library(dplyr)
library(ggplot2)

setwd('~/gadget/models/atlantis/haddock/hadModel/WGTS/out.fit')
had.std.end <- read.table('had.std', comment.char = ';')
names(had.std.end) <- c('year', 'step', 'area', 'age', 'number', 'mn.length', 
                    'mn.biomass', 'stddev.length', 'number.consumed', 'biomass.consumed')

had.biomass.end <- 
    had.std.end %>%
    mutate(biomass = number * mn.biomass) %>%
    group_by(year, step) %>%
    summarize(total.biomass = sum(biomass)) %>%
    mutate(month = rep(c(3,6,9,12), length(unique(year)))) %>%
    select(-step)


############################################################################
## be sure to go and change the printfile.fit and rerun gadget from terminal
## using gadget -s -main WGTS/main.print -i WGTS/params.final
############################################################################

had.std.start <- read.table('had.std', comment.char = ';')
names(had.std.start) <- c('year', 'step', 'area', 'age', 'number', 'mn.length', 
                        'mn.biomass', 'stddev.length', 'number.consumed', 'biomass.consumed')

had.biomass.start <- 
    had.std.start %>%
    mutate(biomass = number * mn.biomass) %>%
    group_by(year, step) %>%
    summarize(total.biomass = sum(biomass)) %>%
    mutate(month = rep(c(1,4,7,10), length(unique(year)))) %>%
    select(-step)


had.biomass <- 
    rbind(had.biomass.start, had.biomass.end) %>%
    arrange(year, month)

step.biomass.plot <- 
    ggplot(data=had.biomass, aes(x=year, y=total.biomass, color=factor(month))) + 
    geom_line() + theme_bw()

