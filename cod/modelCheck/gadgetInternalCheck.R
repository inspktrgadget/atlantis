# script to get total numbers from gadget model and compare with surveys
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(Rgadget)
setwd('~/gadget/models/atlantis/cod/codVersions/codMod50')
fit <- gadget.fit(wgts="WGTS", main.file='WGTS/main.final',
                  fleet.predict = data.frame(fleet = 'comm', ratio=1),
                  mat.par=c(-6.510198, 1.108594),
                  printfile.printatstart = 0,
                  printfile.steps = 'all')

data.st.year <- 1983
total <- 
    fit$out.fit$cod.full %>%
    filter(year >= data.st.year, step == 2) %>%
    mutate(length = as.numeric(as.character(gsub('len', '', length))),
           area = 'all') %>%
    mutate(length.grps = cut(length, breaks=c(0, 19, 37, max(length)))) %>%
    mutate(new.length.grps = factor(length.grps, labels = c(0,18,36))) %>%
    mutate(length = as.numeric(as.character(new.length.grps))) %>%
    group_by(year, step, length) %>%
    summarize(total = sum(number))

age.total <- filter(fit$out.fit$cod.std, year >= data.st.year)
length.total <- 
    filter(fit$out.fit$cod.full, year >= data.st.year) %>%
    mutate(length = as.numeric(as.character(gsub('len','',length))),
           age = 'all',
           area = 'all')
    

# let's check total numbers against survey indices to make sure that is
# giving us what we want
spr.indices <- 
    rbind(fit$out.fit$spr.si.short, 
          fit$out.fit$spr.si.mid, 
          fit$out.fit$spr.si.long) %>%
    mutate(length = as.numeric(as.character(gsub('len','',label)))) %>%
    select(year, step, length, number, intercept, slope, predict) %>%
    left_join(total)
    
ggplot(data=spr.indices, aes(x=log(total), y=log(predict), color=factor(length))) + 
    geom_point() + geom_abline(slope = 1, intercept = -6.907755)
ggplot(data=spr.indices, aes(x=log(number), y=log(predict))) + geom_point() +
    geom_abline(slope = 1, intercept = -6.907755)
ggplot(data=spr.indices, aes(x=year, y=total*0.001)) + geom_point() + 
    geom_line(aes(x=year, y=predict)) + facet_wrap(~length)

# let's check the length distribution for surveys against actual numbers
lengths <- 
    filter(survdist.data, name == 'ldist.spr') %>%
    mutate(age = 'all') %>%
    left_join(length.total)

# plot model numbers against ldist.spr numbers
ggplot(data=lengths, aes(x=number, y=predicted)) + geom_point() + 
    geom_abline(slope = -6.907755*0.7072255, intercept = 0)
# plot ldist.spr predicted values against observed data
ggplot(data=lengths, aes(x=observed, y=predicted)) + geom_point() +
    geom_abline(slope = 1, intercept = 0)

ldist.mod <- lm(predicted ~ number, data=lengths)
