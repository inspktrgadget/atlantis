library(fjolst)
library(fjolstTranslate)
library(tidyverse)

# first import standardized trawl survey data --------------------------

## get stations
igfs_stations <- 
    select(translate.stodvar(), sample.id, year, sampling.type) %>%
    filter(sampling.type %in% c(30, 35))

## get length data
igfs_ldist <- 
    translate.all.le() %>%
    filter(species.code == 1, sample.id %in% igfs_stations$sample.id) %>%
    left_join(igfs_stations) %>%
    group_by(year, sampling.type, length) %>%
    summarize(count = sum(count))

# get age distributions by year
igfs_aldist <- 
    translate.all.kv() %>%
    filter(species.code == 1, sample.id %in% igfs_stations$sample.id) %>%
    left_join(igfs_stations) %>%
    group_by(year, sampling.type, age) %>%
    summarize(count = n())

# compute overall lengths at age
igfs_aldist <- 
    igfs_aldist_by_year %>% group_by(age, length) %>%
    summarize(total = n())

# next import commercial catch surveys ---------------------------------------

# get commercial catch sample station ids
catch_stations <- 
    select(translate.stodvar(), sample.id, year, sampling.type) %>%
    filter(sampling.type %in% c(1,8))

## get length data
catch_ldist <- 
    translate.all.le() %>%
    filter(species.code == 1, sample.id %in% catch_stations$sample.id) %>%
    left_join(catch_stations) %>%
    group_by(year, length) %>%
    summarize(count = sum(count))

# get age distributions by year
catch_aldist <- 
    translate.all.kv() %>%
    filter(species.code == 1, sample.id %in% catch_stations$sample.id) %>%
    left_join(catch_stations) %>%
    group_by(year, age) %>%
    summarize(count = n())


# plot the output
ice_ldist_plot <- 
    ggplot(data=igfs_ldist, aes(x=length, y=count)) + 
    geom_line(aes(color = factor(sampling.type))) + facet_wrap(~year, scales = "free_y") + 
    geom_line(data=catch_ldist, aes(x=length, y=count))

ice_aldist_plot <- 
    ggplot(data=igfs_aldist, aes(x=age, y=count)) + 
    geom_line(aes(color = factor(sampling.type))) + facet_wrap(~year) + 
    geom_line(data=catch_aldist, aes(x=age, y=count))