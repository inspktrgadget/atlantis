library(dplyr)
library(tidyr)
library(ggplot2)
library(fjolst)
library(fjolstTranslate)


## doing spring surveys first
stations <- 
    select(translate.stodvar(), sample.id, year, sampling.type) %>%
    filter(sampling.type %in% c(30, 35))

## import cod samples
ldist <- 
    translate.all.le() %>%
    filter(species.code == 2, sample.id %in% stations$sample.id) %>%
    left_join(stations)

# get lengths at age by year
aldist.by.year <- 
    translate.all.kv() %>%
    filter(species.code == 2, sample.id %in% stations$sample.id) %>%
    left_join(stations)

# compute overall lengths at age
aldist <- 
    aldist.by.year %>% group_by(age, length) %>%
    summarize(total = n())

# get numbers at each age
n.by.age <- aldist %>% group_by(age) %>% summarize(total = sum(total))

# assemble data.frame used to calculate mean and sd
lengths.at.age <- data.frame(age=rep(n.by.age$age, n.by.age$total),
                  length=rep(aldist$length, aldist$total))
age.groups <- data.frame(age = 0:17,
                          age.group = sort(rep(seq(0,16, by=2), 2)))
lengths.at.age <- mutate(lengths.at.age, 
                          age.group = ifelse(age %% 2 == 0, age, age-1))

had.length.mn.sd <- 
    lengths.at.age %>%
    group_by(age.group) %>%
    summarize(length.mean = mean(length), length.sd = sd(length)) %>% 
    na.omit()




###################################################################################
# use survey numbers and total atlantis numbers to calculate proportion for survey
###################################################################################
survey.numbers <- 
    translate.all.nu() %>%
    filter(species.code == 2 & sample.id %in% stations$sample.id) %>%
    left_join(stations) %>%
    mutate(count = ifelse(number.counted == 1 & number.measured == 0, 
                          0,
                          number.counted + number.measured)) %>%
    group_by(year) %>%
    summarize(survey.total = sum(count, na.rm=T))

# must read in atlantis data first
# open ~/gadget/gadget-models/atlantis/cod/initdb/initdb.R
# surv.prop <-
#     is_fg_count %>%
#     group_by(year) %>%
#     summarize(total = sum(count, na.rm=T)) %>%
#     left_join(survey.numbers) %>%
#     mutate(survey.prop = survey.total / total) %>%
#     na.omit() %>%
#     summarize(median = median(survey.prop),
#               mean = mean(survey.prop),
#               sd = sd(survey.prop))




