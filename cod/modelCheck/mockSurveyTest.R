## setting up a mock surveydistribution to see what is going on
 # open up getAtlantisOutput.R first and read in through the survey

# create just the length distributions - note survey selectivity is 1
atl.ldist <- smooth.len[
    smooth.len$area %in% paste('Box', 0:52, sep='') &
        smooth.len$month %in% c(3,9),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, rep(1, length(length_group)), 0)


data.st.year <- 1983
si <- 
    survey %>%
    filter(year >= data.st.year, month == 3) %>%
    mutate(length.grps = cut(length, breaks=c(0, 18, 36, max(length)))) %>%
    mutate(new.length.grps = factor(length.grps, labels = c(0,18,36))) %>%
    mutate(length = as.numeric(as.character(new.length.grps))) %>%
    group_by(year, month, length) %>%
    summarize(si.total = sum(count))

# format the actual numbers in a similar way
true.vals <- 
    atl.ldist %>%
    filter(year >= data.st.year, month == 3) %>%
    mutate(length.grps = cut(length, breaks=c(0, 18, 36, max(length)))) %>%
    mutate(new.length.grps = factor(length.grps, labels = c(0,18, 36))) %>%
    mutate(length = as.numeric(as.character(new.length.grps))) %>%
    group_by(year, month, length) %>%
    summarize(tv.total = sum(count))

si.tv <- left_join(true.vals, si)

all.mod <- lm(log(si.total) ~ log(tv.total), data=si.tv)
si.short.mod <- lm(log(si.total) ~ log(tv.total), 
                   data = filter(si.tv, length == 0))
si.mid.mod <- lm(log(si.total) ~ log(tv.total), 
                   data = filter(si.tv, length == 18))
si.long.mod <- lm(log(si.total) ~ log(tv.total), 
                   data = filter(si.tv, length == 36))
si.plot <- 
    ggplot(data=si.tv, aes(x=log(tv.total), y=log(si.total), 
                           color=factor(length))) + 
    geom_point() + geom_abline(slope = 1, intercept = -6.907755)

# there's obviously an issue in atlantis_tracer_survey_select
# with the longer age groups - see survey.test and si.test.plot below
## testing out just taking 0.001 of the data
survey.test <- 
    atl.ldist %>%
    mutate(survey.count = count * 0.001) %>%
    filter(year >= data.st.year, month == 3) %>%
    mutate(length.grps = cut(length, breaks=c(0, 18, 36, max(length)))) %>%
    mutate(new.length.grps = factor(length.grps, labels = c(0,18, 36))) %>%
    mutate(length = as.numeric(as.character(new.length.grps))) %>%
    group_by(year, month, length) %>%
    summarize(tv.total = sum(count),
              si.total = sum(survey.count))

si.test.plot <- 
    ggplot(data=survey.test, aes(x=log(tv.total), y=log(si.total),
                                 color=factor(length))) + 
    geom_point() + geom_abline(slope=1, intercept = -6.907755)


# ----------------------------------------------------------------------
# now I'm going to set up a mock surveydistribution for lengths and ages
# must run getAtlantisOutput.R through al.survey for this
ldist <- 
    al.survey %>%
    filter(!is.na(length), year >= data.st.year, month == 3) %>%
    group_by(year, month, length) %>%
    summarize(si.total = sum(count))

atl.ldist.test <- 
    atl.ldist %>%
    filter(year >= data.st.year) %>%
    group_by(year, month, length) %>%
    summarize(tv.total = sum(count)) %>%
    mutate(mock.si = tv.total * (exp(-6.907755) * 0.7072256))

ldist.test <- left_join(atl.ldist.test, ldist)

# first plot ldist from al.survey against true values
mock.si.plot <- 
    ggplot(data=ldist.test, aes(x=log(tv.total), y=log(mock.si))) + 
    geom_point() + geom_abline(slope = 1, intercept = -7.254161)

ldist.test.plot <- 
    ggplot(data=ldist.test, aes(x=log(tv.total), y=log(si.total))) + 
    geom_point() + geom_abline(slope = 1, intercept = -7.254161)




