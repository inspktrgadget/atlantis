# running mock survey trials to get an idea of good sigma values
# must read in data from ../modelCheck/getAtlantisOutput.R
source('functions/pauls_add_lengthgroups.R')

survey_sigma <- c(0, 0.25, 0.5, 0.75, 1)
data.subset <- sample_n(smooth.len, 10000)
test.survey <- NULL
for (i in survey_sigma) {
    temp.survey <- data.subset[
        data.subset$area %in% paste('Box', 0:52, sep='') &
            data.subset$month %in% c(4,9),] %>%
        mutate(sampling_type = ifelse(month == 4,
                                      "SprSurveyTotals",
                                      "AutSurveyTotals")) %>%
        atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort,
                                         keep_zero_counts = T) %>%
        atlantis_tracer_survey_select(length_group, rep(0.001, length(length_group)), i) %>%
        mutate(sigma = i)
    test.survey <- rbind(test.survey, temp.survey)
}

test.indices <- 
    test.survey %>%
    group_by(sigma, year, month, age) %>%
    summarize(total = sum(count))

# plot to check what errors look like - counts not grouped
sigma0.count <- filter(test.survey, sigma == 0)$count
error.test <- 
    test.survey %>%
    filter(sigma > 0) %>%
    mutate(sigma0.count = rep(sigma0.count, nrow(.) / length(sigma0.count)))

error.plot <- 
    ggplot(data=error.test, aes(x=log(sigma0.count), y=log(count))) + geom_point() + 
    facet_wrap(~sigma)

# plot to check what errors look like - counts grouped to year
grp.error.data <- 
    test.survey %>%
    group_by(sigma, year, month) %>%
    summarize(total.count = sum(count))
grp.sigma0.count <- filter(grp.error.data, sigma==0)$total.count
grp.error.test <- 
    grp.error.data %>%
    ungroup() %>%
    filter(sigma > 0) %>%
    mutate(sigma0.count = rep(grp.sigma0.count, nrow(.)/length(grp.sigma0.count)))

grp.error.plot <- 
    ggplot(data=grp.error.test, aes(x=sigma0.count, y=total.count)) + 
    geom_point() + facet_wrap(~sigma)

