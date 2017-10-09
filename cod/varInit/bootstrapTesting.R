## bootstrap test - using this to see what variance looks like in a bootstrap
# you must first read in ../modelCheck/getAtlantisOutput.R
library(R.utils)
library(parallel)

areas <- filter(read.csv('atlantisInfo/boxInfo.csv', header=T), boundary == 0)
data.subset <- smooth.len
#data.subset <- sample_n(smooth.len, 100000)

spr.bs.survey <- data.subset[
    data.subset$area %in% paste('Box', 0:52, sep='') &
        data.subset$month==4,] %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort,
                                     keep_zero_counts = T) %>%
    atlantis_tracer_survey_select(length_group, rep(0.001, length(length_group)), 0) %>%
    group_by(area, year, month, day, group, cohort, maturity_stage, age, 
             length, weight) %>%
    summarize(count = sum(count)) %>% ungroup()

# get areas in dataset
spr.bs.survey <- mutate(spr.bs.survey, area = as.character(area))
survey.areas <- unique(spr.bs.survey$area)

# create a list to use in the bootstrapping
spr.bs.list <- lapply(0:52, function(x) {
    box <- paste0('Box', x)
    tmp.data <- filter(spr.bs.survey, area == box)
    return(tmp.data)
})
names(spr.bs.list) <- paste0('Box', 0:52)

# now bootstrap - this takes a bit
system.time(
bs.data <- do.call(rbind, mclapply(1:99, function(x) {
    tryCatch(expr = {evalWithTimeout( {
        areas.to.boot <- sample(survey.areas, replace=T)
        boot.data <- do.call(rbind, lapply(areas.to.boot, function(y) {
            return(spr.bs.list[[y]])
        }))
        mean.count <- 
            boot.data %>%
            group_by(year) %>%
            summarize(mn.count = mean(count)) %>%
            mutate(bs.level = x) }, timeout = 20)
        print(paste('lapply() iteration #', x, 'completed'))
        return(mean.count)},
    TimeoutException = function(ex) cat('Timeout. Skipping.\n'))
}, mc.cores = 6))
)


# plot the results in a histogram for each year
bs.hist <- 
    ggplot(data=bs.data, aes(x=mn.count)) + geom_histogram() + 
    facet_wrap(~year, scales = 'free_x') + theme_bw()


# plot results to compare with bs.survey totals
# i.e. bootstrapped to true values
survey.totals <- 
    spr.bs.survey %>% 
    group_by(year) %>%
    summarize(spr.survey.totals = mean(count))

bs.surv.test <- left_join(bs.data, survey.totals)

bs.surv.plot <- 
    ggplot(data=bs.surv.test, aes(x=spr.survey.totals, y=mn.count)) + 
    geom_point() + theme_bw() + xlab('Actual total count') +
    ylab('Resampled total') + geom_abline(slope = 1, intercept = 0)


# taking the log model to get multiplicative error
# see ./cod/varInit/multiplicativeErrorExercise.R
mod <- lm(mn.count ~ spr.survey.totals, data=bs.surv.test)
log.mod <- lm(log(mn.count) ~ log(spr.survey.totals), data=bs.surv.test)
lm.resid <- summary(log.mod)$resid
hist(lm.resid)
sd(lm.resid)
