# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'long'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

area.had <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    mutate(area = as.character(area)) %>%
    group_by(area, year, month, day, age) %>% 
    summarize(count = sum(count))

# to set up as age structured data
age.catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num.caught = count) %>%
    select(area, year, month, num.caught, age) %>%
    mutate(area = as.character(area))

age.discards <- 
    discardAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num.discard = count) %>%
    select(area, year, month, num.discard, age) %>%
    mutate(area = as.character(area))

area.had.catch <- 
    left_join(area.had, age.catch) %>%
    left_join(age.discards) %>%
    mutate(num.caught = ifelse(is.na(num.caught), 0, num.caught), 
           num.discard = ifelse(is.na(num.discard), 0, num.discard)) %>%
    mutate(total = count + num.caught + num.discard)


sub <- filter(area.had.catch, month %in% c(2,3,4,5,6))

# function to calculate M
calcM <- function(total, count) {
    m.vector <- NULL;
    for (i in 2:5) {
        m <- -log(total[i] / total[i-1]) + log(count[i-1] / total[i-1]);
        m.vector <- c(m.vector, m);
    }
    return(median(m.vector))
}

# calculate m for each year, area, age class and get mean and median by age
ann.m <-
    sub %>%
    group_by(year, area, age) %>%
    summarize(monthly.m = calcM(total, count)) %>%
    mutate(ann.m = monthly.m*12) %>%
    filter(ann.m > 0)
    
m.by.age <- 
    ann.m %>%
    group_by(age) %>%
    summarize(m = median(ann.m))