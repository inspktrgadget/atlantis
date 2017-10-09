# this code is for diagnostic plots of the age parsing and length smoothing in 
# Atlantis output data
# basically, I split each 2 year age group in Atlantis into 1 year age groups based
# on the natural mortality (see code) and then smoothed the lengths to the appropriate
# respective ages using the von Bertalanffy growth curves for each year and month



## this is just to plot actual length/biomass and smoothed length/biomass across time
## to see how they differ. You can also do this facetted by age
ts.len.test <- 
    len %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age) %>%
    summarize(act.biomass = sum(count*length),
              test.biomass = sum(count*test.length))

ts.len.plot <- 
    ggplot(data=ts.len.test, aes(x=year)) + geom_line(aes(y=act.biomass), color='red') + 
    geom_line(aes(y=test.biomass)) + facet_wrap(~age, scale='free_y')

# heeeeeeeeeer's biomass
ts.wt.test <- 
    wt %>%
    #mutate(age = age - (age %% 2)) %>%
    group_by(year, age) %>%
    summarize(act.biomass = sum(count*weight),
              test.biomass = sum(count*test.wt))

ts.wt.plot <- 
    ggplot(data=ts.wt.test, aes(x=year)) + geom_line(aes(y=act.biomass), color='red') + 
    geom_line(aes(y=test.biomass)) + facet_wrap(~age, scale='free_y')


## this looks at the growth across months for each age class
## if you do this with the actual output you will see big jumps at month 9-10
## when fish move from one age group to the next
## this is a good test to see if you've accurately
## distributed numbers and lengths properly between years
true.grow <- 
    smooth.len %>%
    filter(age %in% c(0,1,2,3,4,5),
           year %in% c(1948:2013)) 

true.grow.plot <- 
    ggplot(data=true.grow, aes(x=month, y=length, color=factor(age))) + geom_point() + facet_wrap(~year) +
    geom_vline(xintercept = 10)
