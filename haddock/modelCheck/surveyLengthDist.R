atl.spr.lw <- is_fg_count %>% filter(count >=1, month == 3)
atl.spr.survey.lw <- is_fg_survey %>% filter(count >=1, month == 3)
mn.wt <- atl.spr.survey.lw %>% group_by(length) %>% summarize(mn.wt = mean(weight))
mn.laa <- atl.spr.survey.lw %>% group_by(age) %>% summarize(mn.laa = mean(length))

# testing weight at length
ggplot(atl.spr.lw, aes(x=length, y=weight)) + geom_point() + 
    geom_point(data=atl.spr.survey.lw, aes(x=length, y=weight), color='red') + 
    geom_point(data=mn.wt, aes(x=length, y=mn.wt), pch='x', color='blue', size=5)

# testing length at age
ggplot(atl.spr.lw, aes(x=age, y=length)) + geom_point() +
    geom_point(data=atl.spr.survey.lw, aes(x=age, y=length), color='red') +
    geom_point(data=mn.laa, aes(x=age, y=mn.laa), pch='x', color='blue', size=5)

# testing length distributions
atl.ldist <- 
    atl.spr.lw %>% 
    mutate(len.grp = round(length)) %>%
    group_by(year, len.grp) %>%
    summarize(total = sum(count) * 5e-04)
atl.survey.ldist <- 
    atl.spr.survey.lw %>%
    mutate(len.grp = round(length)) %>%
    group_by(year, len.grp) %>%
    summarize(total = sum(count)) 
ggplot(atl.ldist, aes(x=len.grp, y=total)) + geom_line() + 
    geom_line(data=atl.survey.ldist, aes(x=len.grp, y=total), color='red') + 
    facet_wrap(~year)





