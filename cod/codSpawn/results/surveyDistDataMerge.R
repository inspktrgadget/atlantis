survdists <- c('ldist.spr', 'ldist.aut', 'aldist.spr', 'aldist.aut')

survdist.data <- 
    do.call(rbind, lapply(survdists, function(x) {
        tmp.dat <- 
            read.table(paste0('Data/surveydistribution.', x, '.'), 
                       comment.char = ';') %>%
            rename(year=V1, step=V2, area=V3, age=V4, length=V5, observed=V6) %>%
            mutate(length = as.numeric(as.character(gsub('len', '', length))),
                   area = as.character(area), age = as.character(age),
                   name = x)
        tmp.predict <- 
            read.table(sprintf('WGTS/out.fit/%s', x), 
                       comment.char = ';',
                       stringsAsFactors = F) %>%
            rename(year=V1, step=V2, area=V3, 
                   age=V4, length=V5, predicted=V6) %>% 
            mutate(length = as.numeric(as.character(gsub('len', '', length))))
        tmp <- left_join(tmp.dat, tmp.predict) %>%
            rename(allages = age)
        if (!any(tmp$age == 'allages')) {
            tmp <- 
                tmp %>%
                mutate(age = as.numeric(as.character(gsub('age', '', allages))))
        }
        return(tmp)
    })) 