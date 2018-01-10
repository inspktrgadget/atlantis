# this is a test to double check atlantis data going into gadget
library(tidyverse)

setwd("~/gadget/models/atlantis/cod/codModel")


# first check length and age data
datas <- c("ldist.spr", "ldist.aut", 
           "aldist.spr", "aldist.aut", 
           "ldist.comm", "aldist.comm")

data_list <- lapply(datas, function(x) {
    tmp <- read.table(sprintf("Data/catchdistribution.%s.sumofsquares", x),
                      comment.char = ";")
    names(tmp) <- c("year", "step", "area", "age", "length", "number")
    tmp$data_type <- x
    if (grepl("aldist", x)) {
        tmp <- 
            tmp %>% 
            group_by(year, step , area, age, data_type) %>%
            summarize(number = sum(number))
    }
    return(tmp)
})
names(data_list) <- datas

ggplot(data=data_list$aldist.comm, aes(x=year, y=number, color = factor(step))) + 
    geom_line() + facet_wrap(~age)


# now check surveyindices
lengths <- c("0.20", "20.35", "35.45", "45.60", "60.80", "80.pl")
season <- c("spr", "aut")
inds <- apply(expand.grid(season, lengths), 1, paste, collapse = ".")

ind_list <- 
    lapply(inds, function(x) {
        ssn <- substr(x, 1, 3)
        len <- substr(x, 5, nchar(x))
        tmp <- read.table(sprintf("Data/surveyindices.%s.si.%s.lengths",
                                  ssn, len),
                              comment.char = ";")
            names(tmp) <- c("year", "step", "area", "length", "number")
            tmp$data_type <- x
            return(tmp)
})
names(ind_list) <- inds

ggplot(data=ind_list$aut.0.20, aes(x=year, y=number)) + 
    geom_line() + facet_wrap(~length)


# check fleet landings just to be thorough
landings <- read.table("Data/fleet.comm.data", comment.char = ";")
names(landings) <- c("year", "step", "area", "fleet", "weight")
ggplot(data=landings, aes(x=year, y=weight, color = factor(step))) + 
    geom_line()
