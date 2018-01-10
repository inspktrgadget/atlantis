library(tidyverse)

# set data directory and get file names
data_dir <- "~/gadget/models/atlantis/cod/codModel/Data"

si_files <- grep("surveyindices", dir(data_dir), value = TRUE)
cd_files <- grep("catchdistribution", dir(data_dir), value = TRUE)

# get data from above directory
si_data <-
    lapply(si_files, function(x) {
        tmp <-
            read.table(paste(data_dir, x, sep = "/"),
                       header = FALSE, comment.char = ";") %>%
            rename(year = V1, step = V2, age = V3, length = V4, number = V5) %>%
            mutate(length = as.numeric(as.character(gsub("len", "", length))))
    }) %>%
    do.call("rbind", .)

cd_data <-
    lapply(cd_files, function(x) {
        tmp <-
            read.table(paste(data_dir, x, sep = "/"),
                       header = FALSE, comment.char = ";") %>%
            rename(year = V1, step = V2, area = V3, age = V4, length = V5, number = V6) %>%
            mutate(length = as.numeric(as.character(gsub("len", "", length))))
        if (grepl("aldist", x)) {
            tmp <-
                tmp %>%
                mutate(age = as.numeric(as.character(gsub("age", "", age)))) %>%
                group_by(year, step, area, age) %>%
                summarize(number = sum(number)) %>%
                mutate(length = NA, cd_type = "aldist",
                       fleet = substr(x, 26, 28)) %>%
                select(year, step, area, age, length, number, cd_type, fleet)
        } else {
            tmp <- 
                tmp %>%
                mutate(cd_type = "ldist", fleet = substr(x, 25, 27),
                       age = as.numeric(NA))
        }
        return(as.data.frame(tmp))
    }) %>%
    do.call("rbind", .)

gad_ldist_data <- 
    cd_data %>%
    filter(cd_type == "ldist") %>%
    group_by(year, fleet, length) %>%
    summarize(number = sum(number))

gad_aldist_data <- 
    cd_data %>%
    filter(cd_type == "aldist") %>%
    group_by(year, fleet, age) %>%
    summarize(number = sum(number))

# a few plots to visualize the gadget data ------------------------------

# survey indices
gad_si_plot <-
    ggplot(data=si_data, aes(x=year, y=number, color = factor(step))) +
    geom_line() + facet_wrap(~length, scales = "free_y")

# plots of length distributions
gad_ldist_ts <-
    filter(gad_ldist_data, fleet == "aut" | fleet == "spr") %>%
    ggplot(aes(x=year, y=number, color = factor(fleet))) +
    geom_line() + facet_wrap(~length, scales = "free_y")

gad_ldist_plot <- 
    filter(gad_ldist_data, fleet == "aut" | fleet == "spr") %>%
    ggplot(aes(x=length, y=number, color = factor(fleet))) + 
    geom_line() + facet_wrap(~year)

# plots of age distributions
gad_aldist_ts <-
    filter(gad_aldist_data, fleet == "aut" | fleet == "spr") %>%
    ggplot(aes(x=year, y=number, color = factor(fleet))) +
    geom_line() + facet_wrap(~as.numeric(age), scales = "free_y")

gad_aldist_plot <- 
    filter(gad_aldist_data, fleet == "aut" | fleet == "spr") %>%
    ggplot(aes(x=age, y=number, color = factor(fleet))) + 
    geom_line() + facet_wrap(~year)
