#  library(tidyverse)
library(mfdb)
library(mfdbatlantis)

# setwd("~/gadget/models/atlantis")
gadget_st_year <- 1970

is_dir <- atlantis_directory("~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF")

is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
mfdb_find_species(is_functional_groups$LongName)["name",],
function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# assemble and import cod 
fgName <- "Cod"
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


# compare biomass by year in gadget to atlantis
atl_biomass <- 
read.table("~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutBiomIndx.txt", 
                      header=T) %>%
mutate(year = 1948:2013) %>%
select(year, starts_with(fg_group$GroupCode)) %>%
mutate(atl_biomass = FCD*1000)


#-----------------------------------------------------------------------
# comparison plots
theme_breaks <- c("gadget", "atlantis")
theme_values <- c("gadget" = "black",
              "atlantis" = "red")
theme_labels <- c("gadget" = "Gadget",
              "atlantis" = "Atlantis")

# plot gadget biomass against atlantis
gad_biomass <- 
fit$stock.std %>%
filter(step == 1) %>%
mutate(total_biomass = mean.weight * number) %>%
group_by(year) %>%
summarize(total_biomass = sum(total_biomass))

atl_gad_biomass <- 
left_join(gad_biomass, atl_biomass) %>%
mutate(scale_diff = total_biomass / atl_biomass)

atl_gad_plot <- 
ggplot(data=atl_gad_biomass, aes(x=atl_biomass, y=total_biomass)) + geom_point() +
geom_abline(intercept=0, slope=1) + theme_bw() + 
xlab("Atlantis Annual Biomass") + ylab("Gadget Annual Biomass")

biomass_comp_plot <-
ggplot(data=atl_gad_biomass, aes(x=year)) + 
geom_line(aes(y=total_biomass/1e6, color="gadget")) +
geom_line(aes(y=atl_biomass/1e6, color="atlantis")) + 
scale_color_manual(name = "",
                   breaks = theme_breaks,
                   values = theme_values, 
                   labels = theme_labels) +
theme_bw() + xlab("Year") + ylab("Biomass (thousand tons)") + 
theme(axis.text = element_text(size = 15),
      axis.title = element_text(size = 17),
      legend.text = element_text(size = 15))

bm_scale_diff_plot <- 
ggplot(data=atl_gad_biomass, aes(x=year, y=scale_diff)) + 
geom_line() + geom_hline(yintercept = 1, linetype="dashed") + 
ylim(0,pmax(1.5, max(atl_gad_biomass$scale_diff, na.rm=T))) +
theme_bw() + xlab("Year") + ylab("Relative difference in biomass") + 
theme(axis.text = element_text(size = 15),
      axis.title = element_text(size = 17),
      legend.text = element_text(size = 15))

#########################
# SSB
#########################
atl_ssb <- 
is_fg_count %>%
filter(count >= 1, age >= 4, month == 3) %>%
group_by(year, age) %>%
summarize(atl_ssb = sum((weight / 1e3) * count))

gad_ssb <- 
fit$stock.std %>%
filter(age >= 4, step == 1) %>%
mutate(age = age - (age %% 2)) %>%
group_by(year, age) %>%
summarize(gad_ssb = sum(number * mean.weight))

atl_gad_ssb <- 
left_join(gad_ssb, atl_ssb)

# total ssb
ssb <- 
atl_gad_ssb %>%
group_by(year) %>%
summarize(gad_ssb = sum(gad_ssb),
          atl_ssb = sum(atl_ssb))

ssb_plot <- 
ggplot(data=ssb, aes(x=year)) + 
geom_line(aes(y=gad_ssb/1e6, color = "gadget")) + 
geom_line(aes(y=atl_ssb/1e6, color = "atlantis")) +
xlab("Year") + ylab("SSB (thousand tons)") + theme_bw() +
scale_color_manual(name = "",
                   breaks = theme_breaks,
                   values = theme_values, 
                   labels = theme_labels) + 
theme(axis.text = element_text(size = 15),
      axis.title = element_text(size = 17),
      legend.text = element_text(size = 15))


# ssb by age
ssb_age_plot <- 
ggplot(data=atl_gad_ssb, aes(x=year)) + 
geom_line(aes(y=gad_ssb / 1e6, color = "gadget")) + 
geom_line(aes(y=atl_ssb / 1e6, color = "atlantis")) + facet_wrap(~age, scales = "free_y") +
xlab("Year") + ylab("SSB (thousand tons)") + theme_bw() + 
scale_color_manual(name = "",
                   breaks = theme_breaks,
                   values = theme_values, 
                   labels = theme_labels) +
theme(axis.text = element_text(size = 15),
      axis.title = element_text(size = 17),
      legend.text = element_text(size = 15))

#######################################
## to check numbers instead of biomass
#######################################
cod_numbers <- 
    is_fg_count %>% 
    filter(month == 3) %>%
    group_by(year) %>% 
    summarize(atl_number = sum(count))
gad_numbers <- 
    fit$stock.std %>%
    filter(step == 1) %>%
    group_by(year) %>%
    summarize(gad_number = sum(number))
atl_gad_numbers <- 
    left_join(gad_numbers, cod_numbers) %>%
    mutate(scale_diff = gad_number / atl_number)

numbers_comp_plot <- 
    ggplot(data=filter(atl_gad_numbers, year < 2011), aes(x=year)) + 
    geom_line(aes(y=gad_number/1e6, color="gadget")) +
    geom_line(aes(y=atl_number/1e6, color="atlantis")) +
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Numbers (millions of fish)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

nmb_scale_diff_plot <- 
    ggplot(data=atl_gad_numbers, aes(x=year, y=scale_diff)) + geom_line() +
    geom_hline(yintercept = 1, linetype="dashed") +
    ylim(0,pmax(1.5, max(atl_gad_numbers$scale_diff, na.rm=T))) +
    theme_bw() + xlab("Year") + ylab("Relative difference in numbers") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
## check landings
#######################################
atl_landings <- 
read.table("~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF/OutCatch.txt", 
           header=T) %>%
mutate(year = 1948:2012) %>%
select(year, starts_with(fg_group$GroupCode))

atl_catch_plot <- catch_plot + geom_line(data=atl_landings, aes(x=year, y=FCD))


#######################################
## check numbers by age
#######################################
gad_age_numbers <- 
    fit$stock.std %>%
    filter(step == 1) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age) %>%
    summarize(gad_number = sum(number))

atl_age_numbers <- 
    is_fg_count %>%
    filter(month == 3, count >= 1) %>%
    mutate(age = ifelse(age >= 12, 12, age)) %>%
    group_by(year, age) %>%
    summarize(atl_number = sum(count))

atl_gad_age_numbers <- left_join(gad_age_numbers, atl_age_numbers)

age_numbers_plot <-
    ggplot(data=filter(atl_gad_age_numbers, year < 2011), 
           aes(x=year, y=gad_number/1e6, color="gadget")) + geom_line() + 
    geom_line(aes(x=year, y=atl_number/1e6, color="atlantis")) + 
    facet_wrap(~age, scales="free_y") + 
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Numbers (millions of fish)") + 
    theme(axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

# ----------------------------------------------------------
# age numbers by parsed out by step
# Note: this only works when all timesteps are printed by gadget
gad_step_age_numbers <- 
    fit$stock.std %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(year, age, step) %>%
    summarize(gad_number = sum(number))

monthToStep <- sort(rep(1:4,3))
atl_step_age_numbers <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    mutate(step = monthToStep[month]) %>%
    filter(month %in% c(1,4,7,10)) %>%
    group_by(year, age, step) %>%
    summarize(atl_number = sum(count))

atl_gad_step_age_numbers <- 
    left_join(gad_step_age_numbers, atl_step_age_numbers)

step_age_numbers_plot <-
    ggplot(data=atl_gad_step_age_numbers, 
           aes(x=year, y=gad_number/1e6, color ="gadget", 
               linetype=factor(step))) + 
    geom_line() + 
    geom_line(data=atl_gad_step_age_numbers,
              aes(x=year, y=atl_number/1e6, color="atlantis", 
                  linetype=factor(step))) + 
    facet_wrap(~age, scales="free_y") + 
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    scale_linetype_discrete(name = "Gadget Timestep") + 
    theme_bw() + xlab("Year") + ylab("Numbers (millions of fish)") + 
    theme(axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#-----------------------------------------------------------
# visualize the contribution of each age group to biomass
gad_age_biomass <- 
    fit$stock.std %>%
    filter(step == 3) %>%
    mutate(age = age - (age %% 2),
           biomass = mean.weight * number) %>%
    group_by(year, age) %>%
    summarize(total_biomass = sum(biomass))

atl_age_biomass <- 
    is_fg_count %>%
    filter(month == 9, count >= 1) %>%
    mutate(biomass = (count * weight)/1e3) %>%
    group_by(year, age) %>%
    summarize(atl_biomass = sum(biomass))

atl_gad_age_biomass <- 
    left_join(gad_age_biomass, atl_age_biomass) %>%
    mutate(diff = total_biomass - atl_biomass,
           scale_diff = total_biomass / atl_biomass)

age_biomass_comp_plot <-
    ggplot(data=atl_gad_age_biomass, aes(x=year)) + 
    geom_line(aes(y=total_biomass/1e3, color="gadget")) +
    geom_line(aes(y=atl_biomass/1e3, color="atlantis")) + 
    facet_wrap(~age, scales = "free_y") +
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Biomass (tons)") + 
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))

diff_by_age <- 
    atl_gad_age_biomass %>%
    filter(year >= gadget_st_year) %>%
    group_by(age) %>%
    summarize(mn_diff = mean(diff/1e3),
              se_diff = sd(diff/1e3) / sqrt(n()))

diff_by_age_plot <- 
    ggplot(data=diff_by_age, aes(x=age, y=mn_diff)) + geom_point(size = 4) +
    geom_errorbar(aes(ymin = mn_diff - se_diff, 
                      ymax = mn_diff + se_diff), width = 0) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme_bw() + xlab("Age") + ylab("Difference in Biomass (tons)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
## check growth
#######################################
cod_growth <- 
    is_fg_count %>%
    filter(count > 0) %>%
    sample_n(10000)

gr_check <- 
    ggplot(data=cod_growth, aes(x=age, y=length)) + 
    geom_point(size = 3, shape=1) +
    geom_line(data=fit$stock.growth, aes(x=age, y=length)) +
    theme_bw() + xlab("Age") + ylab("Length (cm)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
# compare gadget initial values to 
# atlantis initial values
#######################################
gad_init_year <- min(fit$stock.std$year)
atl_init <- 
    is_fg_count %>%
    filter(year == gad_init_year,
           month == 3,
           count >= 1) %>%
    group_by(age) %>%
    summarize(atl_init = sum(count))

gad_init <- 
    fit$stock.std %>%
    filter(year == min(year)) %>%
    mutate(age = age - (age %% 2)) %>%
    group_by(age) %>% 
    summarize(gad_init = sum(number))

gad_atl_init_plot <- 
    ggplot(data=gad_init, aes(x=age, y=gad_init)) + 
    geom_bar(stat="identity") + 
    geom_line(data=atl_init, aes(x=age, y=atl_init))
