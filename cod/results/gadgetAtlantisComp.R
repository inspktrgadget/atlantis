#  library(tidyverse)
library(mfdb)
library(mfdbatlantis)
source("~/gadget/models/atlantis/functions/smooth_age_weight.R")

# setwd("~/gadget/models/atlantis")
gadget_st_year <- min(fit$stock.std$year)

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

# distribute atlantis age groups to single year classes and smooth weight among them
z_vals <- c(0.31, 0.325, 0.45, 0.54, 0.6, 0.62, 0.63, 0.63, 0.63, 0.01)
alpha <- fg_group$FLAG_LI_A
beta <- fg_group$FLAG_LI_B

smooth_len <- 
    is_fg_count %>%
    parse_ages(z_vals) %>%
    smooth_atl_weight() %>%
    mutate(length = wt2len(alpha, beta, weight))

# compare biomass by year in gadget to atlantis
mon2step <- c(3,6,9,12)
atl_age_data <- 
    smooth_len %>%
    filter(count >= 1) %>%
    mutate(weight = weight / 1000) %>%
    group_by(year, month, age) %>%
    summarize(atl_numbers = sum(count), 
              atl_biomass = sum(count * weight)) %>%
    mutate(step = match(month, mon2step)) %>%
    filter(!is.na(step)) %>%
    ungroup()

atl_data <- 
    atl_age_data %>%
    group_by(year, step) %>%
    summarize(atl_numbers = sum(atl_numbers), 
              atl_biomass = sum(atl_biomass))

gad_age_data <- 
    fit$stock.std %>%
    group_by(year, step, age) %>%
    summarize(gad_numbers = sum(number),
              gad_biomass = sum(mean.weight * number)) %>%
    ungroup()

gad_data <- 
    gad_age_data %>%
    group_by(year, step) %>%
    summarize(gad_numbers = sum(gad_numbers),
              gad_biomass = sum(gad_biomass)) %>%
    ungroup()

gad_atl_age_data <- left_join(gad_age_data, atl_age_data, by = c("year", "step", "age"))
gad_atl_data <- left_join(gad_data, atl_data, by = c("year", "step"))

#-----------------------------------------------------------------------
# comparison plots
theme_breaks <- c("gadget", "atlantis")
theme_values <- c("gadget" = "black",
                  "atlantis" = "red")
theme_labels <- c("gadget" = "Gadget",
                  "atlantis" = "Atlantis")

# plot gadget biomass against atlantis
atl_gad_plot <- 
    ggplot(data=atl_gad_biomass, aes(x=atl_biomass, y=total_biomass)) + geom_point() +
    geom_abline(intercept=0, slope=1) + theme_bw() + 
    xlab("Atlantis Annual Biomass") + ylab("Gadget Annual Biomass")

biomass_comp_plot <-
    ggplot(data=filter(gad_atl_data, step == 1), aes(x=year)) + 
    geom_line(aes(y=gad_biomass/1e6, color="gadget")) +
    geom_line(aes(y=atl_biomass/1e6, color="atlantis")) + 
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Biomass (thousand tons)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#########################
# SSB
#########################


# total ssb
ssb <- 
    gad_atl_age_data %>%
    filter(age >= 4) %>%
    group_by(year, step) %>%
    summarize(gad_ssb = sum(gad_biomass),
              atl_ssb = sum(atl_biomass))
    
ssb_plot <- 
    ggplot(data=filter(ssb, step == 1), aes(x=year)) + 
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
    ggplot(data=filter(gad_atl_age_data, age >= 4, step == 1), aes(x=year)) + 
    geom_line(aes(y=gad_biomass/1e6, color = "gadget")) + 
    geom_line(aes(y=atl_biomass/1e6, color = "atlantis")) + facet_wrap(~age, scales = "free_y") +
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
numbers_comp_plot <- 
    ggplot(data=filter(gad_atl_data, step == 1, year < 2011), aes(x=year)) + 
    geom_line(aes(y=gad_numbers/1e6, color="gadget")) +
    geom_line(aes(y=atl_numbers/1e6, color="atlantis")) +
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Numbers (millions of fish)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#######################################
## check numbers by age
#######################################
age_numbers_plot <-
    ggplot(data=filter(gad_atl_age_data, step == 1, year < 2011), 
           aes(x=year, y=gad_numbers/1e6, color="gadget")) + geom_line() + 
    geom_line(aes(x=year, y=atl_numbers/1e6, color="atlantis")) + 
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
step_age_numbers_plot <-
    ggplot(data=filter(gad_atl_age_data, year < 2011), 
           aes(x=year, y=gad_numbers/1e6, linetype = factor(step), color="gadget")) + 
    geom_line() + 
    geom_line(aes(x=year, y=atl_numbers/1e6, color="atlantis")) + 
    facet_wrap(~age, scales="free_y") + 
    scale_color_manual(name = "",
                       breaks = theme_breaks,
                       values = theme_values, 
                       labels = theme_labels) +
    theme_bw() + xlab("Year") + ylab("Numbers (millions of fish)") + 
    theme(axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))


#-----------------------------------------------------------
# visualize the contribution of each age group to biomass
age_biomass_plot <-
    ggplot(data=filter(gad_atl_age_data, step == 1), aes(x=year)) + 
    geom_line(aes(y=gad_biomass/1e3, color="gadget")) +
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


#######################################
## check growth
#######################################
cod_growth <- 
    smooth_len %>%
    filter(count > 0) %>%
    sample_n(1e4)

gr_check <- 
    ggplot(data=cod_growth, aes(x=age, y=length)) + 
    geom_point(size = 3, shape=1) +
    geom_line(data=fit$stock.growth, aes(x=age, y=length)) +
    theme_bw() + xlab("Age") + ylab("Length (cm)") + 
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 17),
          legend.text = element_text(size = 15))
