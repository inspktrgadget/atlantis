# code to compare bootstrapped gadget models to gadget models with different variance
# this script uses results from the res.by.year output from Rgadget
library(tidyverse)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)

# read in formatted gadget results
setwd("~/gadget/models/atlantis/varTest/results")
source("varBootDataAssembly_resByYear.R")


# compare numbers across time
theme_breaks <- c("varMod", "bootMod", "halfBoot", "fullBoot", "atlMod")
theme_values <- c("varMod" = "darkred", "bootMod" = "black",
                  "halfBoot" = "Purple", "fullBoot" = "Green",
                  "atlMod" = "red")
theme_labels <- c("varMod" = "Variance Models", 
                  "bootMod" = "Bootstrap (No Error)",
                  "halfBoot" = "Bootstrap (Half Error)",
                  "fullBoot" = "Bootstrap (Full Error)",
                  "atlMod" = "Atlantis")
numbersCompPlot <- 
    ggplot(data=bmn_summary, aes(x=year)) + 
    geom_ribbon(aes(ymin = total.number_median - (1.96 * total.number_se), 
                    ymax = total.number_median + (1.96 * total.number_se), 
                    fill = model), alpha = 0.2) + 
    geom_line(aes(y=total.number_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_numbers, month == 9, year > 1969),
              aes(x=year, y=total.number, color = "atlMod")) + 
    xlab("Year") + ylab("Total number (millions)") + theme_bw() + 
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels)

# compare number densities
numbersCompDens <- 
    ggplot(data=filter(res_by_year, year > 1982), 
           aes(x=total.number/1e6, fill = model, color = model)) + 
    geom_density(alpha = 0.5) +
    geom_vline(data=filter(atl_numbers, year > 1982, year < 2013, month == 1),
               aes(xintercept = total.number), color = "black") +
    facet_wrap(~year) + 
    theme_bw() + xlab("Total number (millions)") + ylab("Density") + 
    scale_color_discrete(guide = FALSE) + 
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels)

# compare biomass
biomassCompPlot <- 
    ggplot(data=bmn_summary, aes(x=year)) + 
    geom_ribbon(aes(ymin = total.biomass_median - (1.96 * total.biomass_se), 
                    ymax = total.biomass_median + (1.96 * total.biomass_se), 
                    fill = model), alpha = 0.2) + 
    geom_line(aes(y=total.biomass_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_biomass, year > 1969), 
              aes(x=year, y=atl.biomass, color = "atlMod")) +
    xlab("Year") + ylab("Total biomass (thousands of tons)") + theme_bw() + 
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels)

# compare densities of biomass
biomassCompDens <- 
    ggplot(data=filter(res_by_year, year > 1982), 
           aes(x=total.biomass/1e6, fill = model, color = model)) + 
    geom_density(alpha = 0.2) +
    geom_vline(data=filter(atl_biomass, year > 1982, year < 2013),
               aes(xintercept = atl.biomass), color = "black") +
    facet_wrap(~year) + theme_bw() + xlab("Total biomass (thousands of tons)") + ylab("Density") + 
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels)