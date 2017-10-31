# code to compare age-length distributions from  bootstrapped gadget models to 
# gadget models with different error levels on th data
library(tidyverse)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
library(RColorBrewer)
library(grid)
library(gridExtra)

# read in formatted gadget results
setwd("~/gadget/models/atlantis/varTest/results")
source("ageLengthDataAssembly.R")

# set up plot legend objects
theme_palette <- brewer.pal(5, "Set2")
theme_breaks <- c("varMod", "bootMod", "halfBoot", "fullBoot", "atlMod")
theme_values <- c("atlMod" = theme_palette[1],
                  "varMod" = theme_palette[3], "bootMod" = theme_palette[2],
                  "halfBoot" = theme_palette[4], "fullBoot" = theme_palette[5])
theme_labels <- c("varMod" = "EEM", 
                  "bootMod" = expression("BEM"[0]),
                  "halfBoot" = expression("BEM"[0.5]),
                  "fullBoot" = expression("BEM"[1]),
                  "atlMod" = "Atlantis")
theme_linetypes <- c("model" = "solid", "data" = "dashed")

# ldist.spr
ldist.spr.plot <- 
    ggplot(data=al_summary$ldist.spr, aes(x=length)) + 
    geom_line(aes(y=median.prop, color = model, linetype = source)) + 
    geom_ribbon(aes(ymin=ci_lower, ymax = ci_upper, color = model, fill = source), alpha = 0.2) + 
    facet_wrap(~year) + 
    scale_linetype_manual(name = "Models",
                          breaks = c("models", "data"),
                          values = c("models" = "solid", "data" = "dashed")) +
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels)