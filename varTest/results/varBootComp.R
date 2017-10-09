# code to compare bootstrapped gadget models to gadget models with different variance
# this script uses results from the stock.std output from gadget
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
source("varBootDataAssembly.R")

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


#------------------------------------------------------------------------
# basic time-series plots (numbers, biomass, SSB)

# plot numbers by time
numbersCompPlot <- 
    ggplot(data=filter(bmn_summary, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = total.number_median,
                        ymin = total.number_ci_lower,
                    ymax = total.number_ci_upper,
                    color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=total.number_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_numbers, month == 4, year > 1969),
              aes(x=year, y=atl.number, color = "atlMod")) + 
    xlab("Year") + ylab("Total number (millions)") + theme_bw() + 
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels) + 
    scale_color_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels) + 
    theme(legend.text.align = 0)


# compare biomass
biomassCompPlot <- 
    ggplot(data=filter(bmn_summary, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = total.biomass_median,
                        ymin = total.biomass_ci_lower,
                        ymax = total.biomass_ci_upper,
                        color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=total.biomass_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_total_biomass, year > 1969, month == 4), 
              aes(x=year, y=atl.biomass, color = "atlMod")) +
    xlab("Year") + ylab("Total biomass (thousands of tons)") + theme_bw() +
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

# compare ssb
ssbCompPlot <- 
    ggplot(data=filter(gadget_ssb_summary, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = ssb_median,
                        ymin = ssb_ci_lower,
                        ymax = ssb_ci_upper,
                        color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=ssb_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_ssb, year > 1969, month == 4), 
              aes(x=year, y=atl.ssb, color = "atlMod")) +
    xlab("Year") + ylab("SSB (thousands of tons)") + theme_bw() +
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) + 
    theme(legend.text.align = 0)


#---------------------------------------------------------------------------
# time series plots as above but facetted by age

# plot numbers by time and age
ageNumbersCompPlot <- 
    ggplot(data=filter(bmn_summary_by_age, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = total.number_median,
                        ymin = total.number_ci_lower,
                        ymax = total.number_ci_upper,
                        color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=total.number_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_numbers_by_age, month == 4, year > 1969),
              aes(x=year, y=atl.number, color = "atlMod")) + 
    facet_wrap(~age, scales = "free_y") + 
    xlab("Year") + ylab("Total number (millions)") + theme_bw() + 
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) + 
    theme(legend.text.align = 0)

# compare biomass by age
ageBiomassCompPlot <- 
    ggplot(data=filter(bmn_summary_by_age, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = total.biomass_median,
                        ymin = total.biomass_ci_lower,
                        ymax = total.biomass_ci_upper,
                        color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=total.biomass_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_total_biomass_by_age, year > 1969, month == 4), 
              aes(x=year, y=atl.biomass, color = "atlMod")) +
    facet_wrap(~age, scales = "free_y") + 
    xlab("Year") + ylab("Total biomass (thousands of tons)") + theme_bw() +
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

# compare ssb
ageSSBcompPlot <- 
    ggplot(data=filter(gadget_ssb_summary_by_age, step == 1), aes(x=year)) + 
    geom_pointrange(aes(y = ssb_median,
                        ymin = ssb_ci_lower,
                        ymax = ssb_ci_upper,
                        color = model), 
                    fatten = 2,
                    position = position_dodge(width = 0.6), show.legend = FALSE) +
    geom_line(aes(y=ssb_median, color = model), alpha = 0.8) + 
    geom_line(data=filter(atl_ssb_by_age, year > 1969, month == 4), 
              aes(x=year, y=atl.ssb, color = "atlMod")) +
    facet_wrap(~age, scales = "free_y") +
    xlab("Year") + ylab("SSB (thousands of tons)") + theme_bw() +
    scale_fill_discrete(guide = FALSE) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) + 
    theme(legend.text.align = 0)


#-------------------------------------------------------------------------
# density comparison plots (numbers, biomass, SSB)

# compare number densities
numbersCompDens <- 
    ggplot(data=filter(res_by_step, step == 1, year > 1982), 
           aes(x=total.number/1e6, fill = model, color = model)) + 
    geom_density(alpha = 0.5) +
    geom_vline(data=filter(atl_numbers, year > 1982, year < 2013, month == 4),
               aes(xintercept = atl.number), color = "black") +
    facet_wrap(~year) + 
    theme_bw() + xlab("Total number (millions)") + ylab("Density") + 
    scale_color_discrete(guide = FALSE) + 
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels)


# compare densities of biomass
biomassCompDens <- 
    ggplot(data=filter(res_by_step, step == 1, year > 1982), 
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


# compare densities of ssb
ssbCompDens <- 
    ggplot(data=filter(gadget_ssb, step == 1, year > 1982), 
           aes(x=ssb/1e6, fill = model, color = model)) + 
    geom_density(alpha = 0.2) +
    geom_vline(data=filter(atl_ssb, year > 1982, year < 2013, month == 4),
               aes(xintercept = atl.ssb), color = "black") +
    facet_wrap(~year) + theme_bw() + xlab("SSB (thousands of tons)") + ylab("Density") + 
    scale_fill_manual(name = "Models",
                      breaks = theme_breaks,
                      values = theme_values,
                      labels = theme_labels) + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels)


#-------------------------------------------------------------------------
# forest plots to show bias in numbers and ssb at age among models
bias_data <- 
    res_by_step_age %>%
    filter(step == 1, year > 1982) %>%
    mutate(month = 4,
           age = age - (age %% 2)) %>%
    rename(gad.number = total.number,
           gad.biomass = total.biomass) %>%
    group_by(model, model_index, year, month, step, area, age) %>%
    summarize(gad.number = sum(gad.number) / 1e6,
              gad.biomass = sum(gad.biomass) / 1e6) %>%
    ungroup()

num_bias_data <- 
    bias_data %>%
    left_join(atl_numbers_by_age, by = c("year", "age", "month")) %>%
    mutate(gad.atl.bias = gad.number / atl.number) %>%
    group_by(model, year, age) %>%
    summarize(median.bias = median(gad.atl.bias),
              se.bias = sd(gad.atl.bias) / sqrt(n()))

mn_num_bias <- 
    num_bias_data %>%
    group_by(model, age) %>%
    summarize(mean.bias = mean(median.bias),
              se.bias = sd(median.bias) / sqrt(n()))

numBiasPlot <- 
    ggplot(data=mn_num_bias, aes(x=model, y=mean.bias, color = model)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = mean.bias - se.bias,
                      ymax = mean.bias + se.bias),
                  position = "dodge", width = 0.2) + 
    facet_wrap(~age) + ylim(-1,20) +  
    geom_hline(yintercept = 1, linetype = "dashed") + 
    scale_x_discrete(labels = theme_labels) + 
    coord_flip() +
    ylab("Mean Bias") + xlab("") + theme_bw() + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

numBiasPlot2 <- 
    ggplot(data=mn_num_bias, aes(x=factor(age), y=mean.bias, color = model)) + 
    geom_point(position = position_dodge(width = 0.4)) + 
    geom_errorbar(aes(ymin = mean.bias - se.bias,
                      ymax = mean.bias + se.bias),
                  position = position_dodge(width = 0.4), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_bw() + xlab("Age") + ylab("Mean Bias") + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

# now do the same for ssb
ssb_bias_data <- 
    bias_data %>%
    filter(age >= 4) %>%
    rename(gad.ssb = gad.biomass) %>%
    left_join(atl_ssb_by_age) %>%
    mutate(gad.atl.bias = gad.ssb / atl.ssb) %>%
    group_by(model, year, age) %>%
    summarize(median.bias = median(gad.atl.bias),
              se.bias = sd(gad.atl.bias) / sqrt(n()))

mn_ssb_bias <- 
    ssb_bias_data %>%
    group_by(model, age) %>%
    summarize(mean.bias = mean(median.bias),
              se.bias = sd(median.bias) / sqrt(n()))

ssbBiasPlot <- 
    ggplot(data=mn_ssb_bias, aes(x=model, y=mean.bias, color = model)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = mean.bias - se.bias,
                      ymax = mean.bias + se.bias),
                  position = "dodge", width = 0.2) + 
    facet_wrap(~age) + ylim(-1,20) +  
    geom_hline(yintercept = 1, linetype = "dashed") + 
    scale_x_discrete(labels = theme_labels) + 
    coord_flip() +
    ylab("Mean Bias") + xlab("") + theme_bw() + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

ssbBiasPlot2 <- 
    ggplot(data=mn_ssb_bias, aes(x=factor(age), y=mean.bias, color = model)) + 
    geom_point(position = position_dodge(width = 0.4)) + 
    geom_errorbar(aes(ymin = mean.bias - se.bias,
                      ymax = mean.bias + se.bias),
                  position = position_dodge(width = 0.4), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_bw() + xlab("Age") + ylab("Mean Bias") + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)


# now take a look at the relative difference

# relative difference for numbers
num_rel_diff <- 
    bias_data %>%
    left_join(atl_numbers_by_age, by = c("year", "age", "month")) %>%
    mutate(gad.atl.diff = gad.number - atl.number) %>%
    group_by(model, year, age) %>%
    summarize(median.diff = median(gad.atl.diff), 
              se.diff = sd(gad.atl.diff) / sqrt(n()))

mn_num_diff <- 
    num_rel_diff %>%
    group_by(model, age) %>%
    summarize(mn.diff = mean(median.diff),
              se.diff = sd(median.diff) / sqrt(n()))

numDiffPlot <- 
    ggplot(data=mn_num_diff, aes(x=age, y=mn.diff, color = model)) + 
    geom_point(position = position_dodge(width = 0.4)) + 
    geom_errorbar(aes(ymin = mn.diff - se.diff,
                      ymax = mn.diff + se.diff),
                  position = position_dodge(width = 0.4), width = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() + xlab("Age") + ylab("Absolute Difference") + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

# relative difference in ssb
ssb_rel_diff <- 
    bias_data %>%
    filter(age >= 4) %>%
    rename(gad.ssb = gad.biomass) %>%
    left_join(atl_ssb_by_age, by = c("year", "age", "month")) %>%
    mutate(gad.atl.diff = gad.ssb - atl.ssb) %>%
    group_by(model, year, age) %>%
    summarize(median.diff = median(gad.atl.diff), 
              se.diff = sd(gad.atl.diff) / sqrt(n()))

mn_ssb_diff <- 
    ssb_rel_diff %>%
    group_by(model, age) %>%
    summarize(mn.diff = mean(median.diff),
              se.diff = sd(median.diff) / sqrt(n()))


ssbDiffPlot <- 
    ggplot(data=mn_ssb_diff, aes(x=age, y=mn.diff, color = model)) + 
    geom_point(position = position_dodge(width = 0.4)) + 
    geom_errorbar(aes(ymin = mn.diff - se.diff,
                      ymax = mn.diff + se.diff),
                  position = position_dodge(width = 0.4), width = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() + xlab("Age") + ylab("Absolute Difference") + 
    scale_color_manual(name = "Models",
                       breaks = theme_breaks,
                       values = theme_values,
                       labels = theme_labels) +
    theme(legend.text.align = 0)

# function to condense plots with grid.arrange
gl <- ggplotGrob(ssbDiffPlot)$grobs
legend <- gl[[which(sapply(gl, function(x) x$name) == "guide-box")]]   
panel1 <- arrangeGrob(numBiasPlot2 + 
                          theme(legend.position = "none",
                            axis.title.y = element_blank(),
                            axis.title.x = element_blank()),
                      top = textGrob("Numbers"),
                      left = "Relative Bias")
panel2 <- arrangeGrob(ssbBiasPlot2 + theme(legend.position = "none",
                                           axis.title.y = element_blank(),
                                           axis.title.x = element_blank()),
                      top = textGrob("SSB"))
panel3 <- arrangeGrob(numDiffPlot + 
                          theme(legend.position = "none",
                                axis.title.y = element_blank()) + 
                          scale_x_continuous(breaks = seq(0,18,2)),
                      left = "Absolute Difference")
panel4 <- arrangeGrob(ssbDiffPlot + 
                          theme(legend.position = "none",
                                axis.title.y = element_blank()) + 
                          scale_x_continuous(breaks = seq(4,18,2)))
layout <- rbind(c(1,1,2,2,3),
                c(4,4,5,5,3))
biasForestPlots <- 
    arrangeGrob(panel1, panel2, 
                 legend, 
                 panel3, panel4,
                 ncol = 3,
                 layout_matrix = layout)

#--------------------------------------------------------------------------
# boxplots to show variance in model outputs

# boxplot to show model variances across time for each model type
mod_levels <- c("varMod", "bootMod", "halfBoot", "fullBoot")
mod_labels <- c("varMod" = "EEM", 
                "bootMod" = expression("BEM"[0]),
                "halfBoot" = expression("BEM"[0.5]),
                "fullBoot" = expression("BEM"[1]))
modVarPlot <- 
    ggplot(data=filter(res_by_step, year > 1982), 
           aes(x=factor(model, levels = mod_levels), y=total.number/1e6)) + 
    geom_boxplot() + theme_bw() + 
    xlab("\nModel Type") + ylab("Total number (millions)") + 
    scale_x_discrete(labels = mod_labels)


#-------------------------------------------------------------------------
# misc

# total variance for each model type
modVar <- 
    res_by_step %>%
    filter(year > 1982) %>%
    group_by(model) %>%
    summarize(num.var = var(total.number), 
              bm.var = var(total.biomass))
