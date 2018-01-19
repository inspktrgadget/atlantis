# analyzing the output from final gadget run from using gadget.iterative in run.R

#source("~/R/rgadget/trunk/gadgetFileIO.R") ## gadget.fit function doesn"t work when this is read in
#source("~/R/rgadget/trunk/gadgetfunctions.R")
#source("~/R/rgadget/trunk/gadgetClass.R")
#source("~/R/rgadget/trunk/gadgetMethods.R")
#source("~/R/rgadget/trunk/function.R")

library(plyr)
library(tidyverse)
library(grid)
library(Rgadget)
setwd("~/gadget/models/atlantis/cod/codSpawn/codSpawnModel")
fit <- gadget.fit(wgts="WGTS", main.file="WGTS/main.final",
                  fleet.predict = data.frame(fleet = "comm", ratio=1),
                  mat.par=c(-6.510198, 1.108594),
                  printfile.printatstart = 0,
                  printfile.steps = "all",
                  rec.len.param = TRUE)

# source("~/R/rgadget/trunk/gadgetFileIO.R")
# source("~/R/rgadget/trunk/gadgetfunctions.R")
# source("~/R/rgadget/trunk/gadgetClass.R")
# source("~/R/rgadget/trunk/gadgetMethods.R")
# source("~/R/rgadget/trunk/function.R")
# 
# gssForward <-
#     gadget.forward(years=6,params.file="WGTS/params.final",
#                    stochastic=FALSE,
#                    num.trials=1,
#                    effort=0.2)

## fit statistics
resTable <- fit$resTable[tail(head(names(fit$resTable),-2),-1)]

summary_plot <-
    ggplot(filter(fit$likelihoodsummary, year != "all"),
           aes(as.numeric(year), likelihood.value)) +
    geom_point() + facet_wrap(~component, scales="free_y") +theme_bw()+
    xlab("Year") + ylab("Score")


## to calculate biomass index
tmp <- mutate(fit$sidat, survey = ifelse(substr(name,1,3)=="aut","aut", "igfs"))
tmp <- rbind.fill(tmp,
                  ddply(tmp,~year+survey, summarise,
                        observed = sum(observed*0.008249352*lower^3.026918 ),
                        predict = sum(predict*0.008249352*lower^3.026918 ),
                        upper = sum(upper*0.008249352*lower^3.026918 ),
                        lower = sum(lower*0.008249352*lower^3.026918 ),
                        length = "Biomass"))

# plot the model survey data over the actual survey data
si_spr <-
    ggplot(subset(tmp, survey=="igfs"), aes(year,observed)) +
    geom_point() +
    geom_line(aes(year,predict)) +
    geom_linerange(data=subset(tmp,year==max(year)),
                   aes(year,ymax=observed,ymin=predict),col="green")+
    geom_text(data=mutate(subset(tmp,year==min(year)),y=Inf),
              aes(year,y,label=length), vjust = 2,hjust = -1)+
    facet_wrap(~length,scale="free_y",ncol=2) + theme_bw() +
    ylab("Index") + xlab("Year") +
    theme (panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())

si_aut <-
    ggplot(filter(tmp, survey=="aut"), aes(year,observed)) +
    geom_point() +
    geom_line(aes(year,predict)) +
    geom_linerange(data=subset(tmp,year==max(year)),
                   aes(year,ymax=observed,ymin=predict),col="green")+
    geom_text(data=mutate(subset(tmp,year==min(year)),y=Inf),
              aes(year,y,label=length), vjust = 2,hjust = -1)+
    facet_wrap(~length,scale="free_y",ncol=2) + theme_bw() +
    ylab("Index") + xlab("Year") +
    theme (panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot the survey length-distribution data over the actual survey length-distribution data
ldist_spr <-
    ggplot(subset(fit$catchdist.fleets,name == "ldist_spr") ,
           aes(lower,predicted)) + geom_line() +
    geom_line(aes(lower,observed), color="gray") +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == "ldist_spr" & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab("Proportion") + xlab("Length (cm)") + ggtitle("Spring Lengths") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank()) 

ldist_aut <-
    ggplot(subset(fit$catchdist.fleets,name == "ldist_aut") ,
           aes(lower,predicted)) + geom_line() +
    geom_line(aes(lower,observed),col="gray") +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == "ldist_aut" & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab("Proportion") + xlab("Length (cm)") + ggtitle("Autumn Lengths") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot the model catchdistribution data over actual catchdistribution data
ldist_catch <-
    ggplot(subset(fit$catchdist.fleets,name == "ldist_comm"),
           aes(lower,predicted)) +
    geom_line(aes(lower,observed),col="gray") +
    facet_wrap(~year+step) + theme_bw() + geom_line() +
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == "ldist_comm" & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab("Proportion") + xlab("Length (cm)") + ggtitle("Catch Lengths") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())

ldist_discards <-
    ggplot(subset(fit$catchdist.fleets,name == "ldist_discards"),
           aes(lower,predicted)) +
    geom_line(aes(lower,observed),col="gray") +
    facet_wrap(~year+step) + theme_bw() + geom_line() +
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == "ldist_discards" & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab("Proportion") + xlab("Length (cm)") + ggtitle("Discard Lengths") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())

ages <- 
    fit$catchdist.fleets %>%
    filter(name %in% c("aldist_spr", "aldist_aut", "aldist_comm")) %>%
    group_by(name, year, step, age) %>%
    summarize(observed = sum(observed, na.rm=T),
              predicted = sum(predicted, na.rm=T)) %>%
    mutate(age = as.numeric(gsub("age", "", age)))

aldist_spr <-
    ggplot(data=filter(ages, name=="aldist_spr"), aes(x=age, y=predicted)) + 
    geom_line() + geom_line(aes(x=age, y=observed), color="lightgray") +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=filter(ages, name == "aldist_spr" & age == max(age)),
              aes(x=max(age)/(4/3), y=Inf, label=year), vjust = 1.5) +
    ylab("Proportion") + xlab("Age") + ggtitle("Spring Ages") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank()) 

aldist_aut <-
    ggplot(data=filter(ages, name=="aldist_aut"), aes(x=age, y=predicted)) + 
    geom_line() + geom_line(aes(x=age, y=observed), color="lightgray") +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=filter(ages, name == "aldist_aut" & age == max(age)),
              aes(x=max(age)/(4/3), y=Inf, label=year), vjust = 1.5) +
    ylab("Proportion") + xlab("Age") + ggtitle("Autumn Ages") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())

aldist_catch <-
    ggplot(data=filter(ages, name=="aldist_comm"), aes(x=age, y=predicted)) + 
    geom_line() + geom_line(aes(x=age, y=observed), color="lightgray") +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=filter(ages, name == "aldist_comm" & age == max(age)),
              aes(x=max(age)/(4/3), y=Inf, label=year), vjust = 1.5) +
    ylab("Proportion") + xlab("Age") + ggtitle("Catch Ages") +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank()) 



# plot suitability against length for both survey and commercial fleets
selection_plot <-
    ggplot(filter(fit$suitability, suit > 0),
           aes(x=length,y=suit,lty=fleet)) +
    geom_line() +
    theme_bw() + ylab("Suitability") + xlab("Length") +
    theme(legend.position = c(0.8,0.25), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm")) 


# plot growth curve from model
gr_plot <-
    ggplot(fit$stock.growth,
           aes(age,length)) + 
    geom_line() +
    theme_bw() + ylab("Length") + xlab("Age") +
    theme(legend.position = c(0.9,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"))


# plot spawning function
mu <- fit$params$value[grep("bh.mu", fit$params$switch)]
lam <- fit$params$value[grep("bh.lam", fit$params$switch)]
bevHolt <- function(mu, lambda, x) {return((mu * x) / (lambda + x))}
bevHoltAtl <- function(mu, lambda, x) {
    lam <- ((lambda / 0.267) * 20 * 5.7 / 1e6)
    return((mu * x) / (lam + x))}
spawn_plot <-
    ggplot(data=fit$res.by.year, aes(x=ssb)) + 
    stat_function(fun = bevHolt, args = list(mu = mu, lambda = lam)) + 
    stat_function(fun = bevHoltAtl, 
                  args = list(mu = 4e8, lambda = 2.5e11), 
                  color = "red", linetype = "dashed") +
    theme_bw() + xlab("SSB") + ylab("Recruitment")

# plotting the catch by year
catch_plot <- 
    ggplot(fit$res.by.year,aes(year,catch/1000)) +
    geom_bar(stat="identity") +
    ylab("Catches (in tons)") + xlab("Year") +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"))


# plotting the biomass by year
biomass_plot <- 
    ggplot(fit$res.by.year,aes(year,total.biomass/1000)) +
    geom_bar(stat="identity") +
    ylab("Total biomass (in tons)") + xlab("Year") +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm")) #+
#facet_wrap(~stock, scales="free_y")


# plotting the harvest per year
harv_plot <- 
    ggplot(fit$res.by.year,aes(year,harv.biomass/1000)) +
    geom_bar(stat="identity") +
    ylab("Harvestable biomass (in tons)") + xlab("Year") +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm")) #+
#facet_wrap(~stock, scales="free_y")


# plot sustainable stock biomass per year
ssb_plot <- 
    ggplot(fit$res.by.year,aes(year,ssb/1000)) +
    geom_bar(stat="identity") +
    ylab("SSB (in tons)") + xlab("Year") +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"))

f_plot <- 
    ggplot(filter(fit$res.by.year, stock == "cod"), aes(year, F)) + 
    geom_line() + 
    ylab("F") + xlab("Year") +  theme_bw() +
    theme(legend.position=c(0.2, 0.8), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm"))

## basic plots to check parameters, initial values, and
## natural mortality (if estimated)
init_vals <- 
    ggplot(filter(fit$stock.std, year == min(year)), 
           aes(x=age, y=number/1e6)) +
    geom_bar(stat="identity") +
    xlab("Age") + ylab("Numbers (millions)") + theme_bw()

params_plot <- 
    ggplot(data=filter(fit$params, optimise == 1),
           aes(x=switch, y=value)) + geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.1) + 
    facet_wrap(~switch, scales="free") + theme_bw() +
    theme (axis.ticks.y = element_blank(),
           panel.spacing = unit(0,"cm"), plot.margin = unit(c(0,0,0,0),"cm"),
           strip.background = element_blank(), strip.text.x = element_blank())


## plots if formulas are used for estimation

# plot for natural mortality if m.estimate.formula is used
nat_m_plot <- 
    ggplot(data=data.frame(age=unique(fit$stock.std$age)),
                           aes(x=age)) + 
    stat_function(fun=function(x, nat.m, max.m, min.m) {
                exp((-1)*nat.m*x)*(max.m-min.m) + min.m
                }, args=list(nat.m=fit$params$value[grep("m.decay", 
                                                        fit$params$switch)],
                            max.m=fit$params$value[grep("max.m", 
                                                        fit$params$switch)],
                            min.m=fit$params$value[grep("min.m",
                                                         fit$params$switch)]))

# plot for initial values if estimated as a function of age
age_factor <- function(init_max, init_min, init_decay, age) {
    return((exp((-1) * init_decay * age) * (init_max - init_min) + init_min))
}
area_factor <- function(mult, scalar) {mult * scalar}
init_abund <- function(init_max, init_min, init_decay, age, mult, scalar) {
    age_factor(init_max, init_min, init_decay, age) * area_factor(mult, scalar) * 1e4
}
init_max <- fit$params$value[grep("init.scalar", fit$params$switch)]
init_min <- fit$params$value[grep("init.min", fit$params$switch)]
init_decay <- fit$params$value[grep("init.decay", fit$params$switch)]
mult <- fit$params$value[grep("mult", fit$params$switch)]
scalar <- fit$params$value[grep("init.abund", fit$params$switch)]

init_plot <- 
    ggplot(data=data.frame(age=unique(fit$stock.std$age)),
           aes(x=age)) + 
    stat_function(fun = init_abund,
                  args = list(init_max = init_max,
                              init_min = init_min,
                              init_decay = init_decay,
                              mult = mult,
                              scalar = scalar)) + 
    stat_function(fun=function(x, init.m, init.scalar) {
                exp((-1)*init.m*x)*init.scalar
            }, args=list(init.m=fit$params$value[grep("init.m", 
                                                     fit$params$switch)],
                         init.scalar=fit$params$value[grep("init.scalar", 
                                                     fit$params$switch)]))