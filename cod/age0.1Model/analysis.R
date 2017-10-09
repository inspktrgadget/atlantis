# analyzing the output from final gadget run from using gadget.iterative in run.R

#source('~/R/rgadget/trunk/gadgetFileIO.R') ## gadget.fit function doesn't work when this is read in
#source('~/R/rgadget/trunk/gadgetfunctions.R')
#source('~/R/rgadget/trunk/gadgetClass.R')
#source('~/R/rgadget/trunk/gadgetMethods.R')
#source('~/R/rgadget/trunk/function.R')

library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(Rgadget)
setwd('~/gadget/models/atlantis/cod/age0.1Model')
fit <- gadget.fit(wgts="WGTS", main.file='WGTS/main.final',
                  fleet.predict = data.frame(fleet = 'bmt.comm', ratio=1),
                  mat.par=c(-6.510198, 1.108594))

# source('~/R/rgadget/trunk/gadgetFileIO.R')
# source('~/R/rgadget/trunk/gadgetfunctions.R')
# source('~/R/rgadget/trunk/gadgetClass.R')
# source('~/R/rgadget/trunk/gadgetMethods.R')
# source('~/R/rgadget/trunk/function.R')
# 
# gssForward <-
#     gadget.forward(years=6,params.file='WGTS/params.final',
#                    stochastic=FALSE,
#                    num.trials=1,
#                    effort=0.2)

## fit statistics
resTable <- fit$resTable[tail(head(names(fit$resTable),-2),-1)]

summary.plot <-
    ggplot(filter(fit$likelihoodsummary, year != 'all'),
           aes(as.numeric(year), likelihood.value)) +
    geom_point() + facet_wrap(~component, scales="free_y") +theme_bw()+
    xlab('Year') + ylab('Score')


## to calculate biomass index
tmp <- mutate(fit$sidat, survey = ifelse(substr(name,1,3)=='aut','aut', 'igfs'))
tmp <- rbind.fill(tmp,
                  ddply(tmp,~year+survey, summarise,
                        number.x = sum(number.x*0.008249352*lower^3.026918 ),
                        predict = sum(predict*0.008249352*lower^3.026918 ),
                        upper = sum(upper*0.008249352*lower^3.026918 ),
                        lower = sum(lower*0.008249352*lower^3.026918 ),
                        length = 'Biomass'))

# plot the model survey data over the actual survey data
si.fit.spr.survey <-
    ggplot(subset(tmp, survey=='igfs'), aes(year,number.x)) +
    geom_point() +
    geom_line(aes(year,predict)) +
    geom_linerange(data=subset(tmp,year==max(year)),
                   aes(year,ymax=number.x,ymin=predict),col='green')+
    geom_text(data=mutate(subset(tmp,year==min(year)),y=Inf),
              aes(year,y,label=length), vjust = 2,hjust = -1)+
    facet_wrap(~length,scale='free_y',ncol=2) + theme_bw() +
    ylab('Index') + xlab('Year') +
    theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())

si.fit.aut.survey <-
    ggplot(filter(tmp, survey=='aut'), aes(year,number.x)) +
    geom_point() +
    geom_line(aes(year,predict)) +
    geom_linerange(data=subset(tmp,year==max(year)),
                   aes(year,ymax=number.x,ymin=predict),col='green')+
    geom_text(data=mutate(subset(tmp,year==min(year)),y=Inf),
              aes(year,y,label=length), vjust = 2,hjust = -1)+
    facet_wrap(~length,scale='free_y',ncol=2) + theme_bw() +
    ylab('Index') + xlab('Year') +
    theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot the survey length-distribution data over the actual survey length-distribution data
ldist.fit.spr.survey <-
    ggplot(subset(fit$catchdist.fleets,name == 'ldist.spr') ,
           aes(lower,predicted)) + geom_line() +
    geom_line(aes(lower,observed),col='gray') +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == 'ldist.spr' & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab('Proportion') + xlab('length') +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank()) 

ldist.fit.aut.survey <-
    ggplot(subset(fit$catchdist.fleets,name == 'ldist.aut') ,
           aes(lower,predicted)) + geom_line() +
    geom_line(aes(lower,observed),col='gray') +
    facet_wrap(~year+step) + theme_bw() + 
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == 'ldist.aut' & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab('Proportion') + xlab('length') +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot the model catchdistribution data over actual catchdistribution data
ldist.fit.catch <-
    ggplot(subset(fit$catchdist.fleets,name == 'ldist.bmt'),
           aes(lower,predicted)) +
    geom_line(aes(lower,observed),col='gray') +
    facet_wrap(~year+step) + theme_bw() + geom_line() +
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == 'ldist.bmt' & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab('Proportion') + xlab('length') +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot suitability against length for both survey and commercial fleets
selection.plot <-
    ggplot(fit$suitability,
           aes(l,suit,lty=fleet)) +
    geom_line() +
    theme_bw() + ylab('Suitability') + xlab('Length') +
    theme(legend.position = c(0.8,0.25), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm')) 


# plot growth curve from model
gr.plot <-
    ggplot(fit$stock.growth,
           aes(age,length)) + 
    geom_line() +
    theme_bw() + ylab('Length') + xlab('Age') +
    theme(legend.position = c(0.9,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plot recruitment of stock by year
rec.plot <-
    ggplot(fit$res.by.year,aes(year,recruitment/1e6)) +
    geom_bar(stat='identity') +
    ylab("Recruitment (in millions)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm')) 

# plotting the catch by year
catch.plot <- 
ggplot(fit$res.by.year,aes(year,catch/1000)) +
    geom_bar(stat='identity') +
    ylab("Catches (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plotting the biomass by year
biomass.plot <- 
    ggplot(fit$res.by.year,aes(year,total.biomass/1000)) +
    geom_bar(stat='identity') +
    ylab("Total biomass (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm')) #+
    #facet_wrap(~stock, scales="free_y")


# plotting the harvest per year
harv.plot <- 
    ggplot(fit$res.by.year,aes(year,harv.biomass/1000)) +
    geom_bar(stat='identity') +
    ylab("Harvestable biomass (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm')) #+
    #facet_wrap(~stock, scales="free_y")


# plot sustainable stock biomass per year
ssb.plot <- 
    ggplot(fit$res.by.year,aes(year,ssb/1000)) +
    geom_bar(stat='identity') +
    ylab("SSB (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))

f.plot <- 
    ggplot(fit$res.by.year, aes(year, F)) + 
    geom_line() + 
    ylab("F") + xlab("Year") +  theme_bw() +
    theme(legend.position=c(0.2, 0.8), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))

# mig.params <- 
#     read.gadget.parameters('WGTS/params.final') %>%
#     filter(grepl('mig', switch)) %>%
#     mutate(year = as.numeric(gsub('gss.mig', '', switch)))
# mig.plot <- 
#     ggplot(data=mig.params, aes(x=year, y=value)) +
#     geom_line() + theme_bw()
    

##################################################################################
## plots from gadget.forward
##################################################################################

progn.ssb <-
    gssForward$lw %>%
    filter(step == 1) %>%
    group_by(year) %>%
    summarise(ssb = sum(weight*logit(mat.par[1],
                                     mat.par[2],length)*
                            number),
              total.biomass = sum(number*weight))#,

progn.by.year <-
    left_join(prognFmax$catch %>%
                  group_by(year) %>%
                  summarise(catch=sum(biomass.consumed)),
              progn.ssb)


prog.bio.plot <-
    ggplot(progn.by.year,aes(year,ssb/1e6)) +
    geom_rect(aes(xmin=max(fit$res.by.year$year),
                  xmax=Inf,ymin=-Inf,ymax=Inf),
              fill = 'gray90', alpha=0.1) +
    geom_line() +
    theme_bw() + xlab('Year') + ylab('SSB (\'000 tons)') +
    theme(plot.margin = unit(c(0,0,0,0),'cm'),
          legend.title = element_blank(),
          legend.position = c(0.2,0.7))


prog.catch.plot <-
    ggplot(progn.by.year,aes(year,catch/1000)) +
    geom_rect(aes(xmin=max(fit$res.by.year$year),
                  xmax=Inf,ymin=-Inf,ymax=Inf),
              fill = 'gray90', alpha=0.1) +
    geom_line() +
    theme_bw() + xlab('Year') + ylab('Catch (\'000 tons)') +
    theme(plot.margin = unit(c(0,0,0,0),'cm'),
          legend.title = element_blank(),
          legend.position = c(0.2,0.7)) +
    ylim(c(0,max(fit$res.by.year$catch/1000)))

prog.rec.plot <- rec.plot + geom_bar(aes(year,10*recruitment),
                                     data=gssForward$recruitment,
                                     fill='red',stat='identity')













