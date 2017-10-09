# find some decent starting values for recl and stddev

mla <- mfdb_sample_meanlength_stddev(mdb, c('age'),
                                     c(list(sampling_type=c("SprSurvey","AutSurvey"),
                                            age=0:19),
                                       data.defaults))
init.sigma <- 
    mla[[1]] %>% 
    na.omit() %>% 
    group_by(age) %>%
    summarize(ml=mean(mean), ms=mean(stddev, na.rm=T))

atl.init.sigma <- 
    rbind(init.sigma, init.sigma) %>%
    arrange(age)

# lw <- mfdb_sample_meanweight(mdb, c('length'),
#                             c(list(sampling_type=c('SprSurvey','AutSurvey'),
#                                    species='COD',
#                                    length=mfdb_interval("", seq(0,200, by=1)))))
# 
# lw.tmp <-   
#     lw[[1]] %>% 
#     mutate(length=as.numeric(as.character(length)),
#            weight=mean/1e3) %>%
#     na.omit() %>%
#     nls(weight ~ a*length^b,.,start=list(a=0.1,b=3)) %>%
#     coefficients() %>%
#     as.numeric()

## populate the model with starting default values

## alpha and beta for lw relationship
weight.alpha <- 0.0000021
weight.beta <- 3.3437

# ## setup M and determine initial abundance
# source('cod/modelCheck/getAtlantisMort3.R')
# # for m as function of age use m.decay.vals
# for calculated m use m.data
# nat.mort <- round(c(m.data$median.m, 
#                     rep(m.data$median.m[nrow(m.data)], 3)), 
#                   3)
#rc <- 20

# age.mean.formula <- 'exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s'
rec.number <- sprintf('%1$s.rec.scalar*%1$s.rec.%2$s', species.name, year.range)
rec.sd <- sprintf('#%s.rec.sd', species.name)

cod0 <- 
    gadgetstock('cod0', gd$dir, missingOkay=T) %>%
    gadget_update('stock',
                  minage = 0,
                  maxage = 1,
                  minlength = 1,
                  maxlength = 60,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update('doesgrow',
                  growthparameters=c(linf=sprintf('#%s.linf', species.name), 
                                     k=sprintf('#%s.k', species.name),
                                     alpha=weight.alpha,
                                     beta=weight.beta),
                  beta=sprintf('(* 10 #%s.bbin)', species.name)) %>%
    # gadget_update('naturalmortality', # m for each age
    #               sprintf('#%1$s.age%2$s.m', 
    #                       species.name, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update('naturalmortality', # m as a function of age
                  m.estimate.formula(age=.[[1]]$minage:.[[1]]$maxage,
                                     m=sprintf('%s.m.decay', species.name),
                                     max.m=sprintf('%s.max.m', species.name),
                                     min.m=sprintf('%s.min.m', species.name))) %>%
    gadget_update('initialconditions',
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 # age.factor = sprintf('(* 10 #%1$s.init%2$s)',
                                 #                      species.name,
                                 #                      age),
                                 age.factor=init.age.factor(age=age,
                                                            m=sprintf('%s.init.decay', species.name),
                                                            age.scalar=sprintf('%s.init.scalar', species.name),
                                                            init.min=sprintf('%s.init.min', species.name)),
                                 area.factor=sprintf('( * #%1$s.mult #%1$s.init.abund)',
                                                     species.name),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf('%s.linf', species.name),
                                                     k=sprintf('%s.k', species.name),
                                                     recl=sprintf('%s.recl', species.name)),
                                 stddev = init.sigma$ms[1:2],
                                 alpha = weight.alpha,
                                 beta = weight.beta)) %>%
    gadget_update('refweight',
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight.alpha*length^weight.beta)) %>%
    gadget_update('iseaten', 1) %>%
    # gadget_update('doesmature', 
    #               maturityfunction = 'continuous',
    #               maturestocksandratios = sprintf('%smat 1',species_name),
    #               coefficients = sprintf('( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0',
    #                                         species_name)) %>% 
    gadget_update('doesmove',
                  transitionstocksandratios = sprintf('%s 1', species.name),
                  transitionstep = 4) %>%
    gadget_update('doesrenew',
                  normalparam = 
                      data_frame(
                          year = year.range,
                          step = 1,
                          area = 1, 
                          age = .[[1]]$minage,
                          number = parse(text=rec.number) %>%
                              map(to.gadget.formulae) %>%
                              unlist(),
                          mean = vonb_formula(
                              age=.[[1]]$minage,
                              linf=sprintf('%s.linf', species.name),
                              k=sprintf('%s.k', species.name),
                              recl=sprintf('%s.recl', species.name)),
                          stddev = rec.sd,
                          alpha = weight.alpha,
                          beta = weight.beta))



cod <- 
    gadgetstock('cod', gd$dir, missingOkay=T) %>%
    gadget_update('stock',
                  minage = 2,
                  maxage = 19,
                  minlength = 1,
                  maxlength = 199,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update('doesgrow',
                  growthparameters=c(linf=sprintf('#%s.linf', species.name), 
                                     k=sprintf('#%s.k', species.name),
                                     alpha=weight.alpha,
                                     beta=weight.beta),
                  beta=sprintf('(* 10 #%s.bbin)', .[[1]]$stockname)) %>%
    # gadget_update('naturalmortality', # m for each age
    #               sprintf('#%1$s.age%2$s.m', 
    #                       .[[1]]$stockname, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update('naturalmortality', # m as a function of age
                  m.estimate.formula(age=.[[1]]$minage:.[[1]]$maxage,
                                     m=sprintf('%s.m.decay', .[[1]]$stockname),
                                     max.m=sprintf('%s.max.m', .[[1]]$stockname),
                                     min.m=sprintf('%s.min.m', .[[1]]$stockname))) %>%
    gadget_update('initialconditions',
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 # age.factor = sprintf('(* 10 #%1$s.init%2$s)',
                                 #                      .[[1]]$stockname,
                                 #                      age),
                                 age.factor=init.age.factor(age=age,
                                                            m=sprintf('%s.init.decay', .[[1]]$stockname),
                                                            age.scalar=sprintf('%s.init.scalar', .[[1]]$stockname),
                                                            init.min=sprintf('%s.init.min', .[[1]]$stockname)),
                                 area.factor=sprintf('( * #%1$s.mult #%1$s.init.abund)',
                                                     .[[1]]$stockname),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf('%s.linf', species.name),
                                                     k=sprintf('%s.k', species.name),
                                                     recl=sprintf('%s.recl', species.name)),
                                 stddev = c(init.sigma$ms, 
                                            rep(init.sigma$ms[nrow(init.sigma)],
                                                (.[[1]]$maxage-.[[1]]$minage)-(nrow(init.sigma)-1))),
                                 alpha = weight.alpha,
                                 beta = weight.beta)) %>%
    gadget_update('refweight',
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight.alpha*length^weight.beta)) %>%
    gadget_update('iseaten', 1)
    # gadget_update('doesmature', 
    #               maturityfunction = 'continuous',
    #               maturestocksandratios = sprintf('%smat 1',species_name),
    #               coefficients = sprintf('( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0',
    #                                         species_name)) %>% 
    # gadget_update('doesmove',
    #               transitionstocksandratios = sprintf('%s.mat 1', species.name),
    #               transitionstep = 4) %>%
    # gadget_update('doesrenew',
    #               normalparam = 
    #                   data_frame(
    #                       year = year.range,
    #                       step = 1,
    #                       area = 1, 
    #                       age = .[[1]]$minage,
    #                       number = parse(text=rec.number) %>%
    #                                map(to.gadget.formulae) %>%
    #                                unlist(),
    #                       mean = vonb_formula(
    #                                 age=.[[1]]$minage,
    #                                 linf=sprintf('%s.linf', species.name),
    #                                 k=sprintf('%s.k', species.name),
    #                                 recl=sprintf('%s.recl', species.name)),
    #                       stddev = rec.sd,
    #                       alpha = weight.alpha,
    #                       beta = weight.beta))


write.gadget.file(cod0, gd$dir)
write.gadget.file(cod, gd$dir)
