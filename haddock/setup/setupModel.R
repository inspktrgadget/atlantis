# find some decent starting values for recl and stddev

mla <- mfdb_sample_meanlength_stddev(mdb, c('age'),
                                     c(list(sampling_type=c("SprSurvey","AutSurvey"),
                                            age=0:19),
                                       defaults))
init.sigma <- 
    mla[[1]] %>% 
    na.omit() %>% 
    group_by(age) %>%
    summarize(ml=mean(mean), ms=mean(stddev, na.rm=T))

atl.init.sigma <- 
    rbind(init.sigma, init.sigma) %>%
    arrange(age)

lw <- mfdb_sample_meanweight(mdb, c('length'),
                            c(list(sampling_type=c('SprSurvey','AutSurvey'),
                                   species='HAD',
                                   length=mfdb_interval("", seq(0,120, by=1)))))

lw.tmp <-   
    lw[[1]] %>% 
    mutate(length=as.numeric(as.character(length)),
           weight=mean/1e3) %>%
    na.omit() %>%
    nls(weight ~ a*length^b,.,start=list(a=0.1,b=3)) %>%
    coefficients() %>%
    as.numeric()

## populate the model with starting default values

## alpha and beta for lw relationship
weight.alpha <- 0.00000587903
weight.beta <- 3.116172

## setup M and determine initial abundance
source('haddock/modelCheck/getAtlantisMort.R')
nat.mort <- round(m.data$m, 3)
#rc <- 20

# age.mean.formula <- 'exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s'
rec.number <- sprintf('%1$s.rec.scalar*%1$s.rec.%2$s', species.name, year.range)
rec.sd <- sprintf('#%s.rec.sd', species.name)
vonb <- von_b_formula(age, 
                      linf=sprintf('%s.linf', species.name), 
                      k=sprintf('%s.k',species.name), 
                      recl=sprintf('%s.recl', species.name))

## set up the one stock stock
had <- 
    gadgetstock('had', gd$dir, missingOkay=T) %>%
    gadget_update('stock',
                  minage = 0,
                  maxage = 19,
                  minlength = 1,
                  maxlength = 120,
                  dl = 1,
                  livesonareas = 1) %>%
    gadget_update('doesgrow',
                  growthparameters=c(linf=sprintf('#%s.linf', species.name), 
                                     k=sprintf('#%s.k', species.name),
                                     alpha=weight.alpha,
                                     beta=weight.beta),
                  beta=to.gadget.formulae(quote(10*had.bbin))) %>%
    gadget_update('naturalmortality', c(nat.mort, nat.mort[length(nat.mort)])) %>%
    gadget_update('initialconditions',
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor=sprintf('#%2$s.age%1$s', age,
                                                    .[[1]]$stockname),
                                 area.factor=sprintf('( * #%1$s.mult #%1$s.init.abund)',
                                                     .[[1]]$stockname),
                                 mean = vonb,
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
    gadget_update('iseaten', 1) %>%
    # gadget_update('doesmature', 
    #               maturityfunction = 'continuous',
    #               maturestocksandratios = sprintf('%smat 1',species_name),
    #               coefficients = sprintf('( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0',
    #                                         species_name)) %>% 
    # gadget_update('doesmove',
    #               transitionstocksandratios = sprintf('%s.mat 1', species.name),
    #               transitionstep = 4) %>%
    gadget_update('doesrenew',
                  normalparam = data_frame(year = year.range,
                                           step = 1,
                                           area = 1, 
                                           age = .[[1]]$minage,
                                           number = parse(text=rec.number) %>%
                                               map(to.gadget.formulae) %>%
                                               unlist(),
                                           mean = vonb,
                                           stddev = rec.sd,
                                           alpha = weight.alpha,
                                           beta = weight.beta))



write.gadget.file(had, gd$dir)
