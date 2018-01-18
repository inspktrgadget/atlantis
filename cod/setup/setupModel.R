# find some decent starting values for recl and stddev

mla <- mfdb_sample_meanlength_stddev(mdb, c("age"),
                                     c(list(sampling_type=c("SprSurveyTotals",
                                                            "AutSurveyTotals"),
                                            age=0:19),
                                       data_defaults))
init_sigma <- 
    mla[[1]] %>% 
    na.omit() %>% 
    group_by(age) %>%
    summarize(ml=mean(mean), ms=mean(stddev, na.rm=T))

atl_init_sigma <- 
    rbind(init_sigma, init_sigma) %>%
    arrange(age)

# lw <- mfdb_sample_meanweight(mdb, c("length"),
#                             c(list(sampling_type=c("SprSurvey","AutSurvey"),
#                                    species="COD",
#                                    length=mfdb_interval(, seq(0,200, by=1)))))
# 
# lw_tmp <-   
#     lw[[1]] %>% 
#     mutate(length=as.numeric(as.character(length)),
#            weight=mean/1e3) %>%
#     na.omit() %>%
#     nls(weight ~ a*length^b,.,start=list(a=0.1,b=3)) %>%
#     coefficients() %>%
#     as.numeric()

## populate the model with starting default values

## alpha and beta for lw relationship
weight_alpha <- 0.0000021
weight_beta <- 3.3437

# ## setup M and determine initial abundance
# source("cod/modelCheck/getAtlantisMort3.R")
# # for m as function of age use m_decay_vals
# for calculated m use m_data
# nat_mort <- round(c(m_data$median.m, 
#                     rep(m_data$median.m[nrow(m_data)], 3)), 
#                   3)
#rc <- 20

# age.mean.formula <- "exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s"
rec_number <- sprintf("%1$s.rec.scalar*%1$s.rec.%2$s", species_name, year_range)
rec_sd <- sprintf("#%s.rec.sd", species_name)

st_sigma <- rbind(init_sigma, init_sigma) %>% arrange(age)

# setup age 0-1 stock
cod0 <- 
    gadgetstock("cod0", gd$dir, missingOkay=T) %>%
    gadget_update("stock",
                  minage = 0,
                  maxage = 1,
                  minlength = 1,
                  maxlength = 60,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight_alpha,
                                     beta=weight_beta),
                  beta=sprintf("(* 100 #%s.bbin)", species_name)) %>%
    # gadget_update("naturalmortality", 
    #               rep(0.2, ((.[[1]]$maxage - .[[1]]$minage) + 1))) %>%
    # gadget_update("naturalmortality", # m for each age
    #               sprintf("#%1$s.age%2$s.m", 
    #                       .[[1]]$species_name, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update("naturalmortality", # m as a function of age
                  m_estimate_formula(max_m=sprintf("%s.max.m", species_name),
                                     min_m=sprintf("%s.min.m", species_name),
                                     m_decay=sprintf("%s.m.decay", species_name),
                                     age=.[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update("initialconditions",
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor=init_age_factor(init_max=sprintf("%s.init.max", 
                                                                             species_name),
                                                            init_min=sprintf("%s.init.min", 
                                                                             species_name),
                                                            init_decay=sprintf("%s.init.decay", 
                                                                               species_name),
                                                            age=age),
                                 area.factor=sprintf("( * #%1$s.mult #%1$s.init.scalar)",
                                                      species_name),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf("%s.linf", species_name),
                                                     k=sprintf("%s.k", species_name),
                                                     recl=sprintf("%s.recl", species_name)),
                                 stddev = st_sigma$ms[1:2],
                                 alpha = weight_alpha,
                                 beta = weight_beta)) %>%
    gadget_update("refweight",
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight_alpha*length^weight_beta)) %>%
    gadget_update("iseaten", 0) %>%
    # gadget_update("doesmature", 
    #               maturityfunction = "continuous",
    #               maturestocksandratios = sprintf("%smat 1",species_name),
    #               coefficients = sprintf("( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0",
    #                                         species_name)) %>% 
    gadget_update("doesmove",
                  transitionstocksandratios = c(stock, 1),
                  transitionstep = 4) %>%
    gadget_update("doesrenew",
                  normalparam =
                      data_frame(
                          year = year_range,
                          step = 1,
                          area = 1,
                          age = .[[1]]$minage,
                          number = parse(text=rec_number) %>%
                              map(to.gadget.formulae) %>%
                              unlist(),
                          mean = vonb_formula(
                              age=.[[1]]$minage,
                              linf=sprintf("%s.linf", species_name),
                              k=sprintf("%s.k", species_name),
                              recl=sprintf("%s.recl", species_name)),
                          stddev = rec_sd,
                          alpha = weight_alpha,
                          beta = weight_beta))

# pre-define some paramater file arguments
stock_minage <- 2
stock_maxage <- 12
stock_m <- m_estimate_formula(max_m=sprintf("%s.max.m", species_name),
                              min_m=sprintf("%s.min.m", species_name),
                              m_decay=sprintf("%s.m.decay", species_name),
                              age=stock_minage:stock_maxage)
stock_age_factor <- init_age_factor(init_max=sprintf("%s.init.max", species_name),
                                    init_min=sprintf("%s.init.min", species_name),
                                    init_decay=sprintf("%s.init.decay", species_name),
                                    age=stock_minage:stock_maxage)
stock_area_factor <- sprintf("( * #%1$s.init.mult #%1$s.init.scalar)",
                             species_name)
stock_meanlength <- vonb_formula(linf=sprintf("%s.linf", species_name),
                                 k=sprintf("%s.k", species_name),
                                 recl=sprintf("%s.recl", species_name),
                                 age = stock_minage:stock_maxage)
stock_sd <- c(init_sigma$ms[seq(stock_minage+1,
                             ifelse(stock_maxage >= nrow(init_sigma),
                                    nrow(init_sigma),
                                    stock_maxage+1))])
age_range <- stock_maxage - stock_minage + 1
if (length(stock_sd) < age_range) {
    stock_sd <- 
        c(stock_sd,
          rep(stock_sd[length(stock_sd)], age_range - length(stock_sd)))
}


# setup stock file using above parameters
cod <- 
    gadgetstock("cod", gd$dir, missingOkay=T) %>%
    gadget_update("stock",
                  minage = stock_minage,
                  maxage = stock_maxage,
                  minlength = 1,
                  maxlength = 199,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight_alpha,
                                     beta=weight_beta),
                  beta=sprintf("(* #%1$s.bbin.mult #%1$s.bbin)", .[[1]]$stockname)) %>%
    # gadget_update(naturalmortality, 
    #               rep(0.2, ((.[[1]]$maxage - .[[1]]$minage) + 1))) %>%
    # gadget_update("naturalmortality", # m for each age
    #               sprintf("#%1$s.age%2$s.m", 
    #                       .[[1]]$stockname, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update("naturalmortality", # m as a function of age
                  stock_m) %>%
    gadget_update("initialconditions",
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor = stock_age_factor,
                                 area.factor=stock_area_factor,
                                 mean = stock_meanlength,
                                 stddev = stock_sd,
                                 alpha = weight_alpha,
                                 beta = weight_beta)) %>%
    gadget_update("refweight",
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight_alpha*length^weight_beta)) %>%
    gadget_update("iseaten", 1)
    # gadget_update("doesmature", 
    #               maturityfunction = "continuous",
    #               maturestocksandratios = sprintf("%smat 1",species_name),
    #               coefficients = sprintf("( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0",
    #                                         species_name)) %>% 
    # gadget_update("doesmove",
    #               transitionstocksandratios = sprintf("%s.mat 1", species_name),
    #               transitionstep = 4) %>%
    # gadget_update("doesrenew",
    #           normalparam =
    #               data_frame(
    #                   year = year_range,
    #                   step = 1,
    #                   area = 1,
    #                   age = .[[1]]$minage,
    #                   number = parse(text=rec_number) %>%
    #                            map(to.gadget.formulae) %>%
    #                            unlist(),
    #                   mean = vonb_formula(
    #                             age=.[[1]]$minage,
    #                             linf=sprintf("%s.linf", species_name),
    #                             k=sprintf("%s.k", species_name),
    #                             recl=sprintf("%s.recl", species_name)),
    #                   stddev = rec_sd,
    #                   alpha = weight_alpha,
    #                   beta = weight_beta))


write.gadget.file(cod0, gd$dir)
write.gadget.file(cod, gd$dir)
