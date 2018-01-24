# find some decent starting values for recl and stddev

mla <- mfdb_sample_meanlength_stddev(mdb, c("age"),
                                     c(list(sampling_type=c("SprSurveyTotals",
                                                            "AutSurveyTotals"),
                                            age=0:19),
                                       model_defaults))
init_sigma <- 
    mla[[1]] %>% 
    na.omit() %>% 
    group_by(age) %>%
    summarize(ml=mean(mean), ms=mean(stddev, na.rm=T))

atl_init_sigma <- 
    rbind(init_sigma, init_sigma) %>%
    arrange(age)

## populate the model with starting default values

## alpha and beta for lw relationship
weight_alpha <- 0.0000021
weight_beta <- 3.3437

# setup some stockfile parameters for the young, unfished stock
stock0_minage <- 0
stock0_maxage <- 1
stock0_age_range <- stock0_minage:stock0_maxage
stock0_init_age <- 
    init_age_factor(init_max=sprintf("%s.init.max", species_name),
                    init_min=sprintf("%s.init.min", species_name),
                    init_decay=sprintf("%s.init.decay", species_name),
                    age=stock0_age_range)
stock0_m <- 
    m_estimate_formula(max_m=sprintf("%s.max.m", species_name),
                       min_m=sprintf("%s.min.m", species_name),
                       m_decay=sprintf("%s.m.decay", species_name),
                       age=stock0_minage:stock0_maxage)
stock0_meanlength <- 
    vonb_formula(linf=sprintf("%s.linf", species_name),
                 k=sprintf("%s.k", species_name),
                 recl=sprintf("%s.recl", species_name),
                 age = stock0_age_range)
stock0_length_sd <- init_sigma$ms[c(((stock0_minage)+1):((stock0_maxage)+1))]



# age_mean_formula <- "exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s"
rec_number <- sprintf("%1$s.rec.scalar*%1$s.rec.%2$s", species_name, year.range)
rec_sd <- sprintf("#%s.rec.sd", species_name)

cod0 <- 
    gadgetstock("cod0", gd$dir, missingOkay=T) %>%
    gadget_update("stock",
                  minage = stock0_minage,
                  maxage = stock0_maxage,
                  minlength = 1,
                  maxlength = 60,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight.alpha,
                                     beta=weight.beta),
                  beta=sprintf("(* #%1$s.bbin.mult #%1$s.bbin)", .[[1]]$stockname)) %>%
    # gadget_update("naturalmortality", # estimate m for each age
    #               sprintf("#%1$s.age%2$s.m", 
    #                       species_name, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update("naturalmortality", # m as a function of age
                  m_estimate_formula(age=.[[1]]$minage:.[[1]]$maxage,
                                     m=sprintf("%s.m.decay", species_name),
                                     max.m=sprintf("%s.max.m", species_name),
                                     min.m=sprintf("%s.min.m", species_name))) %>%
    gadget_update("initialconditions",
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 # age.factor = sprintf("(* 10 #%1$s.init%2$s)",
                                 #                      species_name,
                                 #                      age),
                                 age.factor=init_age_factor(age=age,
                                                            m=sprintf("%s.init.decay", 
                                                                      species_name),
                                                            age.scalar=sprintf("%s.init.scalar", 
                                                                               species_name),
                                                            init.min=sprintf("%s.init.min", 
                                                                             species_name)),
                                 area.factor=sprintf("( * #%1$s.mult #%1$s.init.abund)",
                                                     species_name),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf("%s.linf", species_name),
                                                     k=sprintf("%s.k", species_name),
                                                     recl=sprintf("%s.recl", species_name)),
                                 stddev = init.sigma$ms[1:2],
                                 alpha = weight.alpha,
                                 beta = weight.beta)) %>%
    gadget_update("refweight",
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight.alpha*length^weight.beta)) %>%
    gadget_update("iseaten", 1) %>%
    # gadget_update("doesmature", 
    #               maturityfunction = "continuous",
    #               maturestocksandratios = sprintf("%smat 1",species_name),
    #               coefficients = sprintf("( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0",
    #                                         species_name)) %>% 
    gadget_update("doesmove",
                  transitionstocksandratios = sprintf("%s 1", species_name),
                  transitionstep = 4)

# setup some stockfile parameters for fished stock
stock_minage <- 2
stock_maxage <- 12
stock_age_range <- stock_minage:stock_maxage
stock_init_age <- 
    init_age_factor(init_max=sprintf("%s.init.max", species_name),
                    init_min=sprintf("%s.init.min", species_name),
                    init_decay=sprintf("%s.init.decay", species_name),
                    age=stock_age_range)
stock_m <- 
    m_estimate_formula(max_m=sprintf("%s.max.m", .[[1]]$stockname),
                       min_m=sprintf("%s.min.m", .[[1]]$stockname),
                       m_decay=sprintf("%s.m.decay", .[[1]]$stockname),
                       age=.[[1]]$minage:.[[1]]$maxage)
stock_meanlength <- 
    vonb_formula(linf=sprintf("%s.linf", species_name),
                 k=sprintf("%s.k", species_name),
                 recl=sprintf("%s.recl", species_name),
                 age = stock_age_range)
stock_length_sd <- 
    init_sigma$ms[c(((stock_minage)+1):((stock_maxage)+1),
                    rep(stock_maxage, 4))]

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
                                 age.factor=stock_init_age,
                                 area.factor=sprintf("( * #%1$s.mult #%1$s.init.abund)",
                                                     .[[1]]$stockname),
                                 mean = stock_meanlength,
                                 stddev = stock_length_sd,
                                 alpha = weight_alpha,
                                 beta = weight_beta)) %>%
    gadget_update("refweight",
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight_alpha*length^weight_beta)) %>%
    gadget_update("iseaten", 1) %>%
    gadget_update("doesspawn",
                  spawnfile = eval(spawnfile(., st_year, end_year,
                                             proportionfunction = paste("exponential",
                                                                        "#cod.spawn.alpha",
                                                                        "#cod.spawn.l50"))))


write.gadget.fit(cod0, gd$dir)
write.gadget.file(cod, gd$dir)


# ## create and write out spawnfile
# spawnfile <- list(
#     spawnsteps="spawnsteps 1",
#     spawnareas="spawnareas 1",
#     firstspawnyear= sprintf("firstspawnyear %s", st_year),
#     lastspawnyear=sprintf("lastspawnyear %s", end_year),
#     spawnstocksandratios=sprintf("spawnstocksandratios\t%s\t1",
#                                  species_name),
#     proportionfunction="proportionfunction\tconstant\t1",
#     mortalityfunction="mortalityfunction\tconstant\t0",
#     weightlossfunction="weightlossfunction\tconstant\t0",
#     recruitment = 
#         sprintf("recruitment\tbevertonholt\t#%1$s.bh.mu\t#%1$s.bh.lam",
#                 species_name),
#     stockparameters = paste("stockparameters",
#                             vonb_formula(age=0,
#                                          linf=sprintf("%s.linf", species_name),
#                                          k=sprintf("%s.k", species_name),
#                                          recl=sprintf("%s.recl", species_name)),
#                             sprintf("#%s.rec.sd", species_name),
#                             weight_alpha,
#                             weight_beta,
#                             sep="\t")
#     
# )
# 
# cat(sapply(spawnfile, toString),
#     file=paste(getwd(), gd$dir, 
#                sprintf("Modelfiles/%s.spawnfile", species_name),
#                sep="/"),
#     sep="\n")
