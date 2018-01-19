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

# age_mean_formula <- "exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s"
rec_number <- sprintf("%1$s.rec.scalar*%1$s.rec.%2$s", species_name, year_range)
rec_sd <- sprintf("#%s.rec.sd", species_name)


cod <- 
    gadgetstock("cod", gd$dir, missingOkay=T) %>%
    gadget_update("stock",
                  minage = 2,
                  maxage = 19,
                  minlength = 1,
                  maxlength = 199,
                  dl = 5,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight_alpha,
                                     beta=weight_beta),
                  beta=sprintf("(* 10 #%s.bbin)", .[[1]]$stockname)) %>%
    # gadget_update("naturalmortality", # m for each age
    #               sprintf("#%1$s.age%2$s.m", 
    #                       .[[1]]$stockname, 
    #                       .[[1]]$minage:.[[1]]$maxage)) %>%
    gadget_update("naturalmortality", # m as a function of age
                  m_estimate_formula(max_m=sprintf("%s.max.m", .[[1]]$stockname),
                                     min_m=sprintf("%s.min.m", .[[1]]$stockname),
                                     m_decay=sprintf("%s.m.decay", .[[1]]$stockname),
                                     age=.[[1]]$minage:.[[1]]$maxage)
                                     ) %>%
    gadget_update("initialconditions",
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor=init_age_factor(init_max=sprintf("%s.init.max", 
                                                                             .[[1]]$stockname),
                                                            init_min=sprintf("%s.init.min", 
                                                                             .[[1]]$stockname),
                                                            init_decay=sprintf("%s.init.decay", 
                                                                            .[[1]]$stockname),
                                                            age=age),
                                 area.factor=sprintf("( * #%1$s.mult #%1$s.init.abund)",
                                                     .[[1]]$stockname),
                                 mean = vonb_formula(.[[1]]$minage:.[[1]]$maxage,
                                                     linf=sprintf("%s.linf", species_name),
                                                     k=sprintf("%s.k", species_name),
                                                     recl=sprintf("%s.recl", species_name)),
                                 stddev = init_sigma$ms[c(((.[[1]]$minage)+1):
                                                         ((.[[1]]$maxage)+1),
							 rep(.[[1]]$maxage, 4))],
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
