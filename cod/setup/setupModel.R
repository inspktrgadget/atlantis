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


## populate the model with starting default values

## alpha and beta for lw relationship
## values taken from Atlantis model
weight_alpha <- 0.0000021
weight_beta <- 3.3437

# age.mean.formula <- "exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s"
rec_number <- sprintf("%1$s.rec.scalar*%1$s.rec.%2$s", species_name, year_range)
rec_sd <- sprintf("#%s.rec.sd", species_name)

st_sigma <- rbind(init_sigma, init_sigma) %>% arrange(age)


# pre-define some paramater file arguments for 2-12 year old stock
stock0_minage <- 0
stock0_maxage <- 1
stock0_age_factor <- init_age_factor(init_max=sprintf("%s.init.max", species_name),
                                     init_min=sprintf("%s.init.min", species_name),
                                     init_decay=sprintf("%s.init.decay", species_name),
                                     age=stock0_minage:stock0_maxage)
stock0_area_factor <- sprintf("( * #%1$s.init.mult #%1$s.init.scalar)",
                             species_name)
stock0_meanlength <- vonb_formula(linf=sprintf("%s.linf", species_name),
                                  k=sprintf("%s.k", species_name),
                                  recl=sprintf("%s.recl", species_name),
                                  age = stock0_minage:stock0_maxage)
stock0_sd <- c(init_sigma$ms[1:2])


# setup age 0-1 stock
cod0 <- 
    gadgetstock("cod0", gd$dir, missingOkay=T) %>%
    gadget_update("stock",
                  minage = stock0_minage,
                  maxage = stock0_maxage,
                  minlength = 1,
                  maxlength = 36,
                  dl = 1,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight_alpha,
                                     beta=weight_beta),
                  beta=sprintf("(* #%1$s.bbin.mult #%s.bbin)", species_name),
		  maxlengthgroupgrowth = 10) %>%
    gadget_update("naturalmortality", 
                  c(0.2, 0.175)) %>%
    gadget_update("initialconditions",
                  normalparam=
                      data_frame(age = .[[1]]$minage:.[[1]]$maxage, 
                                 area = 1,
                                 age.factor=stock0_age_factor,
                                 area.factor=stock0_area_factor,
                                 mean = stock0_meanlength,
                                 stddev = stock0_sd,
                                 alpha = weight_alpha,
                                 beta = weight_beta)) %>%
    gadget_update("refweight",
                  data=data_frame(length=seq(.[[1]]$minlength,
                                             .[[1]]$maxlength,
                                             .[[1]]$dl),
                                  mean = weight_alpha*length^weight_beta)) %>%
    gadget_update("iseaten", 0) %>%
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
                          mean = stock0_meanlength[1],
                          stddev = rec_sd,
                          alpha = weight_alpha,
                          beta = weight_beta))


# pre-define some paramater file arguments for 2-12 year old stock
stock_minage <- 2
stock_maxage <- 12
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
                  maxlength = 130,
                  dl = 1,
                  livesonareas = 1) %>%
    gadget_update("doesgrow",
                  growthparameters=c(linf=sprintf("#%s.linf", species_name), 
                                     k=sprintf("#%s.k", species_name),
                                     alpha=weight_alpha,
                                     beta=weight_beta),
                  beta=sprintf("(* #%1$s.bbin.mult #%1$s.bbin)", .[[1]]$stockname),
		  maxlengthgroupgrowth = 10) %>%
    gadget_update("naturalmortality", 
                  c(rep(0.15, ((.[[1]]$maxage - .[[1]]$minage))), 0.3)) %>%
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


write.gadget.file(cod0, gd$dir)
write.gadget.file(cod, gd$dir)
