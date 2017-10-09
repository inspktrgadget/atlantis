minage <- Rgadget:::getMinage(gm)
maxage <- Rgadget:::getMaxage(gm)
maxlength <- 120 #max(Rgadget:::getLengthgroups(gm))

### setting up tracer length and age distributions
tracer.ldata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type='Bio', 
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))

attributes(tracer.ldata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.tracer",
                                             weight = 1e09,
                                             data = tracer.ldata[[1]],
                                             fleetnames = c('tracer'),
                                             stocknames = stocknames))


tracer.aldata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type='Bio',
    age = mfdb_step_interval('age',by=2,from=0,to=18),
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.tracer",
                                             weight = 1e09,
                                             data = tracer.aldata[[1]],
                                             fleetnames = c('tracer'),
                                             stocknames = stocknames))



## Query length data to create survey catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SprSurvey',
    species = defaults$species,
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage


gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.spr",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = stocknames))

rm(aggdata)

# Age surveys
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SprSurvey',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             species=defaults$species,
                             length = mfdb_interval("len", seq(0, maxlength, by = 1))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

# keep.years <- seq(1948, 2013, by=5)
# age5yr model
# aggdata[[1]] <- filter(aggdata[[1]], year %in% keep.years)

# # age0.1 model
# aggdata[[1]]$number <- round(aggdata[[1]]$number*0.1)
# aggdata[[1]] <- filter(aggdata[[1]], number > 0)

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.spr",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = stocknames))
rm(aggdata)


# ## Maturity @3 from IGFS
# aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
#                     append(defaults,
#                         list(sampling_type='IGFS',
#                                 age=mfdb_group(imm=1:6, mat=7:30),
#                                 length = mfdb_step_interval('len', by=1, from=0, to=maxlength),              
#                                 maturity_stage = mfdb_group(gssimm = 1, gssmat = 2:5))))
# 
# gadget_dir_write(gd,
#                  gadget_likelihood_component("stockdistribution",
#                                              name = "matp.igfs",
#                                              weight = 1,
#                                              data = aggdata[[1]],
#                                              fleetnames = c("igfs"),
#                                              stocknames = stocknames))
# rm(aggdata)

# Query length data to create autumn survey catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'AutSurvey',
    species = defaults$species,
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.aut",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("aut"),
                                             stocknames = stocknames))
rm(aggdata)

## Age autumn survey
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'AutSurvey',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             species=defaults$species,
                             length = mfdb_interval("len", seq(0, maxlength, by = 1))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

# age5yr model
#aggdata[[1]] <- filter(aggdata[[1]], year %in% keep.years)

# # age0.1model
# aggdata[[1]]$number <- round(aggdata[[1]]$number*0.1)
# aggdata[[1]] <- filter(aggdata[[1]], number > 0)

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.aut",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("aut"),
                                             stocknames = stocknames))
rm(aggdata)


# ## Maturity @3 from autumn survey
# aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
#                              append(defaults,
#                                     list(sampling_type='AUT',
#                                          age=mfdb_group(imm=1:6, mat=7:20),
#                                          length = mfdb_step_interval('len', by=1, from=0, to=maxlength),
#                                          maturity_stage = mfdb_group(gssimm = 1, gssmat = 2:5))))
# 
# gadget_dir_write(gd,
#                  gadget_likelihood_component("stockdistribution",
#                                              name = "matp.aut",
#                                              weight = 1,
#                                              data = aggdata[[1]],
#                                              fleetnames = c("aut"),
#                                              stocknames = stocknames))
# rm(aggdata)


# Query length data to create bmt catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'CommSurvey',
    species = defaults$species,
    gear = c('LLN'),
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.lln",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("lln.comm"),
                                                 stocknames = stocknames))
rm(aggdata)


## Age bottom.trawl fleet
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'CommSurvey',
                             gear = 'LLN',
                             age = mfdb_step_interval('age',by=2,from=0,to=18),
                             length = mfdb_interval("len", seq(0, maxlength, by = 1))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

# age5yr model
# aggdata[[1]] <- filter(aggdata[[1]], year %in% keep.years)

# # age0.1 model
# aggdata[[1]]$number <- round(aggdata[[1]]$number*0.1)
# aggdata[[1]] <- filter(aggdata[[1]], number > 0)

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.lln",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("lln.comm"),
                                             stocknames = stocknames))
rm(aggdata)
