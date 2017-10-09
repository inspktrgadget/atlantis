## setup landings for long line fishery
lln.landings <- mfdb_sample_totalweight(mdb, c('age', 'length'),
                              c(list(
                                  gear = 'LLN',
                                  sampling_type='Cat',
                                  species=defaults$species), defaults))
names(lln.landings[[1]])[names(lln.landings[[1]])=='total_weight'] <- 'number'
lln.landings[[1]]$area <- 1

# ## setup discards for long line fishery
# lln.discards <- mfdb_sample_totalweight(mdb, c('age', 'length'),
#                                         c(list(
#                                             gear = 'LLN',
#                                             sampling_type='Discard',
#                                             species=defaults$species), defaults))
# names(lln.discards[[1]])[names(lln.discards[[1]])=='total_weight'] <- 'number'
# lln.discards[[1]]$area <- 1


## make the longline fleet
lln.fleet <- Rgadget:::make.gadget.fleet(name='lln.comm', suitability='exponentiall50',
                                          fleet.data=lln.landings[[1]],
                                          stocknames=stocknames)

# ## include the discards as part of the catch
# lln.discards <- Rgadget:::make.gadget.fleet(name='lln.discards', suitability='exponentiall50',
#                                                   fleet.data=lln.discards[[1]],
#                                                   stocknames=stocknames)


## set up and make surveys as fleet
igfs.landings <- data.frame(year=defaults$year, step=1, number=1, area=1)
igfs.fleet <- Rgadget:::make.gadget.fleet(name='igfs', suitability='exponentiall50',
                                            fleet.data=igfs.landings,
                                            stocknames=stocknames)

# set up and make autumn survey as fleet
aut.landings <- data.frame(year=defaults$year, step=3, number=1, area=1)
aut.fleet <- Rgadget:::make.gadget.fleet(name='aut', suitability='exponentiall50',
                                         fleet.data=aut.landings,
                                         stocknames=stocknames)

# set up and make tracer as fleet
tracer.landings <- data.frame(year=defaults$year, step=1, number=1, area=1)
tracer.fleet <- Rgadget:::make.gadget.fleet(name='tracer', suitability='exponentiall50',
                                         fleet.data=tracer.landings,
                                         stocknames=stocknames)
