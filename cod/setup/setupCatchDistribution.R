minage <- cod[[1]]$minage
maxage <- cod[[1]]$maxage
minlength <- cod[[1]]$minlength
maxlength <- cod[[1]]$maxlength
dl <- cod[[1]]$dl

## Query length data to create survey catchdistribution components

## try using mfdb_group(all = minage:maxage) for the age in ldist
ldist.igfs <- 
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SprSurvey',
    species = defaults$species,
    age = mfdb_group(all=minage:maxage),
    length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                           open_ended=c('upper', 'lower'))),
    defaults))


# Age surveys
aldist.igfs <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'SprSurvey',
        age = mfdb_interval('age', minage:maxage,
                            open_ended=c('upper')),
        species=defaults$species,
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c('upper', 'lower'))),
        defaults))



# ## Maturity @3 from IGFS
# aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
#                     append(defaults,
#                         list(sampling_type='IGFS',
#                                 age=mfdb_group(imm=1:6, mat=7:30),
#                                 length = mfdb_step_interval('len', by=1, from=0, to=maxlength),              
#                                 maturity_stage = mfdb_group(gssimm = 1, gssmat = 2:5))))


# Query length data to create autumn survey catchdistribution components
ldist.aut <- 
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'AutSurvey',
        species = defaults$species, 
        age = mfdb_group(all=minage:maxage), 
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c('upper', 'lower'))),
        defaults))

## Age autumn survey
aldist.aut <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'AutSurvey',
        age = mfdb_interval('age', minage:maxage,
                            open_ended=c('upper')),
        species=defaults$species,
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c('upper', 'lower'))),
        defaults))

# ## Maturity @3 from autumn survey
# aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
#                              append(defaults,
#                                     list(sampling_type='AUT',
#                                          age=mfdb_group(imm=1:6, mat=7:20),
#                                          length = mfdb_step_interval('len', by=1, from=0, to=maxlength),
#                                          maturity_stage = mfdb_group(gssimm = 1, gssmat = 2:5))))



# Query length data to create bmt catchdistribution components
ldist.comm <- 
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'CommSurvey',
        species = defaults$species,
        age = mfdb_group(all=minage:maxage), 
        gear = c('BMT'),
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c('upper', 'lower'))),
        defaults))


## Age long line fleet
aldist.comm <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'CommSurvey',
        gear = 'BMT',
        age = mfdb_interval('age', minage:maxage,
                            open_ended = c('upper')),
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c('upper', 'lower'))),
        defaults))

#######################################################################
## the following is to set up catchdistribution components for discards
#######################################################################
# Query length data to create bmt catchdistribution components
# ldist.discards <- 
#     mfdb_sample_count(mdb, c('age', 'length'), c(list(
#         sampling_type = 'DiscardSurvey',
#         species = defaults$species,
#         age = mfdb_group(all=minage:maxage),
#         gear = c('BMT'),
#         length = mfdb_interval("len", seq(0, maxlength, by = dl))),
#         defaults))
# 
# ## Age discards
# aldist.discards <-
#     mfdb_sample_count(mdb, c('age', 'length'), c(list(
#         sampling_type = 'DiscardSurvey',
#         gear = 'BMT',
#         age = mfdb_step_interval('age',by=1,from=0,to=19),
#         length = mfdb_interval("len", seq(0, maxlength, by = dl))),
#         defaults))
