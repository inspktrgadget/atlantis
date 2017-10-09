minage <- had[[1]]$minage
maxage <- had[[1]]$maxage
minlength <- had[[1]]$minlength
maxlength <- had[[1]]$maxlength
dl <- had[[1]]$dl

## Query length data to create survey catchdistribution components
ldist.igfs <- 
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SprSurvey',
    species = defaults$species,
    age = mfdb_interval('all', c(minage, maxage), open_ended=T),
    length = mfdb_interval("len", seq(0, maxlength, by = 1))),
    defaults))


# Age surveys
aldist.igfs <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'SprSurvey',
        age = mfdb_step_interval('age',by=1,from=0,to=19),
        species=defaults$species,
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
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
        age = mfdb_interval('all', c(minage, maxage), open_ended=T),
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
        defaults))

## Age autumn survey
aldist.aut <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'AutSurvey',
        age = mfdb_step_interval('age',by=1,from=0,to=19),
        species=defaults$species,
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
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
        age = mfdb_interval('all', c(minage, maxage), open_ended=T),
        gear = c('LLN'),
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
        defaults))


## Age long line fleet
aldist.comm <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'CommSurvey',
        gear = 'LLN',
        age = mfdb_step_interval('age',by=1,from=0,to=19),
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
        defaults))

#######################################################################
## the following is to set up catchdistribution components for discards
#######################################################################
# Query length data to create bmt catchdistribution components
ldist.discards <- 
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'DiscardSurvey',
        species = defaults$species,
        age = mfdb_interval('all', c(minage, maxage), open_ended=T),
        gear = c('LLN'),
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
        defaults))

## Age discards
aldist.discards <-
    mfdb_sample_count(mdb, c('age', 'length'), c(list(
        sampling_type = 'DiscardSurvey',
        gear = 'LLN',
        age = mfdb_step_interval('age',by=1,from=0,to=19),
        length = mfdb_interval("len", seq(0, maxlength, by = 1))),
        defaults))
