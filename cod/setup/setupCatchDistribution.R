minlength <- cod[[1]]$minlength
maxlength <- cod[[1]]$maxlength
ldist_minage <- 2
ldist_maxage <- 19
aldist_minage <- 2
aldist_maxage <- 13
dl <- 1
dage <- 1

## Query length data to create survey catchdistribution components

## try using mfdb_group(all = minage:maxage) for the age in ldist
ldist_spr <- 
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "SprSurvey",
        species = data_defaults$species,
        age = mfdb_group(all=seq(ldist_minage, ldist_maxage, by = dage)),
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))


# Age surveys
aldist_spr <-
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "SprSurvey",
        age = mfdb_interval("age", seq(aldist_minage, aldist_maxage, by = dage),
                            open_ended=c("upper")),
        species=data_defaults$species,
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))




# Query length data to create autumn survey catchdistribution components
ldist_aut <- 
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "AutSurvey",
        species = data_defaults$species, 
        age = mfdb_group(all=seq(ldist_minage, ldist_maxage, by = dage)), 
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))

## Age autumn survey
aldist_aut <-
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "AutSurvey",
        age = mfdb_interval("age", seq(aldist_minage, aldist_maxage, by = dage),
                            open_ended=c("upper")),
        species=data_defaults$species,
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))

ldist_minage <- 2
aldist_minage <- 2

# Query length data to create bmt catchdistribution components
ldist_comm <- 
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "CommSurvey",
        species = data_defaults$species,
        age = mfdb_group(all=seq(ldist_minage, ldist_maxage, by = dage)), 
        gear = c("BMT"),
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))


## Age long line fleet
aldist_comm <-
    mfdb_sample_count(mdb, c("age", "length"), c(list(
        sampling_type = "CommSurvey",
        gear = "BMT",
        age = mfdb_interval("age", seq(aldist_minage, aldist_maxage, by = dage),
                            open_ended = c("upper")),
        length = mfdb_interval("len", seq(minlength, maxlength, by = dl),
                               open_ended=c("upper", "lower"))),
        data_defaults))

