## spring survey indices

spr_si_0_15 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(0,15))),
    data_defaults))

spr_si_15_28 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(15,28))),
    data_defaults))

spr_si_28_43 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(28,43))),    
    data_defaults))

spr_si_43_55 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(43,55))),    
    data_defaults))

spr_si_55_65 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(55,65))),    
    data_defaults))

spr_si_65_pl <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(65, maxlength), open_ended="upper")),    
    data_defaults))

## autumn survey indices

aut_si_0_20 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(0,20))),
    data_defaults))

aut_si_20_35 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(20,35))),
    data_defaults))

aut_si_35_48 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(35,48))),    
    data_defaults))

aut_si_48_60 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(48,60))),    
    data_defaults))

aut_si_60_70 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(60,70))),    
    data_defaults))

aut_si_70_pl <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(70, maxlength), open_ended="upper")),    
    data_defaults))
