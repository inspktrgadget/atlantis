## spring survey indices

spr_si_20_35 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(20,35))),
    data_defaults))

spr_si_35_45 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(35,45))),    
    data_defaults))

spr_si_45_60 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(45,60))),    
    data_defaults))

spr_si_60_80 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(60,80))),    
    data_defaults))

spr_si_80_pl <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(80, maxlength), open_ended="upper")),    
    data_defaults))

## autumn survey indices

aut_si_20_35 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(20,35))),
    data_defaults))

aut_si_35_45 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(35,45))),    
    data_defaults))

aut_si_45_60 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(45,60))),    
    data_defaults))

aut_si_60_80 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(60,80))),    
    data_defaults))

aut_si_80_pl <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(80, maxlength), open_ended="upper")),    
    data_defaults))
