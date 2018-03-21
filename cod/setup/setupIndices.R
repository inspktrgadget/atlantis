## spring survey indices

spr_si_0_18 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(0,18))),
    data_defaults))

spr_si_18_36 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(18,36))),
    data_defaults))

spr_si_36_48 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(36,48))),    
    data_defaults))

spr_si_48_60 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(48,60))),    
    data_defaults))

spr_si_60_70 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(60,70))),    
    data_defaults))

spr_si_70_pl <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(70, maxlength), open_ended="upper")),    
    data_defaults))

## autumn survey indices

aut_si_0_18 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(0,18))),
    data_defaults))

aut_si_18_36 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(18,36))),
    data_defaults))

aut_si_36_48 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(36,48))),    
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
