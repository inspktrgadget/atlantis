## IGFS survey indices
spr_si_0_20 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="SprSurveyTotals",
    length = mfdb_interval("len", c(0,20))),
    data_defaults))

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

## AUT survey indices
aut_si_0_20 <- mfdb_sample_count(mdb, c("length"), c(list(
    sampling_type ="AutSurveyTotals",
    length = mfdb_interval("len", c(0,20))),
    data_defaults))

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


# aut_si_short <- mfdb_sample_count(mdb, c("length"), c(list(
#     sampling_type ="AutSurveyTotals",
#     length = mfdb_interval("len", c(0,18))),
#     data_defaults))
# 
# aut_si_mid <- mfdb_sample_count(mdb, c("length"), c(list(
#     sampling_type ="AutSurveyTotals",
#     length = mfdb_interval("len", c(18, 36))),
#     data_defaults))
# 
# aut_si_long <- mfdb_sample_count(mdb, c("length"), c(list(
#     sampling_type ="AutSurveyTotals",
#     length = mfdb_interval("len", c(36,maxlength))),    
#     data_defaults))
