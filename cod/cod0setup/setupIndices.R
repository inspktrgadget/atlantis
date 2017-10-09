## IGFS survey indices
spr.si.0.18 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(0,20))),
    data.defaults))

spr.si.18.33 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(20,35))),
    data.defaults))

spr.si.33.45 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(35,45))),    
    data.defaults))

spr.si.45.58 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(45,60))),    
    data.defaults))

spr.si.58.80 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(60,80))),    
    data.defaults))

spr.si.80.pl <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(80, maxlength), open_ended='upper')),    
    data.defaults))

## AUT survey indices
aut.si.0.18 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(0,20))),
    data.defaults))

aut.si.18.33 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(20,35))),
    data.defaults))

aut.si.33.45 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(35,45))),    
    data.defaults))

aut.si.45.58 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(45,60))),    
    data.defaults))

aut.si.58.80 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(60,80))),    
    data.defaults))

aut.si.80.pl <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(80, maxlength), open_ended='upper')),    
    data.defaults))


# aut.si.short <- mfdb_sample_count(mdb, c('length'), c(list(
#     sampling_type ='AutSurveyTotals',
#     length = mfdb_interval('len', c(0,18))),
#     data.defaults))
# 
# aut.si.mid <- mfdb_sample_count(mdb, c('length'), c(list(
#     sampling_type ='AutSurveyTotals',
#     length = mfdb_interval('len', c(18, 36))),
#     data.defaults))
# 
# aut.si.long <- mfdb_sample_count(mdb, c('length'), c(list(
#     sampling_type ='AutSurveyTotals',
#     length = mfdb_interval('len', c(36,maxlength))),    
#     data.defaults))
