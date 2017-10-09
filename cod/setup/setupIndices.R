## IGFS survey indices
spr.si.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(0,18))),
    defaults))

spr.si.mid <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(18,36))),
    defaults))

spr.si.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(36,maxlength))),    
    defaults))


## AUT survey indices
aut.si.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(0,18))),
    defaults))

aut.si.mid <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(18, 36))),
    defaults))

aut.si.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(36,maxlength))),    
    defaults))
