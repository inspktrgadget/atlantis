spr.si.0.20 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(0,20))),
    data.defaults))

spr.si.20.35 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(20,35))),
    data.defaults))

spr.si.35.45 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(35,45))),    
    data.defaults))

spr.si.45.60 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(45,60))),    
    data.defaults))

spr.si.60.80 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(60,80))),    
    data.defaults))

spr.si.80.pl <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(80, maxlength), open_ended='upper')),    
    data.defaults))

# autumn survey indices
aut.si.0.20 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(0,20))),
    data.defaults))

aut.si.20.35 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(20,35))),
    data.defaults))

aut.si.35.45 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(35,45))),    
    data.defaults))

aut.si.45.60 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(45,60))),    
    data.defaults))

aut.si.60.80 <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(60,80))),    
    data.defaults))

aut.si.80.pl <- mfdb_sample_count(mdb, c('length'), c(list(
    data_source = "atlantis_survey_totals_Cod",
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(80, maxlength), open_ended='upper')),    
    data.defaults))