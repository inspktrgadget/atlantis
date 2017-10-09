## IGFS survey indices
spr.si.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurvey',
    length = mfdb_interval('len', c(0,15))),
    defaults))

spr.si.mid <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(15,30))),
    defaults))

spr.si.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(30, maxlength))),
    defaults))

# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "igfs.si.short",
#                                                  weight = 1,
#                                                  data = spr.survey.short[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))
# 
# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "igfs.si.mid",
#                                                  weight = 1,
#                                                  data = spr.survey.mid[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))
# 
# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "igfs.si.long",
#                                                  weight = 1,
#                                                  data = spr.survey.long[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))

## AUT survey indices
aut.si.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurvey',
    length = mfdb_interval('len', c(0,15))),
    defaults))

aut.si.mid <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurvey',
    length = mfdb_interval('len', c(15, 30))),
    defaults))

aut.si.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurvey',
    length = mfdb_interval('len', c(30, maxlength))),
    defaults))


# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "aut.si.short",
#                                                  weight = 1,
#                                                  data = aut.survey.short[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))
# 
# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "aut.si.mid",
#                                                  weight = 1,
#                                                  data = aut.survey.mid[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))
# 
# gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
#                                                  name = "aut.si.long",
#                                                  weight = 1,
#                                                  data = aut.survey.long[[1]],
#                                                  fittype = 'fixedslopeloglinearfit',
#                                                  slope=1,
#                                                  stocknames = stocknames))
