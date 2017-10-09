## setting up a tracer index to see how gadget fits to exact numbers
tracer <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type='Bio'),
    defaults))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "tracer",
                                                 weight = 1e09,
                                                 data = tracer[[1]],
                                                 fittype = 'fixedlinearfit',
                                                 slope=1,
                                                 intercept=0,
                                                 stocknames = stocknames))

## IGFS survey indices
spr.survey.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(0,20,30,40))),
    defaults))

spr.survey.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='SprSurveyTotals',
    length = mfdb_interval('len', c(40,50,60,70,maxlength))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "igfs.si.short",
                                                 weight = 1,
                                                 data = spr.survey.short[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = stocknames))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "igfs.si.long",
                                                 weight = 1,
                                                 data = spr.survey.long[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = stocknames))

## AUT survey indices
aut.survey.short <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(0,20,30,40))),
    defaults))

aut.survey.long <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type ='AutSurveyTotals',
    length = mfdb_interval('len', c(40,50,60,70,maxlength))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "aut.si.short",
                                                 weight = 1,
                                                 data = aut.survey.short[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = stocknames))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "aut.si.long",
                                                 weight = 1,
                                                 data = aut.survey.long[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = stocknames))