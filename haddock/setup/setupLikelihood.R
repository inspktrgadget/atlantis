gadgetlikelihood('likelihood', gd$dir, missingOkay=T) %>%
    gadget_update('penalty',
                  name = "bounds",
                  weight = "0.5",
                  data = data.frame(
                      switch = c("default"),
                      power = c(2),
                      upperW=10000,
                      lowerW=10000,
                      stringsAsFactors = FALSE)) %>%
    gadget_update('understocking',
                  name = 'understocking',
                  weight = '100') %>%
    gadget_update('catchdistribution',
                  name = 'ldist.spr',
                  weight = 1,
                  data = ldist.igfs[[1]],
                  fleetnames = c('spr'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'aldist.spr',
                  weight = 1,
                  data = aldist.igfs[[1]],
                  fleetnames = c('spr'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'ldist.aut',
                  weight = 1,
                  data = ldist.aut[[1]],
                  fleetnames = c('aut'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'aldist.aut',
                  weight = 1,
                  data = aldist.aut[[1]],
                  fleetnames = c('aut'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'ldist.comm',
                  weight = 1,
                  data = ldist.comm[[1]],
                  fleetnames = c('comm'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'aldist.comm',
                  weight = 1,
                  data = aldist.comm[[1]],
                  fleetnames = c('comm'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'ldist.discards',
                  weight = 1,
                  data = ldist.discards[[1]],
                  fleetnames = c('discards'),
                  stocknames = stocknames) %>%
    gadget_update('catchdistribution',
                  name = 'aldist.discards',
                  weight = 1,
                  data = aldist.discards[[1]],
                  fleetnames = c('discards'),
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'spr.si.short',
                  weight = 1,
                  data = spr.si.short[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'spr.si.mid',
                  weight = 1,
                  data = spr.si.mid[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'spr.si.long',
                  weight = 1,
                  data = spr.si.long[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'aut.si.short',
                  weight = 1,
                  data = aut.si.short[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'aut.si.mid',
                  weight = 1,
                  data = aut.si.mid[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    gadget_update('surveyindices',
                  name = 'aut.si.long',
                  weight = 1,
                  data = aut.si.long[[1]],
                  fittype = 'fixedslopeloglinearfit',
                  slope = 1,
                  stocknames = stocknames) %>%
    write.gadget.file(gd$dir)



## Write a penalty component to the likelihood file
gadget_dir_write(gd, 
                 gadget_likelihood_component("penalty",
                                             name = "bounds",
                                             weight = "0.5",
                                             data = data.frame(
                                                 switch = c("default"),
                                                 power = c(2),
                                                 upperW=10000,
                                                 lowerW=10000,
                                                 stringsAsFactors = FALSE)))

gadget_dir_write(gd, 
                 gadget_likelihood_component("understocking",
                                             name = "understocking",
                                             weight = "100"))