library(Rgadget)
setwd('~/gadget/models/atlantis/cod/codModel')
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main='main',
                            grouping=list(short=c('spr.si.0.20', 'aut.si.0.20',
						  'spr.si.20.35', 'aut.si.20.35'),
					  mid=c('spr.si.35.45', 'aut.si.35.45',
						'spr.si.45.60', 'aut.si.45.60'),
					  long=c('spr.si.60.80', 'aut.si.60.80',
						 'spr.si.80.pl', 'aut.si.80.pl')),
                            wgts='WGTS')
)[3]

print(paste('Iterative model took',
            c(gt %/% 3600),
            'hours and',
            c((gt - (gt %/% 3600) * 3600) %/% 60),
            'minutes to run.'))

# survey=c('ldist.igfs', 'ldist.aut',
#          'aldist.igfs', 'aldist.aut'),
# comm=c('ldist.bmt', 'aldist.bmt')
