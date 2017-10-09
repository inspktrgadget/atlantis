library(Rgadget)
setwd('~/gadget/atlantis/cod/codModel')
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main='main',
                            grouping=list(short=c('spr.si.short', 'aut.si.short'),
                                          mid=c('spr.si.mid', 'aut.si.mid'),
                                          long=c('spr.si.long', 'aut.si.long')),
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
