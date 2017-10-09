# this is a different way to calculate time from the atlantis output
# for more detail see github.com/mareframe/mfdbatlantis issue #7 - 
# https://github.com/mareframe/mfdbatlantis/issues/7

days.in.year <- 365
sec.in.day <- 60*60*24
sec.in.month <- sec.in.day*(days.in.year/12)
sec.in.year <- sec.in.day*days.in.year

atl.seconds.to.years <- function(time) {
    return(time %/% sec.in.year)
}

atl.seconds.to.months <- function(time) {
    return(((time %% sec.in.year) %/% sec.in.month) + 1)
}

atl.seconds.to.days <- function(time) {
    return((((time %% sec.in.year) %% sec.in.month) %/% sec.in.day) + 1)
}