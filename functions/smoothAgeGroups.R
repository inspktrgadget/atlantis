smoothAgeGroups <- function(tracer_data, M) {
    base.names <- grep('^count', names(tracer_data), value=T, invert=T);
    base.data <- tracer_data[,base.names, drop=F];
    counts <- tracer_data$count;
    age.pl.one.count <- counts*(0.5 + -0.2753947*M + 0.0412508*M^2); # see I. below
    age.one.count <- counts - age.pl.one.count;
    age.one.data <- base.data;
    age.one.data$count <- age.one.count;
    age.pl.one.data <- base.data;
    age.pl.one.data$count <- age.pl.one.count;
    age.pl.one.data$age <- age.pl.one.data$age + 1
    return(rbind(age.one.data, age.pl.one.data))
}

# # I. relationship to determine multiplier above
# m <- seq(0, 2, 0.01)
# counts <- 1000*exp(-m)
# totals <- counts + 1000
# pct <- test/totals
# plot(pct ~ m)
# summary(lm(pct ~ m + I(m^2)))
# func <- function(x) 0.5 - 0.2753947*x + 0.0412508*x^2
# curve(func(x), add=T, col='red', lwd=2)
