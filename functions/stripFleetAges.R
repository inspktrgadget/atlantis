stripFleetAges <- function(survey.data, age.prop) {
    base.names <- grep("^count", names(survey.data), value=T, invert=T);
    base.data <- survey.data[, base.names, drop=F];
    counts <- survey.data$count;
    min.age.est <- ceiling(0.5/age.prop)
    age.count <- sapply(counts, function(x) {
        if (x <= min.age.est & x >= 1) {
            out <- rbinom(1,1,age.prop*2)
        } else {
            out <- ceiling(x*age.prop);
        }
        return(out)
    })
    age.data <- base.data;
    age.data$count <- age.count;
    no.age.count <- counts - age.count;
    no.age.count[no.age.count < 0] <- 0;
    no.age.data <- base.data;
    no.age.data$age <- NA;
    no.age.data$count <- no.age.count;
    data.out <- rbind(age.data, no.age.data);
    return(data.out)
}
