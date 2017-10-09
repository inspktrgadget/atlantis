stripAgeLength <- function(survey.data, length.prop, age.prop) {
    base.names <- grep("^count", names(survey.data), value=T, invert=T);
    base.data <- survey.data[, base.names, drop=F];
    counts <- survey.data$count;
    min.age.est <- ceiling(0.5/age.prop)
    age.count <- sapply(counts, function(x) {
        if (x <= min.age.est) {
            out <- rbinom(1,1,age.prop*2)
        } else {
            out <- ceiling(x*age.prop);
        }
        return(out)
    })
    age.data <- base.data;
    age.data$count <- age.count;
    min.length.est <- ceiling(0.5 / length.prop);
    length.count <- sapply(counts, function(x) {
        if (x <= min.length.est) {
            out <- rbinom(1,1,length.prop);
        } else {
            out <- ceiling(x*length.prop)
        }
        return(out)
    })
    length.data <- base.data;
    length.data$age <- NA;
    length.data$maturity_stage <- NA;
    length.data$weight <- NA;
    length.data$count <- length.count;
    no.age.length.count <- counts - age.count - length.count;
    no.age.length.count[no.age.length.count < 0] <- 0;
    no.age.length.data <- base.data;
    no.age.length.data$age <- NA;
    no.age.length.data$length <- NA;
    no.age.length.data$maturity_stage <- NA;
    no.age.length.data$weight <- NA;
    no.age.length.data$count <- no.age.length.count;
    data1 <- rbind(age.data, length.data);
    data2 <- rbind(no.age.length.data, data1)
}