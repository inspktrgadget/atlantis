stripFleetAges <- function(survey_data, age_prop) {
    base_names <- grep("^count", names(survey_data), value=T, invert=T);
    base_data <- survey_data[, base_names, drop=F];
    counts <- survey_data$count;
    min_age_est <- ceiling(0.5/age_prop)
    age_count <- sapply(counts, function(x) {
        if (x <= min_age_est & x >= 1) {
            out <- rbinom(1,1,age_prop*2)
        } else {
            out <- ceiling(x*age_prop);
        }
        return(out)
    })
    age_data <- base_data;
    age_data$count <- age_count;
    no_age_count <- counts - age_count;
    no_age_count[no_age_count < 0] <- 0;
    no_age_data <- base_data;
    no_age_data$age <- NA;
    no_age_data$count <- no_age_count;
    data_out <- rbind(age_data, no_age_data);
    return(data_out)
}
