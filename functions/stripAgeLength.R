strip_al_data <- function(survey_data, length_prop, age_prop) {
    base_names <- grep("^count", names(survey_data), value=T, invert=T);
    base_data <- survey_data[, base_names, drop=F];
    counts <- survey_data$count;
    min_age_est <- ceiling(0.5/age_prop)
    age_count <- sapply(counts, function(x) {
        if (x <= min_age_est & x >= 1) {
            out <- rbinom(1,1,age_prop)
        } else {
            out <- round(x*age_prop);
        }
        return(out)
    })
    age_data <- base_data;
    age_data$count <- age_count;
    min_length_est <- ceiling(0.5 / length_prop);
    length_count <- sapply(counts, function(x) {
        if (x <= min_length_est & x >= 1) {
            out <- rbinom(1,1,length_prop);
        } else {
            out <- round(x*length_prop)
        }
        return(out)
    })
    length_data <- base_data;
    length_data$age <- NA;
    length_data$maturity_stage <- NA;
    length_data$weight <- NA;
    length_data$count <- length_count;
    no_age_length_count <- counts - age_count - length_count;
    no_age_length_count[no_age_length_count < 0] <- 0;
    no_age_length_data <- base_data;
    no_age_length_data$age <- NA;
    no_age_length_data$length <- NA;
    no_age_length_data$maturity_stage <- NA;
    no_age_length_data$weight <- NA;
    no_age_length_data$count <- no_age_length_count;
    data1 <- rbind(age_data, length_data);
    data2 <- rbind(no_age_length_data, data1)
	return(data2)
}

strip_fleet_al_data <- function(survey_data, age_prop) {
    base_names <- grep("^count", names(survey_data), value=T, invert=T);
    base_data <- survey_data[, base_names, drop=F];
    counts <- survey_data$count;
    min_age_est <- ceiling(0.5/age_prop)
    age_count <- sapply(counts, function(x) {
        if (x <= min_age_est & x >= 1) {
            out <- rbinom(1,1,age_prop)
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
