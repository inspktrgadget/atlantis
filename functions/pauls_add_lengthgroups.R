pauls_add_lengthgroups <- function (tracer_data, length_group, sigma_per_cohort) 
{
    if (length(length_group) < 2) {
        stop("Length group should have at least 2 members")
    }
    tracer_data$length_var <- sigma_per_cohort[tracer_data$cohort]^2
    lengrp_lower <- length_group[-length(length_group)]
    lengrp_upper <- length_group[-1]
    len_dist <- function(len) {
        pnorm(rep(len, each = nrow(tracer_data)), tracer_data$length, 
              sigma_per_cohort[tracer_data$cohort])
    }
    length_groups <- as.data.frame(matrix(rep(tracer_data$count, 
                                              times = length(lengrp_upper)) * (len_dist(lengrp_upper) - 
                                                                                   len_dist(lengrp_lower)), dimnames = list(c(), paste("length", 
                                                                                                                                       lengrp_lower, lengrp_upper, sep = "_")), ncol = length(lengrp_lower)))
    return(cbind(tracer_data, length_groups))
}
