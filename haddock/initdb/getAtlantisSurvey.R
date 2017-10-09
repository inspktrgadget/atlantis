getAtlantisSurvey <- function (tracer_data, length_group, survey_suitability, survey_sigma) 
{
    lengrp_lower <- length_group[-length(length_group)]
    lengrp_upper <- length_group[-1]
    err_variance <- survey_sigma^2
    base_names <- grep("^length|^weight|^count", names(tracer_data), 
                       value = TRUE, invert = TRUE)
    base_data <- tracer_data[, base_names, drop = FALSE]
    do.call(rbind, lapply(seq_len(length(lengrp_lower)), function(i) {
        length_col <- paste("length", lengrp_lower[[i]], lengrp_upper[[i]], 
                            sep = "_")
        out <- base_data
        out$length <- mean(c(lengrp_upper[[i]], lengrp_lower[[i]]))
        out$weight <- fg_group$FLAG_LI_A*out$length^fg_group$FLAG_LI_B
        out$count <- round(tracer_data[, length_col] * exp(rnorm(nrow(base_data), 
                                                                 0, err_variance) - err_variance/2) * survey_suitability[[i]])
        return(out)
    }))
}
