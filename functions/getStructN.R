getStructN <- function (adir, area_data, fg_group) 
{
    nc_out <- ncdf4::nc_open(attr(adir, "nc_out"))
    fg_Nums <- as.numeric(mfdbatlantis:::fetch_nc_variables(nc_out, paste0(fg_group$Name, 
                                                            seq_len(as.character(fg_group$NumCohorts))), "Nums"))
    fg_StructN <- as.numeric(mfdbatlantis:::fetch_nc_variables(nc_out, paste0(fg_group$Name, 
                                                               seq_len(as.character(fg_group$NumCohorts))), "StructN"))
    dims <- expand.grid(depth = nc_out$dim$z$vals, area = as.character(area_data$name), 
                        time = nc_out$dim$t$vals, cohort = seq_len(as.character(fg_group$NumCohorts)), 
                        stringsAsFactors = FALSE)
    weight_grams <- mfdbatlantis:::mgn_to_grams(ifelse(fg_Nums > 0, fg_StructN, 
                                        NA))
    df_out <- data.frame(depth = dims$depth, area = dims$area, 
                         year = mfdbatlantis:::atlantis_time_to_years(dims$time) + attr(adir, 
                                                                         "start_year"), month = mfdbatlantis:::atlantis_time_to_months(dims$time), 
                         day = mfdbatlantis:::atlantis_time_to_days(dims$time), group = as.character(fg_group$GroupCode), 
                         cohort = dims$cohort, 
                         age = (dims$cohort*2) - 2, weight = weight_grams, 
                         length = (weight_grams/fg_group$FLAG_LI_A)^(1/fg_group$FLAG_LI_B), 
                         count = fg_Nums, stringsAsFactors = TRUE)
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    df_out$age <- (df_out$cohort * age_class_size) - (age_class_size%/%2 + 1)
    df_out <- df_out %>% group_by(area, year, month, group, cohort, age) %>% 
        summarize(weight = mean(weight, na.rm=T), length = mean(length, na.rm=T))
    df_out$area <- as.character(df_out$area)
    return(df_out)
}