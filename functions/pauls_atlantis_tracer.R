# function to read in atlantis output using different time values
# see below sourced files
source('functions/paulsAtlTime.R')

pauls_atlantis_tracer <- function (adir, area_data, fg_group, consumption = FALSE) 
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
                         year = atl.seconds.to.years(dims$time) + attr(adir, 
                                                                         "start_year"), 
                         month = atl.seconds.to.months(dims$time), 
                         day = atl.seconds.to.days(dims$time), group = as.character(fg_group$GroupCode), 
                         cohort = dims$cohort, weight = weight_grams, length = (weight_grams/fg_group$FLAG_LI_A)^(1/fg_group$FLAG_LI_B), 
                         count = fg_Nums, stringsAsFactors = TRUE)
    if (consumption) {
        df_out <- aggregate(cbind(weight, length, count) ~ area + 
                                year + month + group + cohort, df_out, sum)
        nc_prod <- ncdf4::nc_open(attr(adir, "nc_prod"))
        fg_Eat <- mfdbatlantis:::fetch_nc_variables(nc_prod, paste0(fg_group$Name, 
                                                     seq_len(as.character(fg_group$NumCohorts))), "Eat")
        dims <- expand.grid(area = as.character(area_data$name), 
                            time = nc_prod$dim$t$vals, cohort = seq_len(fg_group$NumCohorts), 
                            stringsAsFactors = FALSE)
        df_eat <- data.frame(area = dims$area, year = atlantis_time_to_years(dims$time) + 
                                 attr(adir, "start_year"), month = atlantis_time_to_months(dims$time), 
                             day = atlantis_time_to_days(dims$time), group = as.character(fg_group$GroupCode), 
                             cohort = dims$cohort, consumption = as.numeric(fg_Eat), 
                             stringsAsFactors = TRUE)
        df_out <- merge(df_out, df_eat, by = c("area", "year", 
                                               "month", "group", "cohort"))
        df_out$ind_consumption <- df_out$consumption/df_out$count
    }
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    df_out$age <- (df_out$cohort * age_class_size) - (age_class_size%/%2 + 
                                                          1)
    df_out$maturity_stage = ifelse(df_out$cohort > fg_group$FLAG_AGE_MAT, 
                                   5, 1)
    return(df_out)
}
