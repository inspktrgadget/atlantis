discardAges <- function (adir, area_data, fg_group, fishery) 
{
    nc_out <- ncdf4::nc_open(attr(adir, "nc_catch"))
    age_vars <- mfdbatlantis:::list_nc_variables(nc_out, paste0(fg_group$Name, '[0-9]'))
    age_vars <- grep('Discards', age_vars, value=T)
    catch_by_age <- mfdbatlantis:::fetch_nc_variables(nc_out, age_vars)
    ann_area_age_catch <- apply(catch_by_age, 3, function(x) as.numeric(x))
    dims <- expand.grid(area = as.character(area_data$name), 
                        time = nc_out$dim$t$vals, 
                        functional_group = fg_group$GroupCode,
                        stringsAsFactors = TRUE)
    df_base <- data.frame(area = dims$area, time = dims$time, 
                         year = mfdbatlantis:::atlantis_time_to_years(dims$time) + 
                             attr(adir, "start_year"), 
                         month = mfdbatlantis:::atlantis_time_to_months(dims$time), 
                         fishery = fishery$Code, 
                         functional_group = dims$functional_group,
                         stringsAsFactors = TRUE)
    df_out <- do.call('rbind', apply(ann_area_age_catch, 2, FUN=function(x) {
                    temp <- df_base;
                    temp$count <- x;
                    return(temp);
    }))
    rownames(df_out) <- 1:nrow(df_out)
    cohorts <- as.numeric(as.character(fg_group$NumCohorts))
    age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
    df_out$cohort <- sort(rep(1:cohorts, nrow(df_base)))
    df_out$age <- (df_out$cohort * age_class_size) - (age_class_size%/%2 + 1)
    return(df_out)
}
