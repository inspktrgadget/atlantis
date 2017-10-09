getCatchAges <- function (adir, area_data, fg_group, fishery, age=T) 
{
    nc_out <- ncdf4::nc_open(attr(adir, "nc_catch"))
    fishery_vars <- mfdbatlantis:::list_nc_variables(nc_out, paste0(fg_group$GroupCode, "_Catch_FC", 
                                                     fishery$Index, "$"))
    catch_tonnes <- mfdbatlantis:::fetch_nc_variables(nc_out, fishery_vars)
    dims <- expand.grid(area = as.character(area_data$name), 
                        time = nc_out$dim$t$vals, 
                        functional_group = sub("_.*", "", fishery_vars),
                        stringsAsFactors = TRUE)
    if (nrow(dims) == 0) 
        return(data.frame())
    df_out <- data.frame(area = dims$area, time = dims$time, 
                         year = mfdbatlantis:::atlantis_time_to_years(dims$time) + 
                   attr(adir, "start_year"), month = mfdbatlantis:::atlantis_time_to_months(dims$time), 
               fishery = fishery$Code, functional_group = dims$functional_group, 
               weight_total = as.numeric(catch_tonnes), stringsAsFactors = TRUE)
    if (age == T) {
       age_vars <- mfdbatlantis:::list_nc_variables(nc_out, paste0(fg_group$Name, '[0-9]'))
       age_vars <- grep('Catch', age_vars, value=T)
       catch_by_age <- mfdbatlantis:::fetch_nc_variables(nc_out, age_vars)
       ann_area_age_catch <- apply(catch_by_age, 3, function(x) as.numeric(x))
       total_ann_area_catch <- rowSums(ann_area_age_catch)
       age_prop <- ann_area_age_catch / total_ann_area_catch
       df_list <- split(df_out, f=df_out$functional_group)
       age_temp <- as.data.frame(t(age_prop))
       spp.names <- unlist(strsplit(rownames(age_temp), '[0-9]'))
       age_temp$spp <- spp.names[(spp.names %in% fg_group$Name)]
       age_temp$fg <- fg_group$GroupCode[(fg_group$Name %in% age_temp$spp)]
       age_list <- split(age_temp, f=age_temp$fg)
       age_list <- lapply(age_list, FUN=function(x) {
           return(t(subset(x, select= -c(spp, fg))))
       })
       age_df <- do.call('rbind', lapply(df_list, FUN=function(x) {
                        fg.data <- x;
                        age.data <- age_list[[x$functional_group[1]]]
                        cohorts <- grep('[0-9]', colnames(age.data))
                        age.data <- as.vector(as.matrix(age.data))
                        age.data <- data.frame(cohorts=sort(rep(cohorts, nrow(fg.data))),
                                               age.prop = age.data)
                        age_class_size <- as.numeric(as.character(fg_group$NumAgeClassSize))
                        age.data$age <- (age.data$cohorts * age_class_size) - (age_class_size%/%2 + 1)
                        fg.data <- do.call('rbind', replicate(length(cohorts), fg.data, simplify=F))
                        fg.data$age <- age.data$age
                        fg.data$weight <- fg.data$weight_total * age.data$age.prop
                        fg.data$weight[fg.data$weight == 'NaN'] <- 0
                        return(subset(fg.data, select = -c(weight_total)))
           }))
       return(age_df)
    } else {return(df_out)}
}
