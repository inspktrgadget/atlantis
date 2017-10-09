getCodDiscards <- function (adir, area_data, fishery) 
{
    nc_out <- ncdf4::nc_open(attr(adir, "nc_catch"))
    fishery_vars <- mfdbatlantis:::list_nc_variables(nc_out, paste0("FCD_Discard_FC", 
                                                     fishery$Index, "$"))
    catch_tonnes <- mfdbatlantis:::fetch_nc_variables(nc_out, fishery_vars)
    dims <- expand.grid(area = as.character(area_data$name), 
                        time = nc_out$dim$t$vals, functional_group = sub("_.*", 
                                                                         "", fishery_vars), stringsAsFactors = TRUE)
    if (nrow(dims) == 0) 
        return(data.frame())
    data.frame(area = dims$area, time = dims$time, 
               year = mfdbatlantis:::atlantis_time_to_years(dims$time) + 
                   attr(adir, "start_year"), 
               month = mfdbatlantis:::atlantis_time_to_months(dims$time), 
               fishery = fishery$Code, functional_group = dims$functional_group, 
               weight_total = as.numeric(catch_tonnes), stringsAsFactors = TRUE)
}
