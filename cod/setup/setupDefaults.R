# defaults for time, species, area

areas <- read.csv("atlantisInfo/boxInfo.csv", header=T)
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st_year <- 1970
end_year <- 2012
data_st_year <- 1980
year_range <- st_year:end_year
# setup model defaults
model_defaults <- list(   
    areacell = mfdb_group("1" = boxes),
    timestep = mfdb_timestep_quarterly,
    year = st_year:end_year,
    species = "COD")
# this is different because of atlantis data being so long
data_defaults <- within(model_defaults, 
                        year <- data_st_year:end_year)
