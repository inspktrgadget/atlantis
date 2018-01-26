# defaults for time, species, area

gd <- gadget_directory("cod/codSpawn/codSpawnModel")
species_name <- "cod"
stock0 <- "cod0"
stock2 <- "cod2"
stock <- "cod"
stocknames <- c("cod2", "cod")


areas <- read.csv("atlantisInfo/boxInfo.csv", header=T)
#boxes <- filter(areas, boundary == 0)$box_id
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st_year <- 1955
end_year <- 1998
data_st_year <- 1968
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
