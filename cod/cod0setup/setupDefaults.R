# defaults for time, species, area

gd <- gadget_directory('cod/codModel')
species.name <- 'cod'
stock0 <- "cod0"
stock <- "cod"
stocknames <- c(stock0, stock)

areas <- read.csv('atlantisInfo/boxInfo.csv', header=T)
#boxes <- filter(areas, boundary == 0)$box_id
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st.year <- 1970
end.year <- 2012
data.st.year <- 1983
year.range <- st.year:end.year
# setup model defaults
model.defaults <- list(   
    areacell = mfdb_group("1" = boxes),
    timestep = mfdb_timestep_quarterly,
    year = st.year:end.year,
    species = 'COD')
# this is different because of atlantis data being so long
data.defaults <- within(model.defaults, 
                        year <- data.st.year:end.year)
