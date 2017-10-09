# defaults for time, species, area

gd <- gadget_directory('haddock/hadModel')
species.name <- 'had'
stocknames <- 'had'

areas <- read.csv('atlantisInfo/boxInfo.csv', header=T)
#boxes <- filter(areas, boundary == 0)$box_id
boxes <- sprintf("Box%s", filter(areas, boundary == 0)$box_id)

st.year <- 1935
end.year <- 1955
year.range <- st.year:end.year
defaults <- list(   
    areacell = mfdb_group("1" = boxes),
    timestep = mfdb_timestep_quarterly,
    year = st.year:end.year,
    species = 'HAD')
