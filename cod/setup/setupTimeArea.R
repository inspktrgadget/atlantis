# setup time and area files


## write out area and time files
gadgetfile('time',
           file_type = 'time',
           components = list(list(firstyear = st.year,
                                  firststep = 1,
                                  lastyear = end.year,
                                  laststep = 4,
                                  notimesteps = c(4,3,3,3,3)))) %>%
    write.gadget.file(gd$dir)

gadget_areafile(
    size = mfdb_area_size(mdb, defaults)[[1]],
    temperature = mfdb_temperature(mdb, defaults)[[1]]) %>%
    gadget_dir_write(gd, .)

file.copy(paste(getwd(), gd$dir, 'Modelfiles/area', sep='/'), 
          paste(getwd(), gd$dir, 'area', sep='/'), overwrite=T)
