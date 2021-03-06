## setup landings for long line fishery
comm_landings <- mfdb_sample_totalweight(mdb, NULL,
                              c(list(
                                  gear = "BMT",
                                  sampling_type="Cat",
                                  species=model_defaults$species), data_defaults))

## setup discards for long line fishery
# discards <- mfdb_sample_totalweight(mdb, NULL,
#                                         c(list(
#                                             gear = "BMT",
#                                             sampling_type="Discard",
#                                             species=data_defaults$species), data_defaults))

## set up and make surveys as fleet
spr_landings <- structure(data.frame(year=data_defaults$year, step=2, area=1, number=1),
                           area_group=mfdb_group(`1` = 1))
aut_landings <- structure(data.frame(year=data_defaults$year, step=3, area=1, number=1),
          area_group=mfdb_group(`1` = 1))


gadgetfleet("Modelfiles/fleet", gd$dir, missingOkay=T) %>%
    gadget_update("totalfleet",
                  name = "spr",
                  suitability = fleet_suit(fleet = "spr", 
                                           stock = stocknames, 
                                           fun = "newexponentiall50",
                                           params = list("alpha", "l50"),
					   param_names = species_name),
                  data=spr_landings) %>%
    gadget_update("totalfleet",
                  name = "aut",
                  suitability = fleet_suit(fleet = "aut", 
                                           stock = stocknames, 
                                           fun = "newexponentiall50",
                                           params = c("alpha", "l50"), 
                                           param_names = species_name),
                  data = aut_landings) %>%
    gadget_update("totalfleet",
                  name = "comm",
                  suitability = fleet_suit(fleet="comm", 
                                           stock=stock, 
                                           fun="newexponentiall50",
                                           params=list("alpha", "l50")),
                  data = comm_landings[[1]]) %>%
    # gadget_update("totalfleet",
    #               name = "discards",
    #               suitability = fleet_suit("discards", stock, "exponentiall50"),
    #               data = discards[[1]]) %>%
    write.gadget.file(gd$dir)

