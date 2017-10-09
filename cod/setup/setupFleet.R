## setup landings for long line fishery
comm.landings <- mfdb_sample_totalweight(mdb, NULL,
                              c(list(
                                  gear = 'BMT',
                                  sampling_type='Cat',
                                  species=defaults$species), defaults))

## setup discards for long line fishery
# discards <- mfdb_sample_totalweight(mdb, NULL,
#                                         c(list(
#                                             gear = 'BMT',
#                                             sampling_type='Discard',
#                                             species=defaults$species), defaults))

## set up and make surveys as fleet
igfs.landings <- structure(data.frame(year=defaults$data.years, step=2, area=1, number=1),
                           area_group=mfdb_group(`1` = 1))
aut.landings <- structure(data.frame(year=defaults$data.years, step=3, area=1, number=1),
          area_group=mfdb_group(`1` = 1))


gadgetfleet('Modelfiles/fleet', gd$dir, missingOkay=T) %>%
    # gadget_update('totalfleet',
    #               name = 'spr',
    #               suitability = fleet_suit('spr', stocknames, 'exponentiall50'),
    #               data=igfs.landings) %>%
    # gadget_update('totalfleet',
    #               name = 'aut', 
    #               suitability = fleet_suit('aut', stocknames, 'exponentiall50'),
    #               data = aut.landings) %>%
    gadget_update('totalfleet',
                  name = 'comm',
                  suitability = fleet_suit(fleet='comm', 
                                           stock=stocknames, 
                                           fun='newexponentiall50',
                                           params=list('alpha', 'l50')),
                  data = comm.landings[[1]]) %>%
    # gadget_update('totalfleet',
    #               name = 'discards',
    #               suitability = fleet.suit('discards', stocknames, 'exponentiall50'),
    #               data = discards[[1]]) %>%
    write.gadget.file(gd$dir)

