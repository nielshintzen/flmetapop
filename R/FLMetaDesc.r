#- Function name  | Input                           | Output               | Description
"compress"        | FLBiol,FLCatch                  | Same                 | Compress area or unit dimensions to unique
"setDimnames"     | FLStock,FLBiol,FLCatch          | Same                 | Redefine dimnames to compressed dimnames
"as.FLCatch"      | FLStock                         | FLCatch              | Convert FLStock to FLCatch object
"as.FLStock"      | FLCatch+FLBiol                  | FLStock              | Convert FLCatch and FLBiol to FLStock object
"FLAvail"         | FLCatch+FLBiol                  | FLAvail              | Create proportional availability of fishery to population unit matrix
"catch.n"         | FLCatch+FLBiol+FLAvail          | FLCatch@catch.n      | Calculate catch.n    based on fishery selection, numbers in the population units and availability of fishery to population units
"landings.n"      | FLCatch+FLBiol+FLAvail          | FLCatch@landings.n   | Calculate landings.n based on fishery selection, numbers in the population units and availability of fishery to population units
"discards.n"      | FLCatch+FLBiol+FLAvail          | FLCatch@discards.n   | Calculate discards.n based on fishery selection, numbers in the population units and availability of fishery to population units
"computeHarvest"  | FLStock                         | Same                 | Calculate harvest in FLStock object based on numbers-at-age and catch-at-age
"harmonize"       | FLStock                         | Same                 | Recalculate numbers-at-age and harvest-at-age based on initial numbers-at-age and catch-at-age
"splitByUnit"     | FLBiol+FLCatch+FLAvail          | FLCatches            | Split fisheries by unit (catches by unit)
"splitByUnit"     | FLBiol                          | FLBiols              | Split biol by unit (population unit n,m etc by unit)
"splitByArea"     | FLBiol+FLAvail+FLCatch          | FLBiols              | Split biol by area (population n,m etc by area)
"splitByArea"     | FLCatch                         | FLCatches            | Split fisheries by area (catches by area)
"FLConnect"       | FLBiol                          | FLConnect            | Connectivity between population units (fidelity and migration proportions)
"connectUnit"     | FLBiol+FLConnect                | FLBiol               | Calculate n,m, etc by unit based on connectivity
"TAC2harvest"     | FLQuant+FLBiol+FLCatch+FLAvail  | FLCatch              | Calculate fishery selection based on TAC, population numbers-at-age and availability (by="unit" | "area", scenario="minTAC","maxTAC","equalShare")
"survival"        | FLBiol+FLCatch+FLAvail + yr     | FLBiol               | Calculate survivors from year x to year y
"survival"        | FLStock                         | FLStock              | Calculate survivors from year x to year y
