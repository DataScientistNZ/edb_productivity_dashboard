library("data.table")
source("R/00_inflation.R")
source("R/00_edb_status.R")

# dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.05.01.parquet")))
dt <- as.data.table(arrow::read_parquet(file.path("data", "EDB_ID__Full__2024.09.1.parquet")))
dt <- dt[network == "All"]

# not all data before 2013, simplifies quite a lot of things (particularly SAIDI...)
dt <- dt[disc_yr >= 2013]
dt <- dt[disc_yr <= 2023]

# below is arguably wrong
dt <- dt[disc_yr == obs_yr]

# rename EDBs as necessary
dt[edb == "Eastland Network", edb := "Firstlight Network"]

all_edbs <- sort(unique(dt$edb))
years <- sort(unique(dt$disc_yr))

###  SAIDI
dt_saidi <- dt[schedule == "SCHEDULE 10: REPORT ON NETWORK RELIABILITY" &
                sub_category == "SAIDI" & 
                 (! startsWith(category, "Transitional")) & 
                 description %in% c("Class B (planned interruptions on the network)",
                                    "Class C (unplanned interruptions on the network)",
                                    "Classes B & C (interruptions on the network)",
                                    "Defective equipment")]

# validate we have data for all edbs... saidi data collection start in 2013?
stopifnot(all(dt_saidi[, .(n = .N), by=c("description", "edb")]$n==length(years)))

dt_saidi[description == "Class B (planned interruptions on the network)", desc := "saidi_planned"]
dt_saidi[description == "Class C (unplanned interruptions on the network)", desc := "saidi_unplanned"]
dt_saidi[description == "Classes B & C (interruptions on the network)", desc := "saidi_total_norm"]
dt_saidi[description == "Defective equipment", desc := "saidi_unplanned_defective_equipment"]
dt_saidi <- dt_saidi[, c("edb", "disc_yr", "desc", "value"), with=F]


### Opex (unit 1k$)
dt_opex <- dt[section == "6b(i): Operational Expenditure" & description == "Operational expenditure"]
stopifnot(all(dt_opex[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_opex[, desc := "opex"]
dt_opex <- dt_opex[, c("edb", "disc_yr", "desc", "value"), with=F]


### Capex (unit 1k$)
dt_capex <- dt[section == "6a(i): Expenditure on Assets" & description == "Expenditure on assets"]
stopifnot(all(dt_capex[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_capex[, desc := "capex"]
dt_capex <- dt_capex[, c("edb", "disc_yr", "desc", "value"), with=F]


### RAB and Depreciation (unit 1k$)
dt_rab <- dt[section == "4(i): Regulatory Asset Base Value (Rolled Forward)" &
                        description %in% c("Total depreciation", 
                                           "Total opening RAB value",
                                           "Total closing RAB value")]
stopifnot(all(dt_rab[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_rab[description == "Total depreciation", desc := "depreciation"]
dt_rab[description == "Total opening RAB value", desc := "rab_open"]
dt_rab[description == "Total closing RAB value", desc := "rab_close"]
dt_rab <- dt_rab[, c("edb", "disc_yr", "desc", "value"), with=F]


### nb_customers & revenue

dt_icp <- dt[schedule == "SCHEDULE 8: REPORT ON BILLED QUANTITIES AND LINE CHARGE REVENUES" &
               category=="Total" & description == "Average no. of ICPs in disclosure year"]
stopifnot(all(dt_icp[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_icp[, desc := "nb_connections"]
dt_icp <- dt_icp[, c("edb", "disc_yr", "desc", "value"), with=F]

dt_revenue <- dt[schedule == "SCHEDULE 3: REPORT ON REGULATORY PROFIT" &
                   category == "Total regulatory income" & description == "Total regulatory income"]
stopifnot(all(dt_revenue[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_revenue[, desc := "revenue"]
dt_revenue <- dt_revenue[, c("edb", "disc_yr", "desc", "value"), with=F]

### / max_demand / energy_delivered / transformers

dt_max_demand <- dt[schedule == "SCHEDULE 9e: REPORT ON NETWORK DEMAND" &
                      category == "Maximum coincident system demand" & 
                      description == "Maximum coincident system demand"]
stopifnot(all(dt_max_demand[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_max_demand[, desc := "max_demand"]
dt_max_demand <- dt_max_demand[, c("edb", "disc_yr", "desc", "value"), with=F]

dt_energy_delivered <- dt[schedule == "SCHEDULE 9e: REPORT ON NETWORK DEMAND" &
                            category == "Electricity volumes carried" & description == "Total energy delivered to ICPs"]
stopifnot(all(dt_energy_delivered[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_energy_delivered[, desc := "energy_delivered"]
dt_energy_delivered <- dt_energy_delivered[, c("edb", "disc_yr", "desc", "value"), with=F]

dt_transformers <- dt[section == "9e(iii): Transformer Capacity" &
                        description == "Total distribution transformer capacity"]
stopifnot(all(dt_transformers[, .(n = .N), by=c("description", "edb")]$n==length(years)))
dt_transformers[, desc := "transformers"]
dt_transformers <- dt_transformers[, c("edb", "disc_yr", "desc", "value"), with=F]


### circuit mess
### circuit length
dt_circ_length <- dt[schedule == "SCHEDULE 9c: REPORT ON OVERHEAD LINES AND UNDERGROUND CABLES" &
                       description == "Total circuit length (for supply)"]
stopifnot(all(dt_circ_length[, .(n = .N), by=c("description", "edb", "sub_category")]$n==length(years)))
dt_circ_length[sub_category == "Total circuit length (km)", desc := "length_circuit"]
dt_circ_length[sub_category == "Underground (km)", desc := "length_underground"]
dt_circ_length[sub_category == "Overhead (km)", desc := "length_overhead"]
dt_circ_length <- dt_circ_length[, c("edb", "disc_yr", "desc", "value"), with=F]

### circuits mva
# conversion kv -> MVA (see p24 CEPA final paper)
circuit_mva_desc <- list(`> 66kV`=72.5, `50kV & 66kV`=30, `33kV`=15, `SWER (all SWER voltages)`=10,
                     `22kV (other than SWER)`=8, `6.6kV to 11kV (inclusive—other than SWER)`=4,
                     `Low voltage (< 1kV)`=0.4)
dt_circuits_mva <- dt[schedule == "SCHEDULE 9c: REPORT ON OVERHEAD LINES AND UNDERGROUND CABLES" &
                        description %in% names(circuit_mva_desc)]
dt_circuits_mva <- merge(dt_circuits_mva, data.table(description=names(circuit_mva_desc), factor=as.numeric(circuit_mva_desc)))
dt_circuits_mva[sub_category == "Total circuit length (km)", desc := "mva_circuit"]
dt_circuits_mva[sub_category == "Underground (km)", desc := "mva_underground"]
dt_circuits_mva[sub_category == "Overhead (km)", desc := "mva_overhead"]
dt_circuits_mva <- dt_circuits_mva[, .(value = sum(value * factor, na.rm = T)), by=c("edb", "disc_yr", "desc")]

# #### Let's ignore the below for now... the NA are assumed to be zeros and thus unecessary ? 
# # that's f*cked up... EDBs sometimes don't seem to provide the detail
# # instead of considering the unfilled value as NA or zero, the point went fully missing
# # that's highly annoying
# # dt_circuits[, .(n = .N), by=c("description", "edb", "disc_yr")]
# # we'll have to inject back some NA in an artistic way ...
# # long story short we create all expected values and merge the whole thing with what we have
# circuit_type <- c("Underground (km)", "Overhead (km)", "Total circuit length (km)")
# dt_tmp <- data.table(CJ(all_edbs, years, circuit_desc, circuit_type))
# setnames(dt_tmp, c("all_edbs", "years", "circuit_desc", "circuit_type"), 
#          c("edb", "disc_yr", "description", "sub_category"))
# dt_circuits <- merge(dt_circuits, dt_tmp, all.y = T)
# # dt_circuits[is.na(value)] # <- observe the created NA value...


dt_all <- rbindlist(list(dt_icp, dt_rab, dt_opex, dt_capex, dt_revenue, dt_saidi, 
                         dt_energy_delivered, dt_transformers, dt_max_demand, 
                         dt_circ_length, dt_circuits_mva))
dt_all
dt_all <- merge(dt_all, get_edb_status()) # add status

dt_flat <- dcast(dt_all, disc_yr + edb + status ~ desc)
dt_flat

# Compute Flow of Capital Services
wacc_fixed <- 0.056  # see CEPA paper
dt_flat[, flow_capital_services := depreciation + (rab_open + 0.5 * capex) * wacc_fixed]
dt_flat[, annual_charge := opex + flow_capital_services]

# Inject Inflation Indices and Convert in Real Amounts
dt_flat <- merge(merge(merge(dt_flat, get_cgpi_factor(), by="disc_yr"), 
                       get_lci_factor()), get_ppi_factor())
dt_flat[, opex_real := opex * (0.6 * lci_real + 0.4 * ppi_real)]
dt_flat[, flow_capital_services_real := flow_capital_services * cgpi_real]
dt_flat[, annual_charge_real := opex_real + flow_capital_services_real]

# Create an artificial saidi_unplanned_norm
dt_flat[,saidi_unplanned_norm := saidi_total_norm - saidi_planned]

fwrite(dt_flat, file.path("data", "extracted_data.csv"))

# todo next:
# a) prep inflation
# b) visualisation data (total time series for each desc) (care, saidi is weighted.avg not sum)
# c) prep annual user cost (flow of capital services + opex)
# d) prep generic Cobb Douglas
# f) model analysis
# g) implement benchmarking
# h) visualisation for benchmarking
# i) web page