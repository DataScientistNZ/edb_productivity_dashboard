get_edb_status <- function() {

  status_edbs <- list(
    `Alpine Energy`="NonExempt",
    `Aurora Energy`="NonExempt",
    `Buller Electricity`="Exempt",
    `Centralines`="Exempt",
    `Counties Energy`="Exempt",
    `Eastland Network`="NonExempt",
    `Electra`="Exempt",
    `EA Networks`="NonExempt",
    `Electricity Invercargill`="NonExempt",
    `Horizon Energy`="NonExempt",
    `MainPower NZ`="Exempt",
    `Marlborough Lines`="Exempt",
    `Nelson Electricity`="NonExempt",
    `Network Tasman`="NonExempt",
    `Network Waitaki`="Exempt",
    `Northpower`="Exempt",
    `Orion NZ`="NonExempt",
    `OtagoNet`="NonExempt",
    `Powerco`="NonExempt",
    `Scanpower`="Exempt",
    `The Lines Company`="NonExempt",
    `The Power Company`="Exempt",
    `Top Energy`="NonExempt",
    `Unison Networks`="NonExempt",
    `Vector Lines`="NonExempt",
    `Waipa Networks`="Exempt",
    `WEL Networks`="Exempt",
    `Wellington Electricity`="NonExempt",
    `Westpower`="Exempt")
  
  dt_status <- data.table(edb=names(status_edbs), status=as.character(status_edbs))
  return(dt_status)
}
  