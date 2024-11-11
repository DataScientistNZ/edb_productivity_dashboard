get_inflation_index <- function(inflation_nm, min_year=NULL) {
  stopifnot(toupper(inflation_nm) %in% c("CGPI", "PPI", "LCI"))
  dt <- fread(file.path("data", paste0(toupper(inflation_nm), ".csv")), skip = 5, fill=T)
  setnames(dt, c("V1", "V2"), c("quarter", "index"))
  dt <- dt[(!is.na(index)) & endsWith(quarter, "Q1")]
  dt <- dt[, disc_yr := as.numeric(substr(quarter, 1, 4))]
  if (!is.null(min_year)) {
    dt <- dt[disc_yr >= min_year]
  }
  dt[, c("disc_yr", "index"), with=F]
}

get_inflation_factor <- function(inflation_nm, base_year=NULL, min_year=NULL) {
  dt <- get_inflation_index(inflation_nm, min_year)
  if (is.null(base_year)) {
    base_year <- max(dt$disc_yr)
  }
  nm <- paste0(tolower(inflation_nm), "_real")
  dt[, (nm) := index[disc_yr==base_year]/index][, c("disc_yr", nm), with=F]
}

get_cgpi_index <- function(...) {get_inflation_index("CGPI", ...)}
get_lci_index <- function(...) {get_inflation_index("lci", ...)}
get_ppi_index <- function(...) {get_inflation_index("ppi", ...)}
get_cgpi_factor <- function(...) {get_inflation_factor("CGPI", ...)}
get_lci_factor <- function(...) {get_inflation_factor("LCI", ...)}
get_ppi_factor <- function(...) {get_inflation_factor("PPI", ...)}

# get_cgpi_index(min_year=2013)
# get_cgpi_factor(min_year=2013, base_year=2017)
