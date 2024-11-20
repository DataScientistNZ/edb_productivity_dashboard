library(data.table)

# todo: implement inclusion of other variables (such as time)
cobb_douglas_regression <- function(dt, input, outputs, other_variables=NULL) {
  # calibrate Cobb Douglas Model
  formula_str <- paste0("I(log(", input, ")) ~ ", 
                        paste0("I(log(", outputs, "))", collapse = " + "))
  m <- glm(data=dt, formula_str)
}


# idx_relative_diff_to_cobb_douglas_eff_frontier
compute_simple_econometric_benchmark <- function(
    dt, input, outputs, allow_time_dependent_frontier=F, other_variables=NULL, 
    edbs_filter=NULL, years_filter=NULL, prod_idx_nm="prod_idx", return_details=F) {
  
  # make a copy of inputs (to allow modification)
  dt <- data.table(dt)
  
  # validate relevance of input parameters
  stopifnot(all(c("disc_yr", "edb", input, outputs) %in% names(dt)))
  
  # keep only what matters for computations
  dt <- dt[, c("disc_yr", "edb", input, outputs), with=F]
  
  # apply filters as required
  if (!is.null(edbs_filter)) {
    stopifnot(all(edbs_filter %in% dt$edb))
    dt <- dt[edb %in% edbs_filter]
  }
  if (!is.null(years_filter)) {
    stopifnot(all(years_filter %in% dt$disc_yr))
    dt <- dt[disc_yr %in% years_filter]
  }
  
  # compute cobb_douglas model
  m <- cobb_douglas_regression(dt, input, outputs, other_variables = other_variables)

  # predict on artificial forecast data that does not allow to 
  # account for time (which is allocated to inefficiency)
  # we store the actual year in `year` but use the base year to make a forecast
  if (allow_time_dependent_frontier) {
    dt_f <- data.table(dt)[, year := disc_yr]
  } else {
    # disc_yr is set to beginning of the period
    dt_f <- data.table(dt)[, year := disc_yr][, disc_yr := min(dt$disc_yr)] 
  }
  
  dt_f[, ("model_efficient_cost") := exp(predict(m, newdata=dt_f, type="response"))]
  dt_f[, (prod_idx_nm) := get("model_efficient_cost")/get("annual_charge_real")]
  
  setnames(dt_f, old=c("disc_yr", "year"), new=c("year", "disc_yr"))
  # return only the desired result?
  if (return_details) return(dt_f)
  return(dt_f[, c("edb", "disc_yr", prod_idx_nm), with=F])
  
}


# source("R/00_echarts.R")
# source("R/00_edb_status.R")
# 
# dt <- fread(file.path("data", "extracted_data.csv"))
# 
# input <- "annual_charge_real"
# outputs <- c("nb_connections", "length_circuit")
# prod_idx_nm <- "prod_idx"
# other_variables <- NULL
# m <- cobb_douglas_regression(dt, input, outputs, other_variables)
# 
# 
# res_all <- compute_simple_econometric_benchmark(dt, my_input, my_outputs, prod_idx_nm=prod_idx_nm)
# res_all <- merge(res_all, get_edb_status())  # add status feature
# res_all[, rank := frankv(get(prod_idx_nm), order=-1, ties.method = "min"), by="disc_yr"]
# 
# my_status <- "NonExempt"
# eplot_line(res_all[status == my_status], "disc_yr", "prod_idx", groupby="edb", x_lab="") |>
#   e_legend(
#     orient = 'vertical',
#     right = 0,
#     top = "middle"
#   ) |>
#   e_grid(right = 200, left=50, bottom=30) |>
#   e_title(paste0("Cobb Douglas rel diff to eff frontier (", my_status, ")"))
# 
# 
# my_status <- "Exempt"
# eplot_line(res_all[status == my_status], "disc_yr", "prod_idx", groupby="edb", x_lab="") |>
#   e_legend(
#     orient = 'vertical',
#     right = 0,
#     top = "middle"
#   ) |>
#   e_grid(right = 200, left=50, bottom=30) |>
#   e_title(paste0("Cobb Douglas rel diff to eff frontier (", my_status, ")"))



