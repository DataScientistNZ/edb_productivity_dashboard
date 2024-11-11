library(data.table)

compute_mtfp <- function(dt, inputs, inputs_nominal, outputs, output_prices,
                 edbs_filter=NULL, years_filter=NULL, return_details=F) {
  
  # make a copy of inputs (to allow modification)
  dt <- data.table(dt)
  
  # validate relevance of input parameters
  stopifnot(all(c("disc_yr", "edb", inputs, inputs_nominal, outputs) %in% names(dt)))
    
  # keep only what matters for computations
  dt <- dt[, c("disc_yr", "edb", inputs, inputs_nominal, outputs), with=F]
  
  # apply filters as required
  if (!is.null(edbs_filter)) {
    stopifnot(all(edbs_filter %in% dt$edb))
    dt <- dt[edb %in% edbs_filter]
  }
  if (!is.null(years_filter)) {
    stopifnot(all(years_filter %in% dt$disc_yr))
    dt <- dt[disc_yr %in% years_filter]
  }
  
  # extract input prices
  for (i in 1:length(inputs)) {
    dt[, (paste0(inputs_nominal[i], "_price")) := get(inputs_nominal[i]) / get(inputs[i])]
  }
  
  # prepare output prices (assumes constant output prices in real terms)
  for (i in 1:length(outputs)) {
    dt[, (paste0(outputs[[i]], "_price")) := output_prices[i]]
  }
  
  #################################
  # outputs cost computations
  dt[, cost_outputs := 0]
  dt[, period_cost_outputs := 0]
  dt[, transitive_log_outputs_idx := 0]
  for (i in 1:length(outputs)) { 
    # initialise cost of outputs at zero
    dt[, (paste0(outputs[i], "_cost")) := 0]
  }
  for (i in 1:length(outputs)) {
    # compute cost of singular outputs  [Q * P]
    dt[, (paste0(outputs[i], "_cost")) := get(paste0(outputs[i], "_cost")) +
         get(outputs[i]) * get(paste0(outputs[i], "_price"))]
    
    # compute avg log output
    dt[, (paste0(outputs[i], "_avg_log")) := mean(log(get(outputs[i])))]
  }
  for (i in 1:length(outputs)) {  # compute overall cost of outputs
    dt[, cost_outputs := cost_outputs + get(paste0(outputs[i], "_cost"))]
    dt[, period_cost_outputs := period_cost_outputs + sum(get(paste0(outputs[i], "_cost")))]
  }
  for (i in 1:length(outputs)) {  # compute shares of outputs
    dt[, (paste0(outputs[i], "_overall_share")) := sum(get(paste0(outputs[i], "_cost"))) / period_cost_outputs]
    dt[, (paste0(outputs[i], "_share")) := get(paste0(outputs[i], "_cost")) / cost_outputs]
  }
  for (i in 1:length(outputs)) {  # compute transitive log output
    dt[, transitive_log_outputs_idx := transitive_log_outputs_idx + 0.5 * (
      get(paste0(outputs[i], "_overall_share")) + get(paste0(outputs[i], "_share"))) * (
        log(get(outputs[i])) - get(paste0(outputs[i], "_avg_log")))]
  }
  
  #########################
  # inputs cost computations
  dt[, cost_inputs := 0]
  dt[, period_cost_inputs := 0]
  dt[, transitive_log_inputs_idx := 0]
  for (i in 1:length(inputs)) {  # initialise cost of inputs at zero
    dt[, (paste0(inputs_nominal[i], "_cost")) := 0]
  }
  for (i in 1:length(inputs)) {
    # compute cost of singular inputs  [Q * P]
    dt[, (paste0(inputs_nominal[i], "_cost")) := get(paste0(inputs_nominal[i], "_cost")) + + 
         get(inputs[i]) * get(paste0(inputs_nominal[i], "_price"))]
    
    # compute avg log input
    dt[, (paste0(inputs_nominal[i], "_avg_log")) := mean(log(get(inputs[i])))]
  }
  for (i in 1:length(inputs)) { # compute overall cost of inputs
    dt[, cost_inputs := cost_inputs + get(paste0(inputs_nominal[i], "_cost"))]
    dt[, period_cost_inputs := period_cost_inputs + sum(get(paste0(inputs_nominal[i], "_cost")))]
  }
  for (i in 1:length(inputs)) {  # compute share of inputs
    dt[, (paste0(inputs_nominal[i], "_overall_share")) := sum(get(paste0(inputs_nominal[i], "_cost"))) / period_cost_inputs]
    dt[, (paste0(inputs_nominal[i], "_share")) := get(paste0(inputs_nominal[i], "_cost")) / cost_inputs]
  }
  for (i in 1:length(inputs)) {  # compute transitive log inputs
    dt[, transitive_log_inputs_idx := transitive_log_inputs_idx + 0.5 * (
      get(paste0(inputs_nominal[i], "_overall_share")) + get(paste0(inputs_nominal[i], "_share"))) * (
        log(get(inputs[i])) - get(paste0(inputs_nominal[i], "_avg_log")))]
  }
  
  # finally compute MFTP
  dt[, mtfp := exp(transitive_log_outputs_idx - transitive_log_inputs_idx)]
  
  # return only the desired result?
  if (return_details) return(dt)
  return(dt[, c("edb", "disc_yr", "mtfp"), with=F])
}

rescale_index <- function(dt, years_filter=NULL, edbs_filter=NULL, field="mtfp") {
  dt <- data.table(dt) # copy input to avoid changing result
  if (is.null(years_filter)) {
    years_filter <- unique(dt[["disc_yr"]])
  } else {
    stopifnot(all(years_filter %in% dt[["disc_yr"]]))
  }
  if (is.null(edbs_filter)) {
    edbs_filter <- unique(dt[["edb"]])
  } else {
    stopifnot(all(edbs_filter %in% dt[["edb"]]))
  }
  mean_res <- mean(dt[disc_yr %in% years_filter & edb %in% edbs_filter][[field]])
  stopifnot(!is.na(mean_res))
  dt[, (field) := get(field)/mean_res][]
}

# library(ggplot2)
# source(file.path(here::here(), "R", "00_edb_status.R"))
# 
# dt <- fread(file.path("data", "extracted_data.csv"))
# dt <- merge(dt, get_edb_status())  # add status feature
# 
# edbs <- c("Alpine Energy", "Orion NZ", "Vector Lines")
# years <- 2017:2021
# 
# # specify model...
# inputs <- c("flow_capital_services_real", "opex_real")
# my_inputs_nominal <- c("flow_capital_services", "opex")
# my_outputs <- c("nb_connections", "length_circuit")
# my_output_prices <- c(330, 3000)

# res <- compute_mtfp(dt, inputs = my_inputs, inputs_nominal = my_inputs_nominal,
#                     outputs = my_outputs, output_prices = my_output_prices,
#                     edbs_filter=edbs, years_filter=years, return_details = T)
# res

# res <- compute_mtfp(dt, inputs = my_inputs, inputs_nominal = my_inputs_nominal,
#                     outputs = my_outputs, output_prices = my_output_prices,
#                     edbs_filter=edbs, years_filter=years)
# res <- rescale_index(res, edbs_filter = "Vector Lines", years_filter = "2017")
# res[, rank := frankv(mtfp, order=-1, ties.method = "min"), by="disc_yr"]
# res

# 
# ggplot(res, aes(x=disc_yr, y=mtfp, color=edb)) +
#   ylim(c(0.8 * min(res$mtfp), 1.2 * max(res$mtfp))) +
#   geom_point() + geom_line(linewidth=1) + theme_minimal()
# 
# res <- compute_mtfp(dt, inputs = my_inputs, inputs_nominal = my_inputs_nominal, 
#                     outputs = my_outputs, output_prices = my_output_prices)
# res[, rank := frankv(mtfp, order=-1, ties.method = "min"), by="disc_yr"]
# res
# 
# # statuses <- c("Exempt", "NonExempt")
# 
# my_status <- "NonExempt"
# res <- compute_mtfp(dt[status == my_status], inputs = my_inputs, inputs_nominal = my_inputs_nominal, 
#                     outputs = my_outputs, output_prices = my_output_prices)
# res[, rank := frankv(mtfp, order=-1, ties.method = "min"), by="disc_yr"]
# ggplot(res, aes(x=disc_yr, y=mtfp)) +
#   ylim(c(min(0.5, min(res$mtfp)), NA)) +
#   geom_point(color="skyblue3") + geom_line(color="skyblue3", linewidth=1) + theme_bw() + 
#   facet_wrap(~edb) + scale_x_continuous(breaks = scales::breaks_pretty()) +
#   ggtitle(paste0("MTFP (",my_status, ")"))
# 
# eplot_line(res, "disc_yr", "mtfp", groupby="edb", x_lab="") 
# 
# my_status <- "Exempt"
# res <- compute_mtfp(dt[status == my_status], inputs = my_inputs, inputs_nominal = my_inputs_nominal,
#                     outputs = my_outputs, output_prices = my_output_prices)
# res[, rank := frankv(mtfp, order=-1, ties.method = "min"), by="disc_yr"]
# ggplot(res, aes(x=disc_yr, y=mtfp)) +
#   ylim(c(min(0.5, min(res$mtfp)), NA)) +
#   geom_point(color="skyblue3") + geom_line(color="skyblue3") + theme_bw() +
#   facet_wrap(~edb) + scale_x_continuous(breaks = scales::breaks_pretty()) +
#   ggtitle(paste0("MTFP (",my_status, ")"))
# 
# eplot_line(res, "disc_yr", "mtfp", groupby="edb", x_lab="") |>
#   e_legend(
#     orient = 'vertical',
#     right = 0,
#     top = "middle"
#   ) |>
#   e_grid(right = 200, left=50, bottom=30) |>
#   e_title(paste0("MTFP (", my_status, ")"))

# ############################################################
# library(ggplot2)
# source(file.path(here::here(), "R", "00_edb_status.R"))
# 
# dt <- fread(file.path("data", "extracted_data.csv"))
# 
# edbs <- c("Alpine Energy", "Orion NZ", "Vector Lines")
# years <- 2017:2021
# 
# # specify model...
# my_inputs <- c("flow_capital_services_real", "opex_real")
# my_inputs_nominal <- c("flow_capital_services", "opex")
# my_outputs <- c("nb_connections", "length_circuit")
# my_output_prices <- c(330, 3000)
# 
# res_all <- compute_mtfp(dt, inputs = my_inputs, inputs_nominal = my_inputs_nominal,
#                         outputs = my_outputs, output_prices = my_output_prices)
# res_all <- rescale_index(res_all, years_filter = "2017")
# res_all <- merge(res_all, get_edb_status())  # add status feature
# stopifnot(all(dt$edb %in% res_all$edb)) # check we didn't lose anything on the way
# 
# 
# ##################
# my_status <- "NonExempt"
# res_non_exempt <- res_all[status == my_status]
# res_non_exempt[, rank := frankv(mtfp, order=-1, ties.method = "min"), by="disc_yr"]
# 
# eplot_line(res_non_exempt, "disc_yr", "mtfp", groupby="edb", x_lab="") |>
#   e_legend(
#     orient = 'vertical',
#     right = 0,
#     top = "middle"
#   ) |>
#   e_grid(right = 200, left=50, bottom=30) |>
#   e_title(paste0("MTFP (", my_status, ")")) |>
#   e_facet(rows=5, cols=5)
# 
# ?e_facet

