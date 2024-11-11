source("R/00_plots.R")

# create an artificial dataset with same content as input dt, but adds aggregation
# for all industry and for exempt vs non exempt
# assume dt loaded as below
# dt <- fread(file.path("data", "extracted_data.csv"))
aggregate_data_by <- function(dt, by, w_avg_variables=NULL) {
  # compute either the sum or the weighted average to aggregate the data for the whole industry
  group_variables <- c("disc_yr", "edb", "status")
  # stopifnot(all(by %in% group_variables))
  
  if (is.null(w_avg_variables)) {
    inflation_variables <- c("ppi_real", "lci_real", "cgpi_real")
    w_avg_variables <- c("saidi_unplanned", "saidi_planned", "saidi_total_norm", 
                         "saidi_unplanned_norm", "saidi_unplanned_defective_equipment", inflation_variables)
  }
  
  all_edb_variables <- setdiff(names(dt), group_variables)
  merge(dt[, lapply(.SD, sum), by=by, .SDcols=setdiff(all_edb_variables, unique(c(by, w_avg_variables)))],
        dt[, lapply(.SD, weighted.mean, nb_connections), by=by, .SDcols=w_avg_variables])
}

# generate a list of plots, one per display variable
# assume the data has a disc_yr column for all points
# [todo: the plots should all be isolated to allow a later conversion to 
# nicer library than ggplot allowing hover text]
generate_visualisation_plots <- function(dt, display_variables=NULL, filters=NULL, 
                                         x_nm="disc_yr"){
  if (is.null(display_variables)) {display_variables <- names(dt)}
  stopifnot(all(display_variables %in% names(dt)))
  if (!is.null(filters)) {
    stopifnot(all(names(filters) %in% names(dt)))
    for (f_nm in names(filters)) {
      dt <- dt[get(f_nm) %in% filters[[f_nm]]]
    }
  }
  l_plots <- sapply(display_variables, function(nm) {
    generate_plot_time_series(dt, nm, x_nm=x_nm)
    # ggplot(dt, aes(x=get(x_nm), y=get(nm))) + xlab(x_nm) +
    #   geom_line(color="steelblue", linewidth=1.5) + theme_minimal() + ylab(nm) + ggtitle(nm) +
    #   scale_x_continuous(breaks = scales::pretty_breaks()) + ylim(c(0, 1.25*max(dt[[nm]])))
  }, simplify = F, USE.NAMES = T)
  l_plots
}

# generate a grid of boxes, with n_columns
# each box contains a plotOutput with a key plot_<display_variable>
# with <display_variable> in <display_variables>
# [can't believe it's THAT painful to code that in R/shiny ><]
generate_matrix_plots_shiny_display <- function(display_variables, n_columns=2) {
  lapply(1:(length(display_variables) %/% n_columns + ifelse(length(display_variables) %% n_columns==0, 0, 1)), function(i) {
    if (i <= length(display_variables) %/% n_columns) {
      fluidRow(lapply(1:n_columns, function(j) box(
        plotOutput(paste0("plot_", display_variables[(i-1)*n_columns+j])), width = 12 %/% n_columns)))
    } else {
      fluidRow(lapply(1:(length(display_variables) %% n_columns), function(j) box(
        plotOutput(paste0("plot_", display_variables[(length(display_variables) %/% n_columns)*n_columns+j])), 
        width= 12 %/% n_columns)))
    }
  })
}