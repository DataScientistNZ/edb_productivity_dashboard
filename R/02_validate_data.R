library(ggplot2)
options(scipen=999)

dt <- fread(file.path("data", "extracted_data.csv"))

# compute either the sum or the weighted average to aggregate the data for the whole industry
group_variables <- c("disc_yr", "edb", "status")
ignore_variables <- c("ppi_real", "lci_real", "cgpi_real")
all_edb_variables <- setdiff(names(dt), c(group_variables, ignore_variables))
w_avg_variables <- c("saidi_unplanned", "saidi_planned", "saidi_total_norm", 
                     "saidi_unplanned_norm", "saidi_unplanned_defective_equipment")
dt_all <- merge(dt[, lapply(.SD, sum), by=disc_yr, .SDcols=setdiff(all_edb_variables, w_avg_variables)],
                dt[, lapply(.SD, weighted.mean, nb_connections), by=disc_yr, .SDcols=w_avg_variables])

l_plots <- list()
for (nm in all_edb_variables) {
  l_plots[[nm]] <- ggplot(dt_all, aes(x=disc_yr, y=get(nm))) +
    geom_line(color="steelblue", linewidth=1.5) + theme_minimal() + ylab(nm) + ggtitle(nm) + 
    scale_x_continuous(breaks = scales::pretty_breaks()) + ylim(c(0, NA))
  print(l_plots[[nm]])
}


# todo: export/save plots

