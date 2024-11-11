library(ggplot2)
library(data.table)
options(scipen=999)

dt <- fread(file.path("data", "extracted_data.csv"))
names(dt)

group_variables <- c("disc_yr", "edb", "status")

all_outputs <- c("nb_connections", "length_circuit", "length_overhead", 
                 "length_underground", "mva_circuit", "mva_overhead", "mva_underground",
                 "transformers", "energy_delivered", "max_demand", "saidi_unplanned_norm")

# create a cache to store all results we intend to display on the app
# ideally obtained through functions rather than pre run...
results_cache <- list()

# Correlogram as numbers
outputs_cor <- cor(dt[disc_yr==2023, all_outputs, with=F])
corrplot::corrplot(outputs_cor, method="number")

models <- list()
models[["icp"]] <- glm(data=dt,
                       "I(log(annual_charge_real)) ~ I(log(nb_connections))")
models[["icp + circuit"]] <- glm(data=dt, 
                                 "I(log(annual_charge_real)) ~ I(log(nb_connections)) + I(log(length_circuit))")
models[["icp + circuit + power"]] <- glm(data=dt, 
                                 "I(log(annual_charge_real)) ~ I(log(nb_connections)) + I(log(length_circuit)) + I(log(mva_underground))")
summary(models[["icp"]])
summary(models[["icp + circuit"]])
summary(models[["icp + circuit + power"]])

modelsummary::modelsummary(models[["icp + circuit + power"]])
modelsummary::modelsummary(models, output="data.frame", stars = T, statistic = NULL)
modelsummary::modelsummary(models, output="default")

picked_input <- "opex_real"
picked_outputs <- c("nb_connections", "length_circuit")

formula_str <- paste0("I(log(", picked_input, ")) ~ ", paste0("I(log(", picked_outputs, "))", collapse = " + "))
m <- glm(data=dt, formula_str)
dt_res <- setnames(as.data.table(
  modelsummary::modelsummary(m, output="data.frame", estimate="{estimate}{stars} ({std.error})", 
                             statistic = NULL, coef_rename = F)), "(1)", "value")[]

results_cache[["dt_models"]] <- modelsummary::modelsummary(
  models, output="data.frame", statistic = NULL, estimate="{estimate}{stars} ({std.error})")
results_cache[["dt_models"]]

# work on artificial forecast data that does not allow to account for time (which is allocated to inefficiency)
# we store the actual year in `year` but use the base year to make a forecast
dt_f <- data.table(dt)[, year := disc_yr][, disc_yr := min(dt$disc_yr)] # disc_yr is set to beginning of the period
for (m_nm in names(models)) {
  dt_f[, (paste0("model_outputs_value_[", m_nm, "]")) := exp(predict(models[[m_nm]], newdata=dt_f, type="response"))]
  dt_f[, (paste0(m_nm)) := get(paste0("model_outputs_value_[", m_nm, "]"))/get("annual_charge_real")]
}

group_variables_ <- c("year", "edb", "status")
dt_prod_idx <- melt(dt_f, id.vars=group_variables_, measure.vars = paste0(names(models)), 
                     value.name = "prod_index", variable.name = "outputs")

blue_colors <- c("#2596a1", "#0d72a6", "#49b7d2", "#93dde6", "steelblue1", "steelblue4", 
                 "cyan2", "#044a85", "#c6edec", "#173d64")

results_cache[["pl_prod_idx_edb_nonexempt"]] <- ggplot(
  dt_prod_idx[status == "NonExempt"], aes(x=year, y=prod_index, color=outputs, group=outputs)) +
  geom_line(linewidth=1.2, alpha=0.6) + 
  ggtitle(paste0("Productivity index (", "NonExempt", ")")) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  ylim(c(0, 1.1*max(dt_prod_idx$prod_index))) +
  theme_minimal() +
  # theme_bw() + 
  # theme(strip.background=element_rect(colour="black", fill="#ffe6cc")) +
  facet_wrap(~edb) + scale_color_discrete(type=blue_colors) 
results_cache[["pl_prod_idx_edb_nonexempt"]]

results_cache[["pl_prod_idx_edb_exempt"]] <- ggplot(
  dt_prod_idx[status == "Exempt"], aes(x=year, y=prod_index, color=outputs, group=outputs)) +
  geom_line(linewidth=1.2, alpha=0.6) + 
  ggtitle(paste0("Productivity index (", "Exempt", ")")) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  ylim(c(0, 1.1*max(dt_prod_idx$prod_index))) +
  theme_minimal() +
  # theme_bw() + 
  # theme(strip.background=element_rect(colour="black", fill="#ffe6cc")) +
  facet_wrap(~edb) + scale_color_discrete(type=blue_colors) 
results_cache[["pl_prod_idx_edb_exempt"]] 


picked_outputs <- "icp + circuit + power"
results_cache[["pl_prod_idx_edb_frontier"]] <- ggplot(dt_f[year== 2023], 
       aes(x=log(annual_charge_real), y=log(get(paste0("model_outputs_value_[", picked_outputs,"]"))))) +
  ylab(paste0("outputs [", picked_outputs, "] (log)")) + xlab("annual_charge_real (log)") + 
  geom_point(color="steelblue") + theme_minimal()
results_cache[["pl_prod_idx_edb_frontier"]]

ggplot(dt_f[year== 2023], 
       aes(x=annual_charge_real, y=get(paste0("model_outputs_value_[", picked_outputs,"]")))) +
  ylab(paste0("outputs [", picked_outputs, "] (log)")) + xlab("annual_charge_real (log)") + 
  geom_point() + theme_minimal()


#### Same for overall industry
## Please note there is an "issue" with economy of scales
## if a model expects an economy of scale (sum of coef < 1)
## then summing all industry together should bring a much better efficiency
## --> a model with a high economy of scale always shows the industry performs "poorly" when the industry is grouped
## the below offers a way to compensate for that by summing the model values of outputs for edbs considering their size
## and summing their inputs. Then the ratio will be computed on that.
## It's different because the value of the sum of inputs is less than the sum of the value of outputs, because of the economy
## of scale. But it's unfair to monitor the whole industry expecting such scale... one can refuse the offered alteration
## to illustrate what is the actual efficiency of NZ industry (low) compared to what it *could* be according to the model with 
## unified EDB


all_edb_variables <- setdiff(names(dt_f), group_variables_)
w_avg_variables_ <- c("saidi_unplanned", "saidi_planned", "saidi_total_norm", "saidi_unplanned_norm", 
                     "cgpi_real", "ppi_real", "lci_real", names(models))
dt_all_f <- rbind(
  merge(dt_f[, lapply(.SD, sum), by="year", .SDcols=setdiff(all_edb_variables, w_avg_variables_)],
        dt_f[, lapply(.SD, weighted.mean, nb_connections), by="year", .SDcols=w_avg_variables_])[, status:="All"][],
  merge(dt_f[, lapply(.SD, sum), by=c("status", "year"), .SDcols=setdiff(all_edb_variables, w_avg_variables_)],
        dt_f[, lapply(.SD, weighted.mean, nb_connections), by=c("status", "year"), .SDcols=w_avg_variables_]))

# overwrite model outputs to cancel the expected economy of scale
# i.e. we sum the model fair values of outputs when computed at the edb level
for (m_nm in names(models)) {
  dt_all_f[, (paste0(m_nm)) := get(paste0("model_outputs_value_[", m_nm, "]"))/get("annual_charge_real")]
}

dt_prod_idx_all <- melt(dt_all_f, id.vars=c("year", "status"), measure.vars = paste0(names(models)), 
                             value.name = "prod_index", variable.name = "outputs")


results_cache[["pl_prod_idx"]] <- ggplot(dt_prod_idx_all, aes(x=year, y=prod_index, color=outputs, group=outputs)) +
  geom_line(linewidth=1.2, alpha=0.6) + 
  ggtitle(paste0("Productivity index")) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  ylim(c(0, 1.2*max(dt_prod_idx_all$prod_index))) +
  theme_minimal() +
  # theme_bw() + 
  # theme(strip.background=element_rect(colour="black", fill="#ffe6cc")) +
  facet_wrap(~status) + scale_color_discrete(type=blue_colors) 
results_cache[["pl_prod_idx"]]

