library(data.table)
library(ggplot2)
options(scipen=999)
set.seed(6)

nb_firms <- 20
efficiencies <- round(rnorm(nb_firms, 1, 0.15), 4)
# efficiencies <- c(1., 1.1, 0.9, 0.8, 1.2, 1.05, 0.95, 1.15, 0.85)

size_proxy <- sample(1:nb_firms, length(efficiencies))
firms <- paste0("firm_", toupper(letters[1:length(efficiencies)]))
stopifnot(length(efficiencies) <= length(letters))
stopifnot(length(efficiencies) == length(size_proxy))

years <- 2021:2023

# number of cost drivers implicit from their sensitivities
cost_drivers_sensitivities <- c(0.25, 0.02, 0.05, 0.5)
cost_driver_avg_growth <- 0.0

# make sure implied economy of scales are reasonable
stopifnot(sum(cost_drivers_sensitivities) <= 1.05)
stopifnot(sum(cost_drivers_sensitivities) >= 0.8)

# mc accumulator for efficiency indices 
mc_cum1 <- numeric(length(firms) * length(years))
mc_cum2 <- numeric(length(firms) * length(years))

# mc accumulators for omit model sensitivities 
mc_coef_cum1 <- numeric(length(cost_drivers_sensitivities) - 1)
mc_coef_cum2 <- numeric(length(cost_drivers_sensitivities) - 1)

nb_runs <- 200
l_tmp <- list()
err <- numeric(length(firms) * length(years))
corrr <- 0
for (mc_i in 1:nb_runs) {
  
  if (mc_i %% 100 == 0) {
    cat(paste0("runs: ", mc_i, "\n"))
  }
  
  # prepare cost driver variables
  dt <- data.table(firm=character(), year=integer(), cost_driver_id=integer(), cost_driver_value=numeric())
  for (firm_i in 1:length(firms)) {
    for (cost_driver_i in 1:length(cost_drivers_sensitivities)) {
      cur_cost_driver_value <- runif(1, min=40, max=100) * size_proxy[firm_i]
      for (year in years) {
        cur_cost_driver_value = cur_cost_driver_value * rnorm(1, 1, 0.05) * (1 + cost_driver_avg_growth)
        dt_tmp <- data.table(firm=firms[firm_i], year=year, 
                             cost_driver_id=paste0("cost_driver_", cost_driver_i),
                             cost_driver_value=cur_cost_driver_value)
        dt <- rbind(dt, dt_tmp)
      }
    }
  }
  
  # dt
  dt <- dcast(dt, firm + year ~ cost_driver_id, value.var = "cost_driver_value")
  # dt
  
  c_0 <- 100.
  dt[, fair_cost := c_0 * exp(cost_drivers_sensitivities[1] * log(cost_driver_1) +
                                cost_drivers_sensitivities[2] * log(cost_driver_2) +
                                cost_drivers_sensitivities[3] * log(cost_driver_3) +
                                cost_drivers_sensitivities[4] * log(cost_driver_4))]
  
  for (firm_i in 1:length(firms)) {
    dt[firm==firms[firm_i], actual_efficiency := efficiencies[firm_i]]
    dt[firm==firms[firm_i], actual_cost := fair_cost / actual_efficiency]
  }
  
  # calibrate perfect model
  formula_str <- paste0("I(log(fair_cost)) ~ ", 
                        paste0("I(log(", paste0("cost_driver_", 1:length(cost_drivers_sensitivities)), "))", collapse = " + "))
  perfect_model <- glm(data=dt, formula=formula_str)
  perfect_model
  
  # calibrate good model
  formula_str <- paste0("I(log(actual_cost)) ~ ", 
                        paste0("I(log(", paste0("cost_driver_", 1:length(cost_drivers_sensitivities)), "))", collapse = " + "))
  good_model <- glm(data=dt, formula=formula_str)
  good_model
  
  # calibrate model omitting one variable
  formula_str <- paste0("I(log(actual_cost)) ~ ", 
                        paste0("I(log(", paste0("cost_driver_", 1:(length(cost_drivers_sensitivities)-1)), "))", collapse = " + "))
  omitting_model <- glm(data=dt, formula=formula_str)
  omitting_model
  
  dt[, assessed_cost_omit := exp(predict(omitting_model, dt, type="response"))]
  dt[, assessed_efficiency_omit := assessed_cost_omit/actual_cost]
  
  # v <- dt[["assessed_efficiency_omit"]] - dt[["actual_efficiency"]]
  v <- dt[["assessed_efficiency_omit"]]
  mc_cum1 <- mc_cum1 + v
  mc_cum2 <- mc_cum2 + v^2
  
  l_tmp[[mc_i]] <- mean(dt[firm == firms[2] & year == 2023][["assessed_efficiency_omit"]])
  
  u <- omitting_model$coefficients[2:(length(cost_drivers_sensitivities)-1+1)]
  mc_coef_cum1 <- mc_coef_cum1 + u
  mc_coef_cum2 <- mc_coef_cum2 + u^2
  
  err <- err + abs(dt[["assessed_efficiency_omit"]] - dt[["actual_efficiency"]])
  corrr <- corrr + cor(dt[["assessed_efficiency_omit"]], dt[["actual_efficiency"]])
}

mc_means <- mc_cum1 / nb_runs
mc_std <- sqrt(mc_cum2 / nb_runs - mc_means ^ 2)
confidence <- 1.96 * mean(mc_std) / sqrt(nb_runs)

mc_means - dt[["actual_efficiency"]]
mc_std
confidence 

mc_coef_means <- mc_coef_cum1 / nb_runs
mc_coef_std <- sqrt(mc_coef_cum2 / nb_runs - mc_coef_means ^ 2)
coef_confidence <- 1.96 * mean(mc_coef_std) / sqrt(nb_runs)

mc_coef_means
mc_coef_std
coef_confidence

v <- as.numeric(l_tmp)
ggplot(data.table(values=v), aes(x = values, y = ..count../sum(..count..))) + 
  geom_histogram(binwidth = 0.02, fill = "skyblue", color="black") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = paste0("Histogram of assessed productivity values - ", firms[2], " (actual ", 
                      efficiencies[2],")"), 
       x = "assessed productivity idx", y = "Percentage") +
  xlim(min(c(v, efficiencies[2]-0.3)), max(c(v, efficiencies[2]+0.3))) +
  theme_minimal()

(sum(v > efficiencies[2]+cost_drivers_sensitivities[length(cost_drivers_sensitivities)]) +
  sum(v < efficiencies[2]-cost_drivers_sensitivities[length(cost_drivers_sensitivities)])) / nb_runs

(sum(v > efficiencies[2]+2*cost_drivers_sensitivities[length(cost_drivers_sensitivities)]) +
    sum(v < efficiencies[2]-2*cost_drivers_sensitivities[length(cost_drivers_sensitivities)])) / nb_runs


meh <- sapply(0:(length(firms)-1),
                function(i) round(mean(dt[["assessed_efficiency_omit"]][(i*length(years)+1):((i+1)*length(years))]), 4))
meh <- setNames(meh, efficiencies)
cor(meh, as.numeric(names(meh)))
mean(abs(meh - as.numeric(names(meh))))


means <- sapply(0:(length(firms)-1), 
                function(i) round(mean(mc_means[(i*length(years)+1):((i+1)*length(years))]), 4))
named_means <- setNames(means, efficiencies)

named_means
mean(abs(efficiencies - named_means))
cor(efficiencies, named_means)
mean(mc_std)

# dt

err <- err / nb_runs
corrr <- corrr / nb_runs
mean(err)
mean(corrr)
# dt[, c("firm", "year", "actual_efficiency", "assessed_efficiency_omit"), with=F]
