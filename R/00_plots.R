# This script contains the plots the shiny app uses
# ideally would be replaced by something a bit more dynamic that
# allows hover text (e_charts...)

# simple plot that displays time-series
generate_plot_time_series <- function(dt, y_nm, x_nm="disc_yr") {
  ggplot(dt, aes(x=get(x_nm), y=get(y_nm))) + xlab(x_nm) +
    geom_line(color="steelblue", linewidth=1.5) + theme_minimal() + ylab(y_nm) + ggtitle(y_nm) +
    scale_x_continuous(breaks = scales::pretty_breaks()) + ylim(c(0, 1.25*max(dt[[y_nm]])))
}
