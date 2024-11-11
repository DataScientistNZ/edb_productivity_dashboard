# :chart_with_upwards_trend: :bulb: EDB Productivity :bulb: :chart_with_upwards_trend:
:chart_with_upwards_trend: EDB Productivity and Benchmarking. 

:electric_plug: Assess and compare the productivity of local electricity network providers in New Zealand.

This project is freely inspired from this [project](https://comcom.govt.nz/regulated-industries/electricity-lines/electricity-distributor-performance-and-data/productivity-and-efficiency-study-of-electricity-distributors) delivered by CEPA.

It is a personal project made in my free time to understand underlying patterns better, and as well to work on a way to share findings efficiently through technology.
There is a corresponding shiny interface that will be made available here soon, its development is still being worked on at the moment.

## Data

All the involved data for this project can be found on the New Zealand Commerce Commission [website](https://comcom.govt.nz/regulated-industries/electricity-lines/electricity-distributor-performance-and-data/information-disclosed-by-electricity-distributors).

## Developers

First, clone this project locally. 

Copy the parquet file (EDB_ID__Full__2024.05.01.parquet) in the ```data``` folder.

Then install the renv library:
```
install.packages("renv")
```
Finally you can install the libraries which have been saved:
```
renv::install()
```
