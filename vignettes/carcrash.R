## ----setup, include = FALSE----------------------------------------------
library(carcrashassignment)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = system.file("data", package = "carcrashassignment"))

## ----load_data-----------------------------------------------------------
make_filename(2014)
file <- file.path(system.file("data/", package = "carcrashassignment"), make_filename(2014))
data <- fars_read(file)
class(data)
str(data)

## ----read_year-----------------------------------------------------------
data <- fars_read_years(years = c(2014, 2015))

## ----summary-------------------------------------------------------------
summary <- fars_summarize_years(years = c(2014, 2015))
knitr::kable(summary)

## ----state---------------------------------------------------------------
fars_map_state(state.num = 12, year = 2014)

