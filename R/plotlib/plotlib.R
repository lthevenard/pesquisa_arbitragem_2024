LOCATION <- "R/plotlib/"

IMPORT <- c(
  "colors",
  "barplot/plot_bar",
  "barplot/plot_fillbar",
  "barplot/plot_col",
  "barplot/plot_fillcol",
  "barplot/barplot_utils"
)

LOCATION <- ifelse(
  grepl(pattern="/$", x=LOCATION),
  LOCATION,
  paste0(LOCATION, "/")
)

for (script in IMPORT) {
  source(paste0(LOCATION, script, ".R"))
}

rm(LOCATION, IMPORT)
