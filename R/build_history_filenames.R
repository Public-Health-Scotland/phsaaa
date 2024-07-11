#' Create filepaths for saving outputs of build_history() function.
#'
#' @param kpi_number Gets passed from the build_history() function, 1.1-1.3/1.4/2/3 as options
#'
#' @return List of variables, important ones are 1 filename_bckp and 2 filename_hist
#'
build_history_filenames <- function(kpi_number) {
  historical <- "/aaa_kpi_historical_"
  bckp <- "_bckp.rds"
  reg <- ".rds"

  theme <- paste0("theme", as.numeric(substr(kpi_number, 1, 1))+1)
  filename_bckp <- paste0(historical, theme, bckp) # backup file
  filename_hist <- paste0(historical, theme, reg) # new historical db

  return(list(filename_bckp = filename_bckp, filename_hist = filename_hist, theme = theme, historical = historical, bckp = bckp, reg = reg))
  }
