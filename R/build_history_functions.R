# build_history() function plus its helpers
# AMc 2024
# PWB 4.1.2

# build_history -----------------------------------------------------------

#' Create backup and update new historical database of AAA screening KPIs, only runs if season is "autumn"
#'
#' Function requires multiple global environment variables to be specified:
#' 'season' - either 'autumn' or 'spring'
#' 'hist_path' - path to directory where historical files are to be stored
#' 'kpi_report_years' - vector containing 3 character strings of financial years - third is the FY due to be published next
#' 'fy_list' - list of financial years covering time since AAA screening started, in chronological order
#' 'hb_list' - list of NHS Health Boards in alphabetical order
#'
#' @param df_hist Dataframe/tibble which contains historical data for relevant KPI.
#' @param df_new Dataframe/tibble containing new data for relevant KPI.
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#' @param season Season of analysis, options are "Spring" or "Autumn"
#' @param kpi_report_years Character vector, must be >= 3 length. Includes financial year being published, plus two prior.
#' @param fy_list Character vector of financial years in data, in chronological order
#' @param hb_list Character vector of Health Board names, in alphabetic order
#' @param year_n Used for KPIs 1.1-1.3 or 1.4, character variable of a financial year. Default is 'year2', which corresponds to the financial year following that which is being published.
#' @param hist_path Filepath that points to the output directory - this is where previous historical data exists.
#'
#' @return New historical dataframe/tibble updated for this analysis round.
#' @export
#'
#' @examples
#'
#'
#' old <- dplyr::tibble(fin_year = c("2021/22", "2022/23"), kpi = "2.2", hbres = "Lothian", value = c(1,2))
#' new <- dplyr::tibble(fin_year = "2023/24", kpi = "2.2", hbres = "Lothian", value = 4)
#'
#' new_hist_db <- build_history(old, new, "2", season = "autumn", kpi_report_years = c("2021/22", "2022/23", "2023/24"), fy_list = fy_list, hb_list = hb_list, year_n = "2024/25")
#'
#' print(new_hist_db)
#'
#' # A tibble: 3 × 4
#' #    fin_year     kpi      hbres     value
#' #     <fct>      <chr>     <fct>     <dbl>
#' # 1  2021/22      2.2      Lothian     1
#' # 2  2022/23      2.2      Lothian     2
#' # 3 2023/24       2.2      Lothian     4

build_history <- function(df_hist,
                          df_new,
                          kpi_number,
                          season,
                          kpi_report_years,
                          fy_list,
                          hb_list,
                          hist_path) {
  if (season == "spring") {
    table(df_hist$kpi, df_hist$fin_year)

    print("Don't add to the history file. Move along to next step")

  } else {

    if (season == "autumn") {
      # initial tests
      build_history_checks(kpi_number)

      # create filename based on KPI inputted
      filenames <- build_history_filenames(kpi_number)


      # Save historical backup --------------------------------------------------
      # read in backup, check that kpi_report_years[2] is not present
      # then save the current df_hist as the new backup file
      df_bckp <- readr::read_rds(paste0(hist_path, filenames$filename_bckp))

      if(!kpi_report_years[2] %in% df_bckp$fin_year & !kpi_number == "1.4"){
        # write backup file
        query_write_rds(df_hist, paste0(hist_path, filenames$filename_bckp))
        # change permissions to give the group read/write
        Sys.chmod(paste0(hist_path, filenames$filename_bckp),
                  mode = "664", use_umask = FALSE)

        print("Backup of historical database written.")
      } else {
        print("Backup already created for this analysis round.")
      }

      # format df_new for inclusion
      df_new_filtered <- build_history_format_df_new(kpi_number, df_new)

      print("Table of df_new_filtered$kpi, df_new_filtered$fin_year:")
      print(table(df_new_filtered$kpi, df_new_filtered$fin_year))


      # New historical database -------------------------------------------------

      # create new historical database
      new_hist_db <- build_history_create_new_hist(df_hist, df_new_filtered, kpi_number)

      print("Table of new_hist_db$kpi, new_hist_db$fin_year:")
      print(table(new_hist_db$kpi, new_hist_db$fin_year))

      # write new hist_db
      query_write_rds(new_hist_db, paste0(hist_path, filenames$filename_hist))
      # change permissions to give the group read/write
      Sys.chmod(paste0(hist_path, filenames$filename_hist),
                mode = "664", use_umask = FALSE)

      print("You made history! Proceed.")

    } else {

      stop("Season is not 'spring' or 'autumn'. Go check your calendar!")
    }
  }
}


# build_history_checks ----------------------------------------------------

#' Check global environment for build_history() function.
#'
#' Ensures all variables required to run build_history() exist within the global environment.
#'
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#'
#' @return Nothing returned, just stops execution if error.
#'
#' @examples
#' build_history_checks(old, new, "2")
build_history_checks <- function(kpi_number){
  stopifnot(
    "KPI inputted is not included in accepted list, see documentation" = kpi_number %in% c("1.1-1.3", "1.4", "2", "3")
  )
  # stopifnot(
  #   "no 'fy_list' variable defined in global environment" = exists("fy_list", envir = globalenv())
  # )
  # stopifnot(
  #   "no 'hb_list' variable defined in global environment" = exists("hb_list", envir = globalenv())
  # )
  # if (kpi_number %in% c("1.1-1.3", "1.4")) {
  #   stopifnot(
  #     "no 'year2' variable defined in global environment" = exists("year2", envir = globalenv())
  #   )
  # }
}


# build_history_filenames -------------------------------------------------

#' Create filepaths for saving outputs of build_history() function.
#'
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
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


# build_history_format_df_new ---------------------------------------------

#' Create and format new historical database for build_history()
#'
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#' @param df_new Dataframe/tibble containing new data for relevant KPI.
#' @param year_n Used for KPIs 1.1-1.3 or 1.4, character variable of a financial year. Default is 'year2', which corresponds to the financial year following that which is being published.
#' @param kpi_report_years Character vector, must be >= 3 length. Includes financial year being published, plus two prior.
#'
#' @return New historical database in the build_history() environment
#'
build_history_format_df_new <- function(kpi_number, df_new, kpi_report_years) {

  if(kpi_number == "1.1-1.3"){
    df_new |>
      dplyr::filter(kpi != "KPI 1.1 Sept coverage",
                    fin_year != year2)
  }
  else {
    df_new |>
      dplyr::filter(fin_year == kpi_report_years[3])
  }
}


# build_history_create_new_hist -------------------------------------------

#' Helper to build_history() which creates new historical database and formats it
#'
#' @param df_hist Dataframe/tibble which contains historical data for relevant KPI.
#' @param df_new Dataframe/tibble containing new data for relevant KPI.
#' @param kpi_number KPI being added to the historical database, options are: "1.1-1.3", "1.4", "2", or "3"
#' @param fy_list Character vector of financial years in data, in chronological order
#' @param hb_list Character vector of Health Board names, in alphabetic order
#'
#' @return Creates new hist_db within build_history() environment
#'
build_history_create_new_hist <- function(df_hist, df_new, kpi_number, fy_list, hb_list) {

  new_hist_db <- add_new_rows(df1 = df_hist, df2 = df_new, fin_year, kpi) |>
    dplyr::mutate(fin_year = forcats::fct_relevel(fin_year, c(fy_list)),
                  hbres = forcats::fct_relevel(hbres, c(hb_list)))

  if(kpi_number == "1.1-1.3"){
    new_hist_db <- new_hist_db |>
      dplyr::mutate(
        kpi = forcats::fct_relevel(
          kpi, c("KPI 1.1", "KPI 1.1 Scotland SIMD",
                 "KPI 1.1 Scotland SIMD Sept coverage",
                 "KPI 1.2a", "KPI 1.2a Sept coverage",
                 "KPI 1.2b", "KPI 1.3a Scotland SIMD",
                 "KPI 1.3a Sept coverage", "KPI 1.3a HB SIMD",
                 "KPI 1.3b Scotland SIMD", "KPI 1.3b HB SIMD",
                 "KPI 1.4a", "KPI 1.4b")))
  }

  new_hist_db |>
    dplyr::arrange(kpi, fin_year, hbres)

}

