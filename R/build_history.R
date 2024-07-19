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
#'
#' @return New historical dataframe/tibble updated for this analysis round.
#' @export
#'
#' @examples
#' # (example does not account for additional global environments required)
#'
#' old <- dplyr::tibble(fin_year = c("2021/22", "2022/23"), kpi = "2.2", hbres = "Lothian", value = c(1,2))
#' new <- dplyr::tibble(fin_year = "2023/24", kpi = "2.2", hbres = "Lothian", value = 4)
#'
#' new_hist_db <- build_history(old, new, "2")
#'
#' print(new_hist_db)
#'
#' # A tibble: 3 × 4
#' #    fin_year     kpi      hbres     value
#' #     <fct>      <chr>     <fct>     <dbl>
#' # 1  2021/22      2.2      Lothian     1
#' # 2  2022/23      2.2      Lothian     2
#' # 3 2023/24       2.2      Lothian     4

build_history <- function(df_hist, df_new, kpi_number) {
  if (season == "spring") {
    table(df_hist$kpi, df_hist$fin_year)

    print("Don't add to the history file. Move along to next step")

  } else {

    if (season == "autumn") {
      # initial tests
      build_history_checks(df_hist, df_new, kpi_number)

      # create filename based on KPI inputted
      filenames <- build_history_filenames(kpi_number)


# Save historical backup --------------------------------------------------
      # read in backup, check that kpi_report_years[2] is not present
      # then save the current df_hist as the new backup file
      df_bckp <- read_rds(paste0(hist_path, filenames$filename_bckp))

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
