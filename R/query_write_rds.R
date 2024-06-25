#'Request confirmation for writing rds objects from user.
#'
#' @param df A dataframe/tibble that's to be written out.
#' @param filepath The filepath to save the object to - must include filename.rds at end.
#'
#' @return Message indicating action which has been taken.
#' @export
#'
#' @examples
#'
#'data <- data.frame(a = "1", b = "a")
#'
#'query_write_rds(data, "temp/output_data.rds")
query_write_rds <- function(df, filepath) {

df_name <- deparse(substitute(df))

user_in <- svDialogs::dlgInput(paste0("Do you want to save the '", df_name,
                                      "' output? Doing so will overwrite previous version. ",
                                      "Enter 'yes' or 'no' below."))$res

if (user_in == "yes"){
  readr::write_rds(df, filepath)
  print("Output saved, carry on")
} else {
  if (user_in == "no"){
    print("No output saved, carry on")
  } else {
    stop("Check your answer is either 'yes' or 'no' please")
  }
}}
