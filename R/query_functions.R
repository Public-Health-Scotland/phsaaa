# functions which use dlgInput 'query' input boxes
# AMc 2024
# PWB 4.1.2


# query_function ----------------------------------------------------------

#' Request confirmation for evaluating expression from user.
#'
#' @param expression An code snippet
#'
#' @return Evaluation of code snippet
#' @export
#'
query_function <- function(expression) {

  expression_name <- deparse(substitute(expression))

  user_in <- svDialogs::dlgInput(paste0("Do you want to evaluate the following expression: '",
                                        expression_name,
                                        "'? Enter 'yes' or 'no' below."))$res

  if (user_in == "yes"){
    eval(substitute(expression), envir = .GlobalEnv)


    print("Expression evaluated, carry on")

  } else {
    if (user_in == "no"){

      print("No expression evaluated, carry on")

    } else {
      stop("Check your answer is either 'yes' or 'no' please")
    }
  }
}



# query_saveWorkbook ------------------------------------------------------

#' Request confirmation for writing Excel workbooks from user.
#'
#' @param wb Workbook name that is being created using openxlsx package.
#' @param filepath Filepath to save output
#'
#' @return Prints confirmation of either writing or skipping.
#' @export
#'
query_saveWorkbook <- function(wb, filepath) {

  wb_name <- deparse(substitute(wb))

  user_in <- svDialogs::dlgInput(paste0("Do you want to save the '", wb_name,
                                        "' output? Doing so will overwrite previous version. ",
                                        "Enter 'yes' or 'no' below."))$res

  if (user_in == "yes"){

    openxlsx::saveWorkbook(wb,
                           filepath,
                           overwrite = TRUE)
    print("Output saved, carry on")

  } else {
    if (user_in == "no"){

      print("No output saved, carry on")

    } else {
      stop("Check your answer is either 'yes' or 'no' please")
    }
  }
}


# query_write_rds ---------------------------------------------------------

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

