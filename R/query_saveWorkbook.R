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
