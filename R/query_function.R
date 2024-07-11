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
