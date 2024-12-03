#' Create custom openxlsx functions
#'
#' Wrap-around functions for opexlsx's writeData, addStyle, mergeCells,
#' setColWidths, and setRowHeights that set the workbook name and sheet name
#'
#' @param wb_name Unquoted name of 'workbook' active binding object
#' @param sheet_name Quoted name of worksheet to add to
#'
#' @return Wrappers for 5 openxlsx functions
#' @export
#'
#' @examples
#' make_openxlsx_funcs(wb, "Sheet1")
make_openxlsx_funcs <- function(wb_name, sheet_name) {


  # writeData
  custWriteData <<- function(x, startRow, startCol, ...) {
    openxlsx::writeData(wb_name, sheet_name, x = x, startRow = startRow, startCol = startCol, ...)
    }

  # addStyle
  custAddStyle <<- function(style, rows, cols, stack = T, gridExpand = T) {
    openxlsx::addStyle(wb_name, sheet_name, style = style, rows = rows, cols = cols, stack = stack, gridExpand = gridExpand, ...)
  }

  # mergeCells
  custMergeCells <<- function(rows, cols, ...) {
    openxlsx::mergeCells(wb_name, sheet_name, rows = rows, cols = cols, ...)
  }

  # setColWidths
  custSetColWidths <<- function(cols, widths, ...) {
    openxlsx::setColWidths(wb_name, sheet_name, cols = cols, widths = widths, ...)
  }

  # setRowHeights
  custSetRowHeights <<- function(rows, heights, ...) {
    openxlsx::setRowHeights(wb_name, sheet_name, rows = rows, heights = heights, ...)
  }

}
