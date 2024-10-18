#' Create individual reports of GP coverage and self-referrals for Health Boards
#'
#' @param hb_name Character var of single Health Board
#' @param financial_year Character vector of financial year (long format 20XX/YY)
#' @param coverage_data Coverage data for current FY and previous FY, by GP practice
#' @param selfref_data Self-referrals data for current FY, by GP Practice
#' @param output_filepath Filepath to output folder (general)
#' @param date_aaa_extracted Date on which AAA extract was downloaded from Atos
#' @param date_gp_extracted Date on which GP history files were refreshed and downloaded from BOXI
#'
#' @return Saves Excel output for individual Health Board to 'GP Practices' folder at the specified output location.
#' @export
#'
#' @examples
#' create_aaa_gp_outputs(hb_name = "Borders",
#'                       financial_year = "2023/24",
#'                       coverage_data = gp_data,
#'                       selfref_data = sr_data,
#'                       output_filepath = "AAA/Topics/Screening/KPI/202409/output",
#'                       date_aaa_extracted = "1 September",
#'                       date_gp_extracted = "15 October")
create_aaa_gp_outputs <- function(hb_name, financial_year, coverage_data, selfref_data, output_filepath, date_aaa_extracted, date_gp_extracted) {

  # 1. text and styles inputs ----

  ## text
  text <- list()

  ### KPI 1.2a/coverage - specific texts
  text$cov_title <- "KPI 1.2a: Percentage of eligible poulation who are tested before age 66 years 3 months by GP practice"
  text$cov_heads <- c("GP Code", "GP Practice Name", paste0("Turned 66 in year ending 31 March ", year_ww), paste0("Turned 66 in year ending 31 March ", year_xx))
  text$cov_subheads <- c("Men eligible", "Tested before age 66 years 3 months", "N", "N", "%")
  text$cov_key <- c("Key", "Essential threshold not met", "Essential ≥ 75%", "Desirable ≥ 85%")
  text$cov_notes <- data.frame(
    title = c("Notes",
              "1. Number of men eligible:",
              "2. Tested before age 66 and 3 months: ",
              "3. GP practice: "),
    content = c("",
                paste0("Men turning age 66 in the financial year ending 31 March ", year_xx, ". Men become eligible for screening when they reach age 65 and should be invited for screening before their 66th birthday."),
                paste0("Men are counted as having been tested if they were screened before they reached the age of 66 and 3 months and had a screening result of positive, negative or non-visualisation. Men who attended clinics and were not screened due to technical failure are not included."),
                paste0("Data are based on the GP practice the man was registered at on 31 March ", year_xx, " and so may not always reflect the practice the man was registered with when invited or screened. Men may attend a GP practice outside the NHS Board that they are resident in. These men are included in the 'Practice outside NHS Board area' figures. For a small number of men, GP practice of registration is unknown. GP practice data has been extracted from the AAA screening business objects data universe (at ", date_gp_extracted, " ", year_xx, ") and mapped to the PHS extract (from ", date_aaa_extracted, " ", year_xx, ").")))
  ### self-referrals - specific texts
  text$sr_title <- "Self-referrals: Number of men tested by GP practice"
  text$sr_heads <- c("Practice Code", "Practice Name", paste0("Tested in year ending 31 March ", year_xx))
  text$sr_notes <- data.frame(
    title = c("Notes",
              "1. Self-referral: ",
              "2. Men tested: ",
              "3. GP practice: ",
              "4. Multiple initial screens: "),
    content = c("",
                "Any man over the age of 65 who has not previously been screened and who contacts their local AAA screening centre directly to request screening.",
                "Men are counted as tested if they have an initial screening result of positive, negative or non-visualisation. Men who attended clinics and were not screened due to technical failure are not included.",
                paste0("Data for year ending 31 March ", year_xx, " are based on the GP practice that the individual was registered at on 31 March ", year_xx, ". This means the data may not always reflect the practice the man was registered with when invited or screened. Men may attend a GP practice outside the NHS Board that they are resident in. These men are included in the 'Practice outside NHS Board area' figures. For a small number of men, GP practice of registration is unknown. GP practice data has been extracted from the AAA screening business objects data universe (at ", date_gp_extracted, " ", year_xx, ") and mapped to the PHS extract (from ", date_aaa_extracted, " ", year_xx, ")."),
                "Self-referrals who had initial screens in more than one year are counted under the year of their most recent screening result only."))
  ### general text
  text$disclosure_text <- c("Practice-level data and disclosive cells",
                            "These data are released for management purposes and have not been adjusted to conform to PHS’s Statistical Disclosure Control protocol. The tables may contain disclosive cells (cells with small numbers that might enable an individual patient or member of staff to be identified, perhaps with the aid of further knowledge of the topic) and should not be published without further consideration of the contents in light of the guidance (link below). Please contact phs.aaascreenstats@phs.scot if you have any queries regarding this.")
  text$disclosure_link <- phsaaa::format_excel_hyperlink("PHS Statistical Disclosure Control", "https://publichealthscotland.scot/publications/public-health-scotland-statistical-disclosure-protocol/public-health-scotland-statistical-disclosure-protocol-version-22/")
  text$legend <- c("-   Zero", "..   Not applicable")

  ## styles
  styles <- list()

  styles$header <- openxlsx::createStyle(fontSize = 18, fontName = "Arial", textDecoration = "bold", wrapText = F)
  styles$body <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", wrapText = T)
  styles$subhead <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", textDecoration = "bold", wrapText = F)
  styles$disclosure_head <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FF0000", textDecoration = c("bold", "underline"), wrapText = F)
  styles$disclosure_body <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FF0000", textDecoration = "bold", wrapText = F)
  ### borders
  styles$b_left <- openxlsx::createStyle(border = "left")
  styles$b_top <- openxlsx::createStyle(border = "top")
  ### aligning
  styles$a_middle <- openxlsx::createStyle(valign = "center")
  styles$a_right <- openxlsx::createStyle(halign = "right")
  styles$a_centre <- openxlsx::createStyle(halign = "center")
  ### number formatting
  styles$counts <- openxlsx::createStyle(numFmt = '_(* #,##0_);_(* (#,##0);_(* "-"_);_(@_)') # adds ',' to 1000s numbers, and replaces 0s with "-"
  styles$percentages <- openxlsx::createStyle(numFmt = '[>0]0.0;[<0]"..";0.0') # positive #s have 1 decimal place, zeroes are 0.0, and minus numbers (placeholder for NAs/NaNs) are coded as ".."
  ### conditional formatting
  styles$red <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF", fgFill = "#911913")
  styles$yellow <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", fgFill = "#F7EC6D")
  styles$green <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", fgFill = "#6AB42D")
  styles$red_cond <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF", bgFill = "#911913")
  styles$yellow_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#F7EC6D")
  styles$green_cond <- openxlsx::createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#6AB42D")

  # 2. functions ----

  # AMc note: a function to create these individual functions would be useful for other projects where there are more than 2 sheets?

  ## coverage functions
  ### data writing
  write_cov_data <- function(data, row, col) {
    openxlsx::writeData(wb, "KPI 1.2a", x = data, startRow = row, startCol = col, colNames = F)
  }

  ### styling
  add_cov_style <- function(style, row, col) {
    openxlsx::addStyle(wb, "KPI 1.2a", style = style, rows = row, cols = col, stack = T, gridExpand = T)
  }

  ### merge cells
  merge_cov_cells <- function(row, col) {
    openxlsx::mergeCells(wb, "KPI 1.2a", rows = row, cols = col)
  }

  ### col widths
  set_cov_col_widths <- function(col, width) {
    openxlsx::setColWidths(wb, "KPI 1.2a", cols = col, widths = width)
  }

  ### row heights
  set_cov_row_heights <- function(row, height) {
    openxlsx::setRowHeights(wb, "KPI 1.2a", rows = row, heights = height)
  }

  ## self-referrals functions
  ### data writing
  write_sr_data <- function(data, row, col) {
    openxlsx::writeData(wb, "Self-referrals", x = data, startRow = row, startCol = col, colNames = F)
  }

  ### styling
  add_sr_style <- function(style, row, col) {
    openxlsx::addStyle(wb, "Self-referrals", style = style, rows = row, cols = col, stack = T, gridExpand = T)
  }

  ### merge cells
  merge_sr_cells <- function(row, col) {
    openxlsx::mergeCells(wb, "Self-referrals", rows = row, cols = col)
  }

  ### col widths
  set_sr_col_widths <- function(col, width) {
    openxlsx::setColWidths(wb, "Self-referrals", cols = col, widths = width)
  }

  ### row heights
  set_sr_row_heights <- function(row, height) {
    openxlsx::setRowHeights(wb, "Self-referrals", rows = row, heights = height)
  }


  # 3. data filtering ----
  ## KPI 1.2a data
  cov <- coverage_data |>
    dplyr::filter(hbres == hb_name) |>
    dplyr::select(!hbres) |>
    dplyr::mutate(gp_desc = case_when(gp_hb == "Practice outside hb area" ~ NA,
                               TRUE ~ gp_desc)) |>
    dplyr::mutate(across(contains("percent"), \(x) replace_na(x, -1000)))
  # AMc note: this done to allow these NaN/NA percentages (where cohort= 0) to be recoded as ".." in the output
  # replacing empty cells with openxlsx is not possible unfortunately

  ## SR data
  sr <- selfref_data |>
    dplyr::filter(hbres == hb_name) |>
    dplyr::select(!hbres)


  # 4: row numbers ----

  # list of row numbers for where major parts of workbook are located - means that adapting later on is made easier

  row_refs <- list()
  # coverage
  row_refs$cov_title <- 1 # title row
  row_refs$cov_heads <- 3 # headings row (usually +2 from title)
  row_refs$cov_subheads <- row_refs$cov_heads + 1
  row_refs$cov_length <- nrow(cov)
  row_refs$cov_start <- 6 # data start row (can be calculated from headings etc if necessary)
  row_refs$cov_end <- row_refs$cov_start + row_refs$cov_length - 1
  row_refs$cov_key_start <- row_refs$cov_end + 2
  row_refs$cov_legend_start <- row_refs$cov_end + 6
  row_refs$cov_disc_start <- row_refs$cov_end + 10
  row_refs$cov_notes_start <- row_refs$cov_end + 14
  # sr
  row_refs$sr_title <- 1 # title row
  row_refs$sr_heads <- 3 # headings row (usually +2 from title)
  row_refs$sr_length <- nrow(sr)
  row_refs$sr_start <- 4 # data start row (can be calculated from headings etc if necessary)
  row_refs$sr_end <- row_refs$sr_start + row_refs$sr_length -1
  row_refs$sr_legend_start <- row_refs$sr_end + 2
  row_refs$sr_disc_start <- row_refs$sr_end + 6
  row_refs$sr_notes_start <- row_refs$sr_end + 10


  # 4. writing Excel ----
  ## create workbook ----
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "KPI 1.2a")
  openxlsx::addWorksheet(wb, sheetName = "Self-referrals")

  ## KPI/Coverage workbook ----

  ### merging cells
  merge_cov_cells(row = row_refs$cov_title, col = 1:10) # title
  merge_cov_cells(row = (row_refs$cov_heads):(row_refs$cov_heads+2), col = 1) # subheads
  merge_cov_cells(row = (row_refs$cov_heads):(row_refs$cov_heads+2), col = 2)
  merge_cov_cells(row = row_refs$cov_heads, col = 3:5)
  merge_cov_cells(row = row_refs$cov_heads, col = 6:ncol(cov))
  merge_cov_cells(row = row_refs$cov_heads+1, col = 4:5) # sub subheads
  merge_cov_cells(row = row_refs$cov_heads+1, col = 7:ncol(cov))
  merge_cov_cells(row = row_refs$cov_disc_start, col = 1:2) # disclosure head
  merge_cov_cells(row = row_refs$cov_disc_start+1, col = 1:ncol(cov))
  merge_cov_cells(row = row_refs$cov_disc_start+2, col = 1:2)
  merge_cov_cells(row = row_refs$cov_notes_start+1, col = 2:ncol(cov)) # notes
  merge_cov_cells(row = row_refs$cov_notes_start+2, col = 2:ncol(cov))
  merge_cov_cells(row = row_refs$cov_notes_start+3, col = 2:ncol(cov))


  ### data: headers
  write_cov_data(text$cov_title, row = row_refs$cov_title, col = 1) # title
  write_cov_data(t(text$cov_heads[1:3]), row = row_refs$cov_heads, col = 1) # subheads
  write_cov_data(text$cov_heads[4], row = row_refs$cov_heads, col = 6)
  write_cov_data(t(text$cov_subheads[1:2]), row = row_refs$cov_heads+1, col = 3) # sub subheads
  write_cov_data(t(text$cov_subheads[1:2]), row = row_refs$cov_heads+1, col = 6)
  write_cov_data(t(text$cov_subheads[3:5]), row = row_refs$cov_heads+2, col = 3)
  write_cov_data(t(text$cov_subheads[3:5]), row = row_refs$cov_heads+2, col = 6)

  ### data: dataset
  write_cov_data(cov, row = row_refs$cov_start, col = 1)

  ### data: notes etc
  write_cov_data(text$cov_key, row = row_refs$cov_key_start, col= 1) # key
  write_cov_data(text$legend, row = row_refs$cov_legend_start, col = 1) # legend
  write_cov_data(text$disclosure_text, row = row_refs$cov_disc_start, col = 1) # disclosure head
  write_cov_data(text$disclosure_link, row = row_refs$cov_disc_start+2, col = 1)
  write_cov_data(text$cov_notes, row = row_refs$cov_notes_start, col = 1) # notes head


  ### styles: font
  add_cov_style(styles$header, row = row_refs$cov_title, col = 1) # title
  add_cov_style(styles$body, row = (row_refs$cov_heads):(row_refs$cov_notes_start+3), col = (1:ncol(cov))) # entire worksheet minus title
  add_cov_style(styles$subhead, row = row_refs$cov_start, col = 1:ncol(cov)) # total HB row
  add_cov_style(styles$subhead, row = c(row_refs$cov_key_start, (row_refs$cov_notes_start):(row_refs$cov_notes_start+3)), col = 1) # key and notes
  add_cov_style(styles$disclosure_head, row = row_refs$cov_disc_start, col = 1) # disclosure header
  add_cov_style(styles$disclosure_body, row = row_refs$cov_disc_start+1, col = 1) # disclosure body

  ### styles: border
  #### top
  # AMc note: when I tried to use c() for row and col here, it produced a corrupt output, hence doing each one individually
  add_cov_style(styles$b_top, row = row_refs$cov_heads, col = 1:ncol(cov)) # data
  add_cov_style(styles$b_top, row = row_refs$cov_subheads, col = 3:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_subheads +1, col = 3:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_start, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_end+1, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+1, col = 1:ncol(cov)) # notes
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+2, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+3, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+4, col = 1:ncol(cov))
  #### left
  add_cov_style(styles$b_left, row = (row_refs$cov_heads):(row_refs$cov_end), col = 1:(ncol(cov)+1)) # data
  add_cov_style(styles$b_left, row = (row_refs$cov_notes_start+1):(row_refs$cov_notes_start+3), col = c(1, (ncol(cov)+1))) # notes

  ### styles: text alignment
  add_cov_style(styles$a_centre, row = (row_refs$cov_heads):(row_refs$cov_subheads), col = 3:ncol(cov)) # head/subhead
  add_cov_style(styles$a_right, row = (row_refs$cov_subheads+1), col = 3:ncol(cov)) # sub subhead
  add_cov_style(styles$a_middle, row = row_refs$cov_disc_start+1, col = 1) # disclosure
  add_cov_style(styles$a_middle, row = (row_refs$cov_notes_start+1):(row_refs$cov_notes_start+4), col = 1:2) # notes

  ### styles: background fill
  add_cov_style(styles$red, row = row_refs$cov_key_start+1, col = 1) # key
  add_cov_style(styles$yellow, row= row_refs$cov_key_start+2, col = 1)
  add_cov_style(styles$green, row= row_refs$cov_key_start+3, col = 1)

  ### styles: number formatting
  add_cov_style(styles$count, row = (row_refs$cov_start):(row_refs$cov_end), col = 3:4) # counts
  add_cov_style(styles$count, row = (row_refs$cov_start):(row_refs$cov_end), col = 6:7)
  add_cov_style(styles$percentages, row = (row_refs$cov_start):(row_refs$cov_end), col = 5) # percentages
  add_cov_style(styles$percentages, row = (row_refs$cov_start):(row_refs$cov_end), col = 8)

  ### conditional formatting

  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # essential not met
                                  type = "between", rule = c(0, 74.9), style = styles$red_cond)
  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                                  type = "between", rule = c(0, 74.9), style = styles$red_cond)
  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # essential
                                  type = "between", rule = c(75, 84.9), style = styles$yellow_cond)
  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                                  type = "between", rule = c(75, 84.9), style = styles$yellow_cond)
  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # desirable
                                  type = "between", rule = c(85, 100), style = styles$green_cond)
  openxlsx::conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                                  type = "between", rule = c(85, 100), style = styles$green_cond)

  ### worksheet formatting
  openxlsx::showGridLines(wb, "KPI 1.2a", showGridLines = F)
  #### col widths
  set_cov_col_widths(col = 1, width = 30)
  set_cov_col_widths(col = 2, width = 45)
  set_cov_col_widths(col = 3:8, width = 15)
  #### row heights
  set_cov_row_heights(row = row_refs$cov_subheads, height = 33) # subheadings
  set_cov_row_heights(row = row_refs$cov_disc_start+1, height = 70) # disclosure
  set_cov_row_heights(row = row_refs$cov_notes_start+1, height = 45) # notes
  set_cov_row_heights(row = row_refs$cov_notes_start+2, height = 38)
  set_cov_row_heights(row = row_refs$cov_notes_start+3, height = 80)




  ## Self-referrals workbook ----

  ### merging cells
  merge_sr_cells(row = row_refs$sr_title, col = 1:ncol(sr)) # title
  merge_sr_cells(row = row_refs$sr_disc_start, col = 1:ncol(sr)) # disclosure head
  merge_sr_cells(row = row_refs$sr_disc_start+1, col = 1:ncol(sr))
  merge_sr_cells(row = row_refs$sr_disc_start+2, col = 1:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+1, col = 2:ncol(sr)) # notes
  merge_sr_cells(row = row_refs$sr_notes_start+2, col = 2:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+3, col = 2:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+4, col = 2:ncol(sr))

  ### data: headers
  write_sr_data(text$sr_title, row = row_refs$sr_title, col = 1) # title
  write_sr_data(t(text$sr_heads), row = row_refs$sr_heads, col = 1) # headers

  ### data: dataset
  write_sr_data(sr, row = row_refs$sr_start, col = 1)

  ### data: notes etc
  write_sr_data(text$legend, row = row_refs$sr_legend_start, col = 1) # legend
  write_sr_data(text$disclosure_text, row = row_refs$sr_disc_start, col = 1) # disclosure text
  write_sr_data(text$disclosure_link, row = row_refs$sr_disc_start+2, col = 1) # disclosure hyperlink
  write_sr_data(text$sr_notes, row = row_refs$sr_notes_start, col = 1) # notes

  ### styles: font
  add_sr_style(styles$header, row = row_refs$sr_title, col = 1) # title
  add_sr_style(styles$body, row = (row_refs$sr_heads):(row_refs$cov_key_start+5), col = 1:ncol(sr)) # entire worksheet minus title
  add_sr_style(styles$subhead, row = row_refs$sr_start, col = 1:ncol(sr)) # hb totals
  add_sr_style(styles$disclosure_head, row = row_refs$sr_disc_start, col = 1) # disclosure head
  add_sr_style(styles$disclosure_body, row = row_refs$sr_disc_start+1, col = 1) # disclosure body
  add_sr_style(styles$subhead, row = (row_refs$sr_notes_start):(row_refs$sr_notes_start+4), col = 1) # notes

  ### styles: border
  #### top
  # AMc note: when I tried to use c() for row and col here, it produced a corrupt output, hence doing each one individually
  add_sr_style(styles$b_top, row = row_refs$sr_heads, col = 1:ncol(sr)) # data
  add_sr_style(styles$b_top, row = row_refs$sr_heads+1, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_end+1, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+1, col = 1:ncol(sr)) # notes
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+2, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+3, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+4, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+5, col = 1:ncol(sr))
  #### left
  add_sr_style(styles$b_left, row = (row_refs$sr_heads):(row_refs$sr_end), col = 1:(ncol(sr)+1)) # data
  add_sr_style(styles$b_left, row = (row_refs$sr_notes_start+1):(row_refs$sr_notes_start+4), col = c(1, ncol(sr)+1)) # notes

  ### styles: text alignment
  add_sr_style(styles$a_centre, row = row_refs$sr_heads, col = ncol(sr)) # heads
  add_sr_style(styles$a_middle, row = row_refs$sr_disc_start+1, col = 1) # disclosure
  add_sr_style(styles$a_middle, row = (row_refs$sr_notes_start+1):(row_refs$sr_notes_start+4), col = 1:2) # notes

  ### styles: number formatting
  add_sr_style(styles$counts, row = (row_refs$sr_start):(row_refs$sr_end), col = ncol(sr)) # counts

  ### worksheet formatting
  showGridLines(wb, "Self-referrals", showGridLines = F)
  #### col widths
  set_sr_col_widths(col = 1, width = 27)
  set_sr_col_widths(col = 2, width = 50)
  set_sr_col_widths(col = 3, width = 15)
  #### row heights
  set_sr_row_heights(row = row_refs$sr_heads, height = 50) # header
  set_sr_row_heights(row = row_refs$sr_disc_start+1, height = 112)# notes
  set_sr_row_heights(row = row_refs$sr_notes_start+1, height = 50)
  set_sr_row_heights(row = row_refs$sr_notes_start+2, height = 50)
  set_sr_row_heights(row = row_refs$sr_notes_start+3, height = 170)
  set_sr_row_heights(row = row_refs$sr_notes_start+4, height = 40)

  # 5: Write output ----
  # create GP Practices folder in output path, if necessary
  ifelse(!dir.exists(file.path(output_path, "GP Practices")), dir.create(file.path(output_path, "GP Practices")), "Directory already exists")

  # save output
  output <- paste0(output_filepath, "/GP Practices/NHS ", hb_name, " Practice level ", simplify_fy(financial_year), ".xlsx")
  openxlsx::saveWorkbook(wb, output, overwrite = T)
  print(paste0("Output saved for NHS ", hb_name))
}
