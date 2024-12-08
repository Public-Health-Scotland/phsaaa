% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick_functions.R
\name{format_excel_hyperlink}
\alias{format_excel_hyperlink}
\title{Turn URL into hyperlink for Excel outputs}
\usage{
format_excel_hyperlink(name, url)
}
\arguments{
\item{name}{Character string text displayed for link}

\item{url}{Character string URL}
}
\value{
1x1 dataframe containing hyperlink ready for writing with openxlsx
}
\description{
Turn URL into hyperlink for Excel outputs
}
\examples{
format_excel_hyperlink("Search engine", "www.google.com")

}
