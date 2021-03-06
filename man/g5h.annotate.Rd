% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate.R
\name{g5h.annotate}
\alias{g5h.annotate}
\title{Annotate a cleaned dataset}
\usage{
g5h.annotate(.data, by = "col")
}
\arguments{
\item{.data}{data.frame cleaned by g5h.clean()}

\item{by}{'col' or 'row', default is 'col'. See ?g5h.gather_col for more info.}
}
\value{
data.frame
}
\description{
Add time interval in hour from the oldest timestamp
}
\examples{
\donttest{
# suppose "gen5_export.txt" is the export from Gen5 2.06

g5h.clean2("gen5_export.txt") \%>\%
    g5h.annotate()
}
}
