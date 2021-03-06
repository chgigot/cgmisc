% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impact-factor.R
\name{collect_impact_factor}
\alias{collect_impact_factor}
\title{Collect journal impact factors in the Science Citation Index}
\usage{
collect_impact_factor(file, ...)
}
\arguments{
\item{file}{Name of the file with academic journal names and relative URLs
related to those journals on the SCI website. Each line of this file is
about one journal and must have the following structure:
`"journal name","relative URL"`. For a file example, take a look at:
`system.file("extdata", "some_journals.txt", package = "cgmisc")`.}

\item{...}{Not yet implemented.}
}
\value{
A data frame with three columns: "journal", "year", "impact_factor".
}
\description{
Gather impact factors of academic journals requested by the user and
published on the SCI website (www.scijournal.org).
}
\details{
"The impact factor (IF) or journal impact factor (JIF) of an academic journal
is a measure reflecting the yearly average number of citations to recent
articles published in that journal. It is frequently used as a proxy for the
relative importance of a journal within its field; journals with higher
impact factors are often deemed to be more important than those with lower
ones. The impact factor was devised by Eugene Garfield, the founder of the
Institute for Scientific Information. Impact factors are calculated yearly
starting from 1975 for those journals that are listed in the Journal Citation
Reports." (source: \url{https://en.wikipedia.org/wiki/Impact_factor})
}
\examples{
my_file <- system.file("extdata", "some_journals.txt", package = "cgmisc")
my_data <- collect_impact_factor(my_file)
plot(my_data)

}
