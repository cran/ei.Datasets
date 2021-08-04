\name{ei_SCO_2007}
\alias{ei_SCO_2007}
\docType{data}
\title{
Ecological inference data sets of the 2007 Scottish National Assembly.
}
\description{
This tibble contains 73 data sets corresponding to the 2007 Scottish National Assembly election. Each data set includes party and candidate vote results by voting unit as well as their associate cross-distributions (for votes and percentages) at the district (constituency) level.
}
\usage{data(ei_SCO_2007)}

\format{A tibble containing 73 observations and 6 variables:
\describe{
\item{\code{Number_of_district}}{Number assigned to the district/constituency by the New Zealand Electoral Commission.}
\item{\code{District}}{Name of the district/constituency.}
\item{\code{Votes_to_parties}}{A tibble for each constituency/district with the party votes recorded in each voting unit of the district.}
\item{\code{Votes_to_candidates}}{A tibble for each constituency/district with the candidate votes recorded in each voting unit of the district.}
\item{\code{District_cross_votes}}{A tibble for each constituency/district with the parties-candidates cross-distribution of votes in the entire constituency/district.}
\item{\code{District_cross_percentages}}{A tibble for each constituency/district, with the parties to candidates voter transition probabilities (in percentages) in the entire constituency/district.}
}}

\details{
Description of the \code{Votes_to_parties}, \code{Votes_to_candidates}, \code{District_cross_votes} and \code{District_cross_percentages} variables in more detail, where N(i), R(i) and C(i) denote, respectively, the number of voting units, party voting options and candidate voting options in district \code{i}:
\itemize{
\item{\code{Votes_to_parties}: A tibble containing N(i) observations and 2+R(i) variables. The two first variables, \code{Polling} and \code{Address} inform, respectively, about the code in the district assigned to the voting unit and the voting unit address. The rest of the columns correspond to the votes gained by the different party voting options competing in the district. The orders of the voting units in \code{Votes_to_parties} and \code{Votes_to_candidates} coincide.
}
\item{\code{Votes_to_candidates}: A tibble containing N(i) observations and 2+C(i) variables. The two first variables, \code{Polling} and \code{Address} inform, respectively, about the code in the district assigned to the voting unit and the voting unit address. The rest of the columns correspond to the votes gained by the different candidate voting options competing in the district. The orders of the voting units in \code{Votes_to_candidates} and \code{Votes_to_parties} coincide.}
\item{\code{District_cross_votes}: A tibble containing R(i) rows and 1+C(i) columns (variables). The first variable, which is labelled after the name of the district, contains the names of the parties in the same order than in \code{Votes_to_parties}, the rest of the variables (columns), ordered as in the \code{Votes_to_candidates} tibble, are labelled as the candidate voting options.}
\item{\code{District_cross_percentages}: A tibble containing R(i) rows and 1+C(i) columns (variables). The first variable, which is labelled after the name of the district, contains the names of the parties in the same order than in \code{Votes_to_parties}, the rest of the variables (columns), ordered as in the \code{Votes_to_candidates} tibble, are labelled as the candidate voting options.}
}
}

\source{
Own elaboration from raw data downloading from the Scotland Electoral Office website in 2011 by Carolina Plescia. These data are not longer available in that site.
}

\references{ ei.Datasets: Real datasets for assessing ecological inference algorithms, Social Science Computer Review, forthcoming.}

\keyword{datasets}

\seealso{
\code{\link{ei_NZ_2002}} \code{\link{ei_NZ_2005}} \code{\link{ei_NZ_2008}}
\code{\link{ei_NZ_2011}} \code{\link{ei_NZ_2014}} \code{\link{ei_NZ_2017}}
\code{\link{ei_NZ_2020}}
}

\author{
Jose M. Pavia, \email{pavia@uv.es}
}


