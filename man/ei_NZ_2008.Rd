\name{ei_NZ_2008}
\alias{ei_NZ_2008}
\docType{data}
\title{
Ecological inference data sets of the 2008 New Zealand General Election.
}
\description{
This tibble contains 70 data sets corresponding to the 2008 New Zealand General Election. Each data set includes party and candidate vote results by voting unit as well as their associate cross-distributions (for votes and percentages) at the district (electorate) level.
}
\usage{data(ei_NZ_2008)}

\format{A tibble containing 70 observations and 6 variables:
\describe{
\item{\code{Number_of_district}}{Number assigned to the district/electorate by the New Zealand Electoral Commission.}
\item{\code{District}}{Name of the district/electorate.}
\item{\code{Votes_to_parties}}{A tibble for each electorate/district with the party votes recorded in each voting unit of the district.}
\item{\code{Votes_to_candidates}}{A tibble for each electorate/district with the candidate votes recorded in each voting unit of the district.}
\item{\code{District_cross_votes}}{A tibble for each electorate/district with the parties-candidates cross-distribution of votes in the entire electorate/district.}
\item{\code{District_cross_percentages}}{A tibble for each electorate/district, with the parties to candidates voter transition probabilities (in percentages) in the entire electorate/district.}
}}

\details{
Description of the \code{Votes_to_parties}, \code{Votes_to_candidates}, \code{District_cross_votes} and \code{District_cross_percentages} variables in more detail, where N(i), R(i) and C(i) denote, respectively, the number of voting units, party voting options and candidate voting options in district \code{i}:
\itemize{
\item{\code{Votes_to_parties}: A tibble containing N(i) observations and 2+R(i) variables. The two first variables, \code{City} and \code{Address} inform, respectively, about the place in the district where the voting unit is located and the voting unit address. The rest of the columns correspond to the votes gained by the different party voting options competing in the district. The orders of the voting units in \code{Votes_to_parties} and \code{Votes_to_candidates} coincide.
}
\item{\code{Votes_to_candidates}: A tibble containing N(i) observations and 2+C(i) variables. The two first variables, \code{City} and \code{Address} inform, respectively, about the place in the district where the voting unit is located and the voting unit address. The rest of the columns correspond to the votes gained by the different candidate voting options competing in the district. The orders of the voting units in \code{Votes_to_candidates} and \code{Votes_to_parties} coincide.}
\item{\code{District_cross_votes}: A tibble containing R(i) rows and 1+C(i) columns (variables). The first variable, which is labelled after the name of the district, contains the names of the parties in the same order than in \code{Votes_to_parties}, the rest of the variables (columns), ordered as in the \code{Votes_to_candidates} tibble, are labelled as the candidate voting options.}
\item{\code{District_cross_percentages}: A tibble containing R(i) rows and 1+C(i) columns (variables). The first variable, which is labelled after the name of the district, contains the names of the parties in the same order than in \code{Votes_to_parties}, the rest of the variables (columns), ordered as in the \code{Votes_to_candidates} tibble, are labelled as the candidate voting options.}
}
}

\source{
Own elaboration from data available in \url{https://www.electionresults.govt.nz}, retrieved 19 January 2019.
}

\references{ ei.Datasets: Real datasets for assessing ecological inference algorithms, Social Science Computer Review, forthcoming.}

\note{The New Zealand Electoral Commission had no involvement in preparing these data sets. The raw data has been pre-processed in order to guarantee their straighforward usefulness in ecological inference procedures. Some small discrepancies exist among the figures in \code{District_cross_percentages} and \code{District_cross_votes}. The percentages are a direct translation of the published data, whereas the vote counts have been adjusted using integer linear programming to make them congruents with the figures in \code{Votes_to_parties} and \code{Votes_to_candidates}. More details in Pavia (2021). For the official results, visit \url{https://www.electionresults.govt.nz}.
}

\keyword{datasets}

\seealso{
\code{\link{ei_NZ_2002}} \code{\link{ei_NZ_2005}} \code{\link{ei_NZ_2011}}
\code{\link{ei_NZ_2014}} \code{\link{ei_NZ_2017}} \code{\link{ei_NZ_2020}}
\code{\link{ei_SCO_2007}}
}
\author{
Jose M. Pavia, \email{pavia@uv.es}
}


