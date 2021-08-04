#' Merge small parties and/or candidates
#'
#' @description  Merge small parties and also small candidates by, respectively, aggregating them
#'               in the options 'Other parties votes' and 'Other candidates votes'.
#'
#' @author Jose M. Pavia, \email{jose.m.pavia@@uv.es}
#' @references Pavia, JM (2021). ei.Datasets: Real datasets for assessing ecological inference algorithms, Social Science Computer Review, forthcoming.
#'
#' @param x A tibble with the same components and structure as the tibbles in the `ei.Datasets` package.
#'          For instance, like the ei_NZ_2020 object.
#'
#' @param min.party A number between 0 and 100. Those parties which individually did not reach
#'                  at least min.party% of the election-district vote are grouped in the option
#'                  ‘Other parties votes’.
#'
#' @param min.candidate A number between 0 and 100. Those candidates which individually did not
#'                        reach at least min.candididate% of the election-district vote are grouped
#'                        in the option ‘Other candidates votes’.

#'
#' @return
#' A tibble similar to `x` with small parties and candidates merged on, respectively, ‘Other parties votes’
#' and ‘Other candidates votes’, with `min.party` and `min.candidate` used to determine when an electoral
#' option is small.
#' @export
#'
#' @seealso \code{\link{ei_NZ_2002}} \code{\link{ei_NZ_2005}} \code{\link{ei_NZ_2008}}
#'          \code{\link{ei_NZ_2011}} \code{\link{ei_NZ_2014}} \code{\link{ei_NZ_2017}}
#'          \code{\link{ei_NZ_2020}} \code{\link{ei_SCO_2007}}
#'
#' @examples
#' collapsed.tibble <- merge_small_options(x = ei_NZ_2020, min.party = 3, min.candidate = 5)
#'
#' @importFrom tibble tibble
#
merge_small_options <- function(x,
                                min.party,
                                min.candidate
                                ) {

  # Loading package tibble
  #  if (!require(tibble)) install.packages("tibble", repos = "http://cran.rstudio.com")
  #  require(tibble)

  # Conditions
  if ( min.party < 0 | min.party > 100 )
    stop('min.party must be a percentage. A number between 0 and 100')
  if ( min.candidate < 0 | min.candidate > 100 )
    stop('min.candidate must be a percentage. A number between 0 and 100')

  # Merging
  for (i in 1:nrow(x)){

      # Parties
      colapsar <- colSums(x$Votes_to_parties[[i]][ ,-c(1,2)])
      colapsar <- (colapsar/sum(colapsar)) < (min.party/100)
      J <- nrow(x$District_cross_votes[[i]])
      K <- ncol(x$District_cross_votes[[i]]) - 1

      if (sum(colapsar) > 1){
        # Transfer matrix counts
          temp <- colSums(x$District_cross_votes[[i]][ ,-1][colapsar, ])
          x$District_cross_votes[[i]][J + 1, -1] <- as.data.frame(t(temp))
          x$District_cross_votes[[i]][J + 1, 1] <- "Other parties votes"
          x$District_cross_votes[[i]] <- x$District_cross_votes[[i]][c(which(!colapsar), J + 1), ]

        # Transfer matrix proportions
          temp <- x$District_cross_percentages[[i]][ ,-1]*colSums(x$Votes_to_parties[[i]][ ,-c(1,2)])/100
          temp <- colSums(temp[colapsar, ])
          temp <- temp/sum(temp)*100
          x$District_cross_percentages[[i]][J + 1, -1] <- as.data.frame(t(temp))
          x$District_cross_percentages[[i]][J + 1, 1] <- "Other parties votes"
          x$District_cross_percentages[[i]] <- x$District_cross_percentages[[i]][c(which(!colapsar), J + 1), ]

        # base parties
        `Other parties votes` <- rowSums(x$Votes_to_parties[[i]][ ,-c(1,2)][, colapsar])
         x$Votes_to_parties[[i]] <- cbind(x$Votes_to_parties[[i]][ ,c(1, 2, which(!colapsar) + 2)],
                                          `Other parties votes`)

      } else {
        mensaje <- ifelse(sum(colapsar) == 0,
                          paste0("No merging of parties performed in district '", x$District[i],
                                 "'. All the parties got more than ",
                                 min.party, "% of the votes"),
                          paste0("No merging of parties performed in district '", x$District[i],
                                 "'. Only a party got less than ",
                                 min.party, "% of the votes"))
        print(mensaje)
      }

      # Candidates
      colapsar <- colSums(x$Votes_to_candidates[[i]][ ,-c(1,2)])
      colapsar <- (colapsar/sum(colapsar)) < (min.candidate/100)

      if (sum(colapsar) > 1){
        # Transfer matrix counts
        temp <- rowSums(x$District_cross_votes[[i]][ ,-1][, colapsar])
        x$District_cross_votes[[i]][, K + 2] <- temp
        names(x$District_cross_votes[[i]])[K + 2] <- "Other candidates votes"
        x$District_cross_votes[[i]] <- x$District_cross_votes[[i]][, c(1, which(!colapsar) + 1, K + 2)]

        # Transfer matrix proportions
        temp <- x$District_cross_percentages[[i]][ ,-1]*colSums(x$Votes_to_parties[[i]][ ,-c(1,2)])/100
        temp0 <- rowSums(temp[, colapsar])
        temp0 <- temp0/rowSums(temp)*100
        x$District_cross_percentages[[i]][, K + 2] <- temp0
        names(x$District_cross_percentages[[i]])[K + 2] <- "Other candidates votes"
        x$District_cross_percentages[[i]] <- x$District_cross_percentages[[i]][, c(1, which(!colapsar) + 1, K + 2)]

        # base candidates
        `Other candidates votes` <- rowSums(x$Votes_to_candidates[[i]][ ,-c(1,2)][, colapsar])
        x$Votes_to_candidates[[i]] <- cbind(x$Votes_to_candidates[[i]][ ,c(1, 2, which(!colapsar) + 2)],
                                         `Other candidates votes`)

      } else {
        mensaje <- ifelse(sum(colapsar) == 0,
                          paste0("No merging of candidates performed in district '", x$District[i],
                                 "'. All the candidates got more than ",
                                 min.candidate, "% of the votes"),
                          paste0("No merging of candidates performed in district '", x$District[i],
                                 "'. Only a candidate got less than ",
                                 min.candidate, "% of the votes"))
        print(mensaje)
      }
  }
  return(x)
}
