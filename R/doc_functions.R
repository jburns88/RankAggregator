## ###################################################################################################################
## Functions for rank aggregation of partial rankings
##    Version 1.0
## ###################################################################################################################
## --- Contents ---
##
## - rankAggregator         --- function to run rank aggregation procedure
##
## - evaluationMatrix       --- creates evaluation matrix (input to consensusRanking)
##
## - consensusRanking       --- core function for branch and bound algorithm
##
## - extendRanking          --- function to run probingHeuristic
##
## - lowerBound             --- lowest possible number of violations for a given partial ranking
##
## - upperBound             --- actual number of violations for complete candidate ranking
##
## ###################################################################################################################

## ##############################################################################
## >> consensusRankingBoot
## ##############################################################################
#' @title Rank aggregation of partial rankings with optonal bootstrapping
## ##############################################################################
#' @description This funciton calls \code{\link{RankAggregator}}::\code{\link{consensusRanking}} to
#'              return a best consensus (or median) ranking for a set of (partial) rankings.
#'              \cr\cr This function also provides an optional bootstrap resampling procedure to
#'              give user-defined confidence intervals and average rank positions with
#'              the consensus ranking.
## ##############################################################################
#' @param x a \code{data.frame} containing columns titled \code{Reviewer}, \code{Item},
#'          \code{Ranking}. On data structure, \code{Reviewer} and \code{Item} must be
#'          character, and \code{Ranking} must be numeric. Each row of \code{x}
#'          identifes the rank position that a single \code{Reviewer} awarded a single
#'          \code{Item}
#'
#' @param bootstrap a logical value indicating whether to bootstrap the rank aggregation
#'                  procedure.
#'
#' @param nboot a numeric value for bootstrap replicates. Default value is \code{10000}.
#'
#' @param conf.int a numeric value >0 and <1. Default value is \code{0.95}, which sets
#'                 confidence interval at 95% level.
#'
#' @param prog.upd a logical value indicating whether the user wants progress updates
#'                 on the bootstrap procedure.
## ############################################################################
#' @return If \code{bootstrap} is \code{FALSE}, a \code{data.frame} is returned,
#'         with two columns: \code{Item} and \code{Rank.est}, where \code{Item}
#'         is a \code{Factor} containing all unique \code{Item}'s from the input
#'         \code{data.frame} \code{x}, and where \code{Rank.est} is the estimated
#'         (numeric) rank position based on the \code{consensusRanking()} rank
#'         aggregation procedure.#'
#'         \cr \cr If \code{bootstrap} is \code{TRUE}, a \code{list} is returned, with two
#'         elements:
#'         \itemize{
#'         \item \code{$summaryTable} is a \code{data.frame} with six columns: \code{Item}
#'         \code{Rank.est}, \code{Rank.cilo}, \code{Rank.cihi}, \code{Rank.median},
#'         \code{Rank.mean}. Where \code{Item} and \code{Rank.est} are as described
#'         above, \code{Rank.cilo} and \code{Rank.cihi} are the estimates for the
#'         low and high confidence intervals, respectively. \code{Rank.median} and
#'         \code{Rank.mean} both describe the average rank positions.
#'         \item \code{$bootstrapData} is an array containing estimated (numeric)
#'         rank positions based on the \code{consensusRanking()} rank aggregation
#'         procedure with resampled data. \code{NA} denotes estimated rankings that
#'         were discarded due to not containing all \code{Item}s.
#'         }
## ############################################################################
## Edits
## ############################################################################
#' @seealso Calls the internal function \code{\link{consensusRanking}}, which calls the other
#'          internal functions \code{\link{evaluationMatrix}}, \code{\link{consensusRanking}},
#'          \code{\link{extendRanking}}, \code{\link{lowerBound}},
#'          \code{\link{upperBound}}
#' @references Cook, W.D., Golany, B., Penn, M. and Raviv, T., 2007.
#'             Creating a consensus ranking of proposals from reviewers
#'             partial ordinal rankings. Computers & Operations Research,
#'             34, pp.954-965.\cr\cr
#'             Marshall, E.C., Sanderson, C., Spiegelhalter, D.J. and McKee,
#'             M., 1998. Reliability of league tables of in vitro fertilisation
#'             clinics: retrospective analysis of live birth ratesCommentary:
#'             How robust are rankings? The implications of confidence intervals.
#'             Bmj, 316, pp.1701-1705.
#' @export
#' @example
#' \donttest{consensusRankingBoot(cook_example, bootstrap = TRUE,
#'                                        nboot = 10000, conf.int = 0.95,
#'                                        prog.upd = TRUE)}

consensusRankingBoot <- function(x,
                           bootstrap,
                           nboot = 10000,
                           conf.int = 0.95,
                           prog.upd = TRUE){

  ## checks
  #####
  ## argument checks
  if(! is.data.frame(x)){
    stop("argument: x must be a data.frame") }
  if(! is.logical(bootstrap)){
    stop("argument: bootstrap must be logical") }
  if(bootstrap){
    if(! is.numeric(nboot)){
      stop("argument: nboot must be numeric") }
    if(! nboot %% 1 == 0){
      stop("argument: nboot must be a wholenumber") }
    if(! is.numeric(conf.int)){
      stop("argument: conf.int must be numeric") }
    if(! (conf.int > 0 & conf.int < 1)){
      stop("argument: conf.int must be >0 and <1") }
    if(! is.logical(prog.upd)){
      stop("argument: prog.upd must be logical") }
  }

  ## data checks
  if(! "Reviewer" %in% colnames(x)){
    stop("x must contain column named Reviewer") }
  if(! "Item" %in% colnames(x)){
    stop("x must contain column named Item") }
  if(! "Ranking" %in% colnames(x)){
    stop("x must contain column named Ranking") }
  if(! is.character(x$Reviewer)){
    stop("x$Reviewer must be a character vector") }
  if(! is.character(x$Item)){
    stop("x$Item must be a character vector") }
  if(! is.numeric(x$Ranking)){
    stop("x$Ranking must be a numeric vector") }
  if(any(data.frame(table(x$Reviewer, x$Ranking))$Freq > 1)){
    warning("Not all rankings are strict, meaning some rankings contain ties") }
  #####

  ######################################
  ## ORIGINAL FUNCTION
  ## create evaluation matrix, run consensus ranking procedure
  ## assign results to output table

  x <- as.data.frame(x[,c("Reviewer","Item","Ranking")], stringsAsFactors = FALSE)

  conr.est <- consensusRanking(x)

  out.tab <- conr.est ; colnames(out.tab) <- gsub("Rank", "Rank.est", colnames(out.tab))

  ######################################
  ## BOOTSTRAPPING
  ## resample studies and re-run consensus ranking procedure nboot times

  if(bootstrap){

    st <- levels(factor(x$Reviewer))

    nt <- length(conr.est$Item)

    ns <- length(st)

    conr.boot <- array(dim = c(nt, nboot))

    nboot.decile <- round(nboot/10)

    dec <- 1

    for(k in 1:nboot){

      if(prog.upd & dec < 10){ if(k%%nboot.decile == 0){
        message(paste0(seq(10,90,10)[dec],"% complete"))
        dec <- dec + 1
        }
      }

      stuk <- sample(st,ns,replace = TRUE)

      tmp <- NULL

      for(j in 1:ns){

        bob <- x[x$Reviewer == stuk[j],]

        bob$Reviewer <- paste0(stuk[j],j)

        tmp <- rbind(tmp, bob)
      }

      conr.k <- consensusRanking(tmp)

      drop.boot <- (length(unique(conr.k$Item)) != nt)

      if(! drop.boot){  conr.boot[,k] <- conr.k$Rank[match(conr.est$Item, conr.k$Item)] }

    }

    ######################################
    ## SUMMARY TABLE
    ## add summary statistics to output table

    out.tab$Rank.cilo <- apply(conr.boot, 1, stats::quantile, (1-conf.int)/2, na.rm=TRUE)
    out.tab$Rank.cihi <- apply(conr.boot, 1, stats::quantile, 1-(1-conf.int)/2, na.rm=TRUE)
    out.tab$Rank.median <- apply(conr.boot, 1, stats::median, na.rm=TRUE)
    out.tab$Rank.mean <- apply(conr.boot, 1, mean, na.rm=TRUE)
  }

  if(bootstrap & prog.upd){message("100% complete")}

  print(out.tab)

  if(bootstrap){
    out.tab <- list(summaryTable = out.tab,
                    bootstrapData = conr.boot)
  }

  invisible(out.tab)
}



## ##############################################################################
## >> consensusRanking
## ##############################################################################
#' @title Rank aggregation of partial rankings
## ##############################################################################
#' @description This function is the core function for the \code{\link{RankAggregator}} package.
#'          This function uses a branch and bound algorithm, described by Cook et al. (2007),
#'          to return a best consensus (or median) ranking for a set of (partial) rankings.
## ##############################################################################
#' @param x a \code{data.frame} containing columns titled \code{Reviewer}, \code{Item},
#'          \code{Ranking}. On data structure, \code{Reviewer} and \code{Item} must be
#'          character, and \code{Ranking} must be numeric. Each row of \code{x}
#'          identifes the rank position that a single \code{Reviewer} awarded a single
#'          \code{Item}
## ############################################################################
#' @return A \code{data.frame} is returned,
#'         with two columns: \code{Item} and \code{Rank}, where \code{Item}
#'         is a \code{Factor} containing all unique \code{Item}'s from the input
#'         \code{data.frame} \code{x}, and where \code{Rank} is the estimated
#'         (numeric) rank position based on the branch and bound rank
#'         aggregation procedure.
## ############################################################################
## Edits
## ############################################################################
#' @seealso This function calls internal functions
#'   \code{\link{evaluationMatrix}},
#'   \code{\link{extendRanking}}, \code{\link{lowerBound}}, and
#'   \code{\link{upperBound}}
#' @export
#' @examples
#' consensusRanking(cook_example)

consensusRanking <- function(x){

  ## checks
  #####
  ## argument checks
  if(! is.data.frame(x)){
    stop("argument: x must be a data.frame") }

  ## data checks
  if(! "Reviewer" %in% colnames(x)){
    stop("x must contain column named Reviewer") }
  if(! "Item" %in% colnames(x)){
    stop("x must contain column named Item") }
  if(! "Ranking" %in% colnames(x)){
    stop("x must contain column named Ranking") }
  if(! is.character(x$Reviewer)){
    stop("x$Reviewer must be a character vector") }
  if(! is.character(x$Item)){
    stop("x$Item must be a character vector") }
  if(! is.numeric(x$Ranking)){
    stop("x$Ranking must be a numeric vector") }
  if(any(data.frame(table(x$Reviewer, x$Ranking))$Freq > 1)){
    warning("Not all rankings are strict, meaning some rankings contain ties") }
  #####

  x <- as.data.frame(x[,c("Reviewer","Item","Ranking")], stringsAsFactors = FALSE)

  rmat <- evaluationMatrix(x)

  nt <- nrow(rmat)

  final.rank <- rep(NA, nt)

  is.dominating <- is.dominated <- rep(FALSE, nt)

  dodo <- rep(FALSE, nt)

  nododo <- ! dodo

  for(i in 1:nt){

    if(any(nododo)){

      for(j in which(nododo)){

        if(! i %in% final.rank){

          is.dominating[j] <- all(rmat[nododo,j] >= rmat[j,nododo])

          if(is.dominating[j]){ final.rank[j] <- i  }
        }

        if(! (nt - i + 1) %in% final.rank){

          is.dominated[j] <- all(rmat[nododo,j] <= rmat[j,nododo])

          if(is.dominated[j]){ final.rank[j] <- nt - i + 1  }
        }
      }

      dodo <- (is.dominating) | (is.dominated)

      nododo <- ! dodo
    }
  }

  nu <- sum(nododo)

  if(nu > 0){

    umat <- rmat[nododo,nododo]

    curnode <- list(partial.ranking = rep(NA, nu))

    curnode$included <- ! is.na(curnode$partial.ranking)

    curnode$prl <- sum(curnode$included)

    curnode$lb <- lowerBound(umat, partial.ranking = curnode$partial.ranking)

    curnode$complete.ranking <- extendRanking(umat, node = curnode)

    curnode$ub <- upperBound(curnode$complete.ranking, umat)

    terminated <- (curnode$lb == curnode$ub)

    activenodes <- as.list(NULL)

    node.explored <- vector(mode = "list", length = 1)

    incumbent <- curnode

    activenodes[[1]] <- curnode

    i <- 1

    while(! terminated){

      active.lb <- unlist(lapply(activenodes, function(x){x$lb}))

      active.prl <- unlist(lapply(activenodes, function(x){x$prl}))

      node.bcs <- which(active.lb == min(active.lb))

      if(length(node.bcs) > 1){

        node.bcs <- node.bcs[active.prl[node.bcs] == max(active.prl[node.bcs])][1]
      }

      curnode <- activenodes[[node.bcs]]

      if(length(activenodes) > 1){ activenodes <- activenodes[-node.bcs] }

      if((length(activenodes) == 1) & i != 1){

        if(length(Filter(length, node.explored)) == 0){

          node.explored[[1]] <- activenodes[[1]]

        } else {

          terminated <- identical(node.explored[[1]], activenodes[[1]])

          if(! terminated){

            node.explored[[1]] <- activenodes[[1]]
          }
        }
      }

      newactive <- vector(mode = "list", length = sum(!curnode$included))

      h1 <- 1

      for(j in which(! curnode$included)){

        newnode <- curnode

        newnode$included[j] <- TRUE

        newnode$prl <- newnode$prl + 1

        newnode$partial.ranking[j] <- newnode$prl

        newnode$lb <- lowerBound(umat,partial.ranking = newnode$partial.ranking)

        if(newnode$lb <= incumbent$ub){

          newnode$complete.ranking <- extendRanking(umat, node = newnode)

          newnode$ub <- upperBound(ccr = newnode$complete.ranking, umat)

          if(newnode$ub < incumbent$ub){

            incumbent <- newnode
          }

          newactive[[h1]] <- newnode

          h1 <- h1 + 1

        }
      }

      if(length(Filter(length, newactive)) != 0){

        activenodes <- Filter(function(x) x$lb <= incumbent$ub,
                              append(activenodes, Filter(length, newactive)))
      }
      i <- i + 1

    }

    final.rank[nododo] <- sum(is.dominating) + incumbent$complete.ranking
  }
  consensus <- data.frame(Item = row.names(rmat), Rank = final.rank)

  return(consensus)
}




## ##############################################################################
## >> evaluationMatrix
## ##############################################################################
#' @title Evaluation matrix
## ##############################################################################
#' @description This function is called by \code{\link{RankAggregator}}::\code{\link{consensusRanking}}.
#'          For each pair of \code{Item}s, whenever both \code{Item}s are ranked by the
#'          same \code{Reviewer}, this function sums the occurances when each of the two
#'          \code{Item}s is preferred to the other.
## ##############################################################################
#' @param x a \code{data.frame} containing columns titled \code{Reviewer}, \code{Item},
#'          \code{Ranking}. On data structure, \code{Reviewer} and \code{Item} must be
#'          character, and \code{Ranking} must be numeric. Each row of \code{x}
#'          identifes the rank position that a single \code{Reviewer} awarded a single
#'          \code{Item}
## ############################################################################
#' @return  An \code{m x n} pairwise \code{matrix} giving the number of times
#'          \code{Item[m]} is preferred to (i.e. receives a ranking value lower than)
#'          \code{Item[n]} across all \code{Reviewer} \code{Rankings}
## ############################################################################
## Edits
## ############################################################################
#' @export
#' @examples
#' evaluationMatrix(cook_example)

evaluationMatrix <- function(x) {

  ## checks
  #####
  ## argument checks
  if(! is.data.frame(x)){
    stop("argument: x must be a data.frame") }
  ## data checks
  if(! "Reviewer" %in% colnames(x)){
    stop("x must contain column named Reviewer") }
  if(! "Item" %in% colnames(x)){
    stop("x must contain column named Item") }
  if(! "Ranking" %in% colnames(x)){
    stop("x must contain column named Ranking") }
  if(! is.character(x$Reviewer)){
    stop("x$Reviewer must be a character vector") }
  if(! is.character(x$Item)){
    stop("x$Item must be a character vector") }
  if(! is.numeric(x$Ranking)){
    stop("x$Ranking must be a numeric vector") }
  #####

  items <- unique(x$Item)

  by_reviewer <- stats::reshape(as.data.frame(x),
                         idvar = "Reviewer",
                         v.names = "Ranking",
                         timevar = "Item",
                         direction = "wide")

  names(by_reviewer) <- gsub("Ranking.", "", names(by_reviewer))

  rmat <- outer(items, items,
                Vectorize(FUN = function (m, n, by_reviewer) sum(by_reviewer[,m] > by_reviewer[,n], na.rm = TRUE),
                          vectorize.args = c("m", "n")),
                by_reviewer)

  colnames(rmat) <- rownames(rmat) <- items

  return(rmat)
}



## ##############################################################################
## >> extendRanking
## ##############################################################################
#' @title Fully extend a partial ranking
## ##############################################################################
#' @description This function is called by \code{\link{RankAggregator}}::\code{\link{consensusRanking}}.
#'          The heuristic procedure orders unranked \code{Items} according the proportion
#'          of times an item was preferred in all pairwise comparisons with other unranked
#'          \code{Item}s.
## ##############################################################################
#' @param umat a \code{matrix}, which is either the output of \code{\link{evaluationMatrix}},
#'             or a subset of the output of \code{\link{evaluationMatrix}}.
#'
#'
#' @param node a \code{list} of elements, containing information about a node in
#'             the branch and bound search space. The relevant elements here are
#'             \code{$partial.ranking}, \code{$included}, and \code{$prl}. Where,
#'             \code{$partial.ranking} is a vector of rank positions for each \code{Item}
#'             in \code{umat} that is ranked so far; partial rankings may contain some -
#'             or all - \code{NA} values. \code{$included} is a logical vector denoting if
#'             an \code{Item} in \code{umat} is ranked in \code{$partial.ranking}. And
#'             \code{$prl} is a numeric value denoting how many of the \code{Item}s in
#'             \code{umat} are ranked in \code{$partial.ranking}.
## ############################################################################
#' @return A vector of rank positions.
## ############################################################################
## Edits
## ############################################################################
#' @export

extendRanking <- function(umat, node) {

  out <- node$partial.ranking

  if (length(which(! node$included)) <= 1) {

    out[!node$included] <- length(out)

  } else {

    cmat <- umat[!node$included, !node$included]
    nt <- nrow(cmat)
    z <- integer(nt)
    rs <- rowSums(cmat)
    cs <- colSums(cmat)
    rv.umat <- rowSums(umat)/(rowSums(umat) + colSums(umat))

    for (i in 1:(nt - 1)) {

      if(sum(rs[is.finite(rs)], cs[is.finite(cs)]) != 0) {

        rmk <- which.min(rs / (cs + rs))
        z[i] <- names(rmk)
        rs <- rs - cmat[, rmk]
        cs <- cs - cmat[rmk,]

      } else {

        rmk.nan <- match(colnames(cmat[is.finite(cs), is.finite(cs)]),
                         colnames(umat))
        rmk <- which.min(rv.umat[rmk.nan])
        z[i] <- names(rmk)
        rmk <- match(names(rmk), names(cs))

      }

      cs[rmk] <- -Inf
      rs[rmk] <- Inf
    }

    z[nt] <- names(cs[which(cs != -Inf)])

    out[!node$included] <- node$prl + data.frame(Item = row.names(umat),
                                                 Rank = match(row.names(umat), z))$Rank[which(!node$included)]
  }

  return(out)
}



## ##############################################################################
## >> lowerBound
## ##############################################################################
#' @title Lower bound value
## ##############################################################################
#' @description This function is called by \code{\link{RankAggregator}}::\code{\link{consensusRanking}}.
#'          The lower bound is the absolute lowest value a complete candidate ranking
#'          could attain. Note, this value is not always achievable, so may differ
#'          from the value returned by \code{\link{upperBound}}.
#'
#'          For each pair of \code{Item}s, there are three possible calculations, depending
#'          on whether both \code{Item}s are in the \code{partial.ranking}, one is in
#'          and the other is out the \code{partial.ranking}, or both are not in
#'          the \code{partial.ranking}.
## ##############################################################################
#' @param umat a \code{matrix}, which is either the output of \code{\link{evaluationMatrix}},
#'             or a subset of the output of \code{\link{evaluationMatrix}}.
#'
#' @param partial.ranking a vector of rank positions
#'                        for each \code{Item} in \code{umat} that is ranked so far;
#'                        partial rankings may contain some - or all - \code{NA}
#'                        values.
## ############################################################################
#' @return A numeric value  for the lower bound of a \code{partial.ranking}
## ############################################################################
## Edits
## ############################################################################
#' @export

lowerBound <- function(umat, partial.ranking){

  p1 <- ! is.na(partial.ranking)

  ## if both in p1
  # output identical to original loop
  eq.p1 <- sum(umat[p1,p1] * outer(partial.ranking[which(p1)],
                                   partial.ranking[which(p1)],
                                   FUN = `<`))

  ## if one in p1 and one not in p1
  # output identical to original loop
  eq.p2 <- sum(umat[p1,!p1])

  ## if both not in p1
  # output not identical to original loop, but original loop ignored instances where umat[q,p] == umat[p,q]
  eq.p3 <- sum(pmin(umat[!p1,!p1][upper.tri(umat[!p1,!p1])],
                    t(umat[!p1,!p1])[upper.tri(t(umat[!p1,!p1]))]))

  return(sum(eq.p1, eq.p2, eq.p3))
}



## ##############################################################################
## >> upperBound
## ##############################################################################
#' @title Upper bound value
## ##############################################################################
#' @description This function is called by \code{\link{RankAggregator}}::\code{\link{consensusRanking}}.
#'          The upper bound value is the value used by the branch and bound
#'          algorithm in determining whether or not to replace the current incumbent
#'          solution.
## ##############################################################################
#' @param ccr a vector of rank positions that is a candidate complete ranking
#'
#' @param umat a \code{matrix}, which is either the output of \code{\link{evaluationMatrix}},
#'             or a subset of the output of \code{\link{evaluationMatrix}}.
## ############################################################################
#' @return  A numeric value  for the upper bound of a candidate complete ranking
## ############################################################################
## Edits
## ############################################################################
#' @export

upperBound <- function(ccr, umat){

  return(sum(umat * outer(ccr, ccr, FUN = `<`)))
}



## ############################################################################
# --
# Biomathematics and Statistics Scotland (BioSS) is formally part of The
# James Hutton Institute (JHI), a registered Scottish charity No. SC041796
# and a company limited by guarantee No. SC374831
