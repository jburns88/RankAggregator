#' RankAggregator
#'
#' This package provides a set of functions to easily compute an aggregate
#' ranking (also called a median ranking or a
#' compromise ranking) according to the axiomatic approach presented
#' by Cook et al. (2007). This approach minimises the number of violations
#' between all candidate consensus rankings and all input (partial) rankings,
#' and draws on a branch and bound algorithm, and a heuristic algorithm to
#' drastically improve speed. Input rankings can be either incomplete (partial)
#' or complete.
#' \cr\cr The package also provides an option to bootstrap
#' resulting consensus ranking based on resampling input rankings (with
#' replacement). This approach was inspired by Marshall et al. (1998).
#'
#' @author Jay Burns \email{jay.burns@sruc.ac.uk},
#'         Adam Butler \email{adam.butler@bioss.ac.uk}
#' @references Cook, W.D., Golany, B., Penn, M. and Raviv, T., 2007.
#'             Creating a consensus ranking of proposals from reviewers
#'             partial ordinal rankings. Computers & Operations Research,
#'             34, pp.954-965.\cr\cr
#'             Marshall, E.C., Sanderson, C., Spiegelhalter, D.J. and McKee,
#'             M., 1998. Reliability of league tables of in vitro fertilisation
#'             clinics: retrospective analysis of live birth ratesCommentary:
#'             How robust are rankings? The implications of confidence intervals.
#'             Bmj, 316, pp.1701-1705.
#' @docType package
#' @name RankAggregator
"_PACKAGE"
