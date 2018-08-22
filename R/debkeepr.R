#' debkeepr: Analysis of Non-Decimal Currencies and Double-Entry Bookkeeping
#'
#' `debkeepr` provides an interface for analyzing non-decimal currencies that
#' use the tripartite system of pounds, shillings, and pence. It includes
#' functions to apply arithmetic and financial operations to single or multiple
#' values and to analyze account books that use either single-entry bookkeeping
#' or double-entry bookkeeping with the latter providing the name for
#' `debkeepr`. The use of non-decimal currencies throughout the medieval and
#' early modern period presents difficulties for the analysis of historical
#' accounts. The pounds, shillings, and pence system complicates even
#' relatively simple arithmetic manipulations, as each unit has to be
#' normalized or converted to the correct base.
#'
#' `debkeepr` implements the lsd class to store pounds, shillings, and pence
#' values and associate the values with bases for the shillings and pence
#' units. Pounds, shillings, and pence values are stored are stored as a list
#' of numeric vectors of length 3 that possesses a bases attribute. In
#' addition, lsd objects can be used as list columns in a data frame.
#'
#' The system of recording value according to pounds, shillings, and pence — or
#' to use the Latin terms from which the English derived libra, solidus, and
#' denarius (lsd) — developed in the 7th and 8th century during the rise of the
#' Carolingian Empire. The bases for the solidus and denarius units were never
#' completely uniform, but most commonly there were 12 denarii in a solidus and
#' 20 solidi in a libra. The custom of counting coins in dozens (solidi) and
#' scores of dozens (librae) spread throughout the Carolingian Empire and
#' became engrained in much of Europe until decimalization occurred after the
#' French Revolution.
#'
#' @docType package
#' @name debkeepr
NULL
