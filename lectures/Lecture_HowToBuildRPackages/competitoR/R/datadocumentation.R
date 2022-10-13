#' Single separate paramecium growth
#'
#' Growth of two species of paramecium, P aurelia
#' and P caudatum, in separate petri dishes.
#'
#' @usage data(single)
#'
#' @format Three columns:
#' \describe{
#'   \item{Day}{Day of experiment}
#'   \item{caudatum}{volume of P caudatum}
#'   \item{aurelia}{volume of P aurelia}
#' }
#'
#' @examples
#' data(single)
#' plot(aurelia ~ Day, data = single)
#' @source
#' @keywords data
"single"
