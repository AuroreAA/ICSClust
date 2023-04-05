
#' @import ICS
#' @export
# plot_gen_kurtosis.ICS <- function(object, select = NULL, scale = FALSE,
#                               ...) {
#   # back-compatibility check
#   if (missing(select) && !missing(index)) {
#     warning("argument 'index' is deprecated, use 'select' instead")
#     select <- index
#   }
#   # extract generalized kurtosis values
#   gen_kurtosis <- object$gen_kurtosis
#   p <- length(gen_kurtosis)
#   # if requested, scale generalized kurtosis values
#   if (isTRUE(scale)) gen_kurtosis <- gen_kurtosis / prod(gen_kurtosis)^(1/p)
#   # check if we have argument of components to return
#   if (!is.null(select)) {
#     # check argument specifying components
#     if (check_undefined(select, max = p, names = names(gen_kurtosis))) {
#       stop("undefined components selected")
#     }
#     # select components
#     gen_kurtosis <- gen_kurtosis[select]
#   }
#   # return generalized kurtosis values for selected components
#   gen_kurtosis
# }