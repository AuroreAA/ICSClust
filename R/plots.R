
#' Plot the Generalized Kurtosis Values of the ICS Transformation
#'
#' Extract the generalized kurtosis values of the components obtained via an
#' ICS transformation and draw a screeplot.

#' @param object  an object inheriting from class \code{"ICS"} containing
#' results from an ICS transformation.
#' @param select  an integer, character, or logical vector specifying for which
#' components to extract the generalized kurtosis values, or \code{NULL} to
#' extract the generalized kurtosis values of all components.
#' @param scale  a logical indicating whether to scale the generalized kurtosis
#' values to have product 1 (defaults to \code{FALSE}).
#' @import ICS
#' @import ggplot2
#' @export
screeplot_crit.ICS <- function(object, select = NULL, scale = FALSE, ...) {
  df <- data.frame(gen_kurtosis = ICS::gen_kurtosis(object, select = select, 
                                                    scale = scale))
 
  df %>% 
    ggplot(aes(x = rownames(df), index, y = gen_kurtosis ))+
    geom_point() +
    # geom_hline(aes(yintercept = 1))+
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(x = "") +
    scale_x_discrete(limits = rownames(df))
}




#' @export
pairs_plot <- function(object, ...) UseMethod("pairs_plot")

#' @import ICS
#' @export
pairs_plot.ICS <- function(object, select = NULL, ...){
  pairs_plot.default(ICS::components(object, select = select), ...)
}

#' @import ICS
#' @export
pairs_plot.data.frame <- function(object, select = NULL, ...){
  pairs_plot.default(object[, select], ...)
}

#' @import ICS
#' @import GGally
#' @importFrom ggthemes scale_colour_colorblind
#' @export
pairs_plot.default <- function(object, clusters = NULL, 
                               text_size_factor = 8/6.5, 
                               colors = NULL, ...) {
  if(is.null(clusters)) clusters <- rep("", nrow(object))
  
  df_plot <- data.frame(object, clusters  = clusters)
  column_labels <- paste0(gsub(".", "[", colnames(object), fixed = TRUE), "]")

  if(is.null(colors)) colors <- scales::hue_pal()(length(unique(clusters)))
  
  p <- GGally::ggpairs(df_plot, aes(color = clusters, alpha = 0.4), 
                      upper = list(continuous = "points"),
                      columns = seq_along(column_labels), 
                      columnLabels = column_labels,
                      labeller = label_parsed) + 
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme_bw() +
    theme(axis.text = element_text(size = 9 * text_size_factor), 
          strip.text = element_text(size = 10 * text_size_factor))
  
  p
  
}