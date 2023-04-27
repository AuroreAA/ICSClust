

#' @export
select_plot <- function(object, ...) UseMethod("select_plot")


#' Plot the Generalized Kurtosis Values of the ICS Transformation
#'
#' Extract the generalized kurtosis values of the components obtained via an
#' ICS transformation and draw a screeplot.

#' @param df 
#' @param type 
#' @param ... 
#'
#' @import ICS
#' @import ggplot2
#' @export
select_plot.data.frame <- function(df, type = c("dots", "lines"),
                                   select = NULL, ...) {
  
  # Initialization
  type <- match.arg(type)
  
  df %>% 
    ggplot(aes(x = rownames(df), index, y = gen_kurtosis, group = 0))+
    geom_point() +
    {if(type == "lines") geom_line()} +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(x = NULL, y = "Generalized Kurtosis") +
    scale_x_discrete(limits = rownames(df))
}


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
select_plot.ICS <- function(object, select = NULL, scale = FALSE, ...) {
  
  # Get the eigenvalues 
  df <- data.frame(gen_kurtosis = ICS::gen_kurtosis(object, 
                                                    select = select, 
                                                    scale = scale))
  select_plot.data.frame(df, ...)
}


#' Title
#'
#' @param object 
#' @param select 
#' @param scale 
#' @param type 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
select_plot.ICS_crit <- function(object, select = NULL, scale = FALSE, 
                                  screeplot = TRUE, ...) {
  
  
  # Plot of the gen_kurtosis
  if(isTRUE(screeplot)){
    # Get the eigenvalues 
    gen_kurtosis <- object$gen_kurtosis
    if (scale){
      gen_kurtosis <- gen_kurtosis / prod(gen_kurtosis)^(1/length(gen_kurtosis))
    } 
    df <- data.frame(gen_kurtosis = gen_kurtosis,
                     select_IC = names(gen_kurtosis) %in% object$select)
    
    select_plot.default(df, ...)
  }else{
    crit <- object$crit
    if(!(crit %in% c("med", "discriminatory"))){
      stop("The non screeplot option is only available for 'med' or 
           'discriminatory' criteria.")
    }*
    if(crit == "med"){
      med_plot(object, ...)
    }
    if(crit == "discriminatory"){
      discriminatory_plot(object, ...)
    }
  }
  
}


#' Title
#'
#' @param df 
#' @param select 
#' @param scale 
#' @param int 
#' @param color 
#' @param alpha 
#' @param screeplot 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
select_plot.default <- function(df, select = NULL, scale = FALSE, 
                                int = 0.2, color = "grey", alpha = 0.3,
                                screeplot = TRUE, ...){
 
  
  # Initialisation
  select_IC <- as.integer(sort(gsub("IC.", "", row.names(df)[df$select_IC])))
  nb_IC <- nrow(df)
  nb_first <- nb_last <- select_first <- select_last <- integer()
  
  if(length(select_IC) > 0){
    if(length(select_IC) == 1){
      if (select_IC == 1) nb_first <- select_IC
      if (select_IC == nb_IC) nb_last <- select_IC
    }else{
      nb_first <- select_IC[which(diff(select_IC) != 1)]
      nb_last <- rev(select_IC)[which(abs(diff(rev(select_IC))) != 1)]
    }
    
    if(length(nb_first) > 0){
      select_first <- range(c(1, nb_first))
    }
    if(length(nb_last) > 0){
      select_last <- range(c(nb_last, nb_IC))
    }
    
  }
  # Initial plot 
  p <- select_plot.data.frame(df, ...)

  p +
    {if(length(select_first) > 0){
      annotate(xmin = select_first[1]-int, xmax = select_first[2]+int, 
               ymin = -Inf, ymax = Inf, geom = 'rect', alpha = alpha, 
               fill = color)
    }
    }+
    {if(length(select_last) > 0){
      annotate(xmin = select_last[1]-int, xmax = select_last[2]+int, 
               ymin = -Inf, ymax = Inf, geom = 'rect', alpha = alpha, 
               fill = color)
    }
    }
}


#' Title
#'
#' @param object 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
med_plot <- function(object,  ...){
  # Get the absolute differences from the generalized kurtosis and the median
  gen_kurtosis_diff_med <- object$gen_kurtosis_diff_med
  df <- data.frame( gen_kurtosis =  gen_kurtosis_diff_med,
                      select_IC = names(gen_kurtosis_diff_med) %in% object$select)
  
  select_plot.default(df, type = "lines" )+ 
    labs(y = "|Generalized kurtosis - median|") 
    
}


discriminatory_plot <- function(object,  size = 3, color = "lightblue", ...){
  # Get the discriminatory power values associated to the combinations of ICs
  # and order them
  power <- sort(object$power_combinations, decreasing = TRUE)
  df <- data.frame(power = power,
                   select_IC = factor(names(power),
                                      levels = rev(names(power))))
  # Plot
  df %>%
    ggplot( aes(x = select_IC, y = power) ) +
    geom_segment( aes(x = select_IC, xend = select_IC, y = 0, yend = power), color = "grey") +
    geom_point(size = size, color = color) +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL, y = "Discriminatory Power") 
}




#' @export
component_plot <- function(object, ...) UseMethod("component_plot")

#' @import ICS
#' @export
component_plot.ICS <- function(object, select = TRUE, ...){
  # if select is an ICS_crit object we extract the selected components.
  if (inherits(select, "ICS_crit")){
    select <- select$select
  }
  if (isTRUE(select)){
    p <- length(ICS::gen_kurtosis(object))
    # not specified which components to plot, use defaults
    if (p <= 6L) select <- NULL
    else select <- c(seq(1,3), p-seq(2,0))
  }
  df_scores <- ICS::components(object, select = select)
  colnames(df_scores) <- paste0(gsub(".", "[", colnames(df_scores), 
                                     fixed = TRUE), "]")
  component_plot.default(df_scores, ...)
}

#' @import ICS
#' @export
component_plot.data.frame <- function(object, select = NULL, ...){
  component_plot.default(object[, select], ...)
}

#' @import ICS
#' @import GGally
#' @importFrom scales hue_pal 
#' @export
component_plot.default <- function(object, clusters = NULL, 
                                   text_size_factor = 8/6.5, 
                                   colors = NULL, ...) {
  if(is.null(clusters)) clusters <- rep("", nrow(object))
  
  df_plot <- data.frame(object, clusters  = clusters)
  column_labels <- colnames(object)
  
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