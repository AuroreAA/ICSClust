# Plot of the generalized eigenvalues -------------------------------------

#' Plot the Generalized Kurtosis Values of the ICS Transformation
#'
#' Extract the generalized kurtosis values of the components obtained via an
#' ICS transformation and draw either a screeplot or a specific plot for a 
#' given criterion. If an object of class `ICS_crit` is given, then the 
#' selected components are shaded on the plot.
#' 
#' @param object an object inheriting from class \code{"ICS"} containing
#' results from an ICS transformation or from class `ICS_crit`.
#' @param select  an integer, character, or logical vector specifying for 
#' which components to extract the generalized kurtosis values, or 
#' \code{NULL} for extracting the generalized kurtosis values of all 
#' components.
#' @param scale  a logical indicating whether to scale the generalized 
#' kurtosis values to have product 1 (defaults to \code{FALSE}).
#' @param type either "dots" or "lines" for the type of plot
#' @param screeplot boolean. If `TRUE` a plot of the generalized kurtosis 
#' values is drawn. Otherwise it is context specific to the `ICS_crit` object.
#' For "med" criterion, the differences between the kurtosis values and the 
#' median are plotted in absolute values. For "discriminatory" the 
#' discriminatory power associated to the evaluated combinations are drawn.
#' @param int the width for shading the selected components in case an 
#' `ICS_crit` object is given.
#' @param color the color for shading the selected components in case an 
#' `ICS_crit` object is given.
#' @param alpha the transparency for shading the selected components in case 
#' an `ICS_crit` object is given.
#' @param ... not used.
#' @author Andreas Alfons and Aurore Archimbaud
#' @import ICS
#' @import ggplot2
#' @export
#' 
#' @rdname select_plot
#' @examples
#' \dontrun{
#' X <- iris[,-5]
#' out <- ICS(X)
#' 
#' # on an ICS object
#' select_plot(out)
#' select_plot(out, type = "lines")
#' 
#' # on an ICS_crit object
#' select <- med_crit(res, nb_select = 1, select_only = FALSE)
#' select_plot(out, type = "lines")
#' select_plot(out, screeplot = FALSE, type = "lines", color = "lightblue")
#' }
#' 
select_plot <- function(object, ...) UseMethod("select_plot")

#' @method select_plot default
#' @rdname select_plot
#' @export
select_plot.default <- function(object, type = c("dots", "lines"),
                                select = NULL, scale = FALSE, 
                                screeplot = TRUE, ...) {
  
  # Initialization
  type <- match.arg(type)
  
  # Get the data to plot 
  out <- df_select_plot(object, select = select, scale = scale,
                        screeplot = screeplot)
  
  if(!(inherits(out, "data.frame") | inherits(out, "ICS_crit"))){
    stop("You can apply 'select_plot' only to an object of class 'ICS' or 'ICS_crit'.")
  }else{
    select_plot(out, type = type, ...)
  }
  
}


#' @method select_plot data.frame
#' @rdname select_plot
#' @export
select_plot.data.frame <- function(out, type = c("dots", "lines"), ...) {
  scree_plot(out, type = type, ...)
}

#' @method select_plot ICS_crit
#' @rdname select_plot
#' @export
select_plot.ICS_crit <- function(out,  ...) {
  crit <- out$crit
  if(!(crit %in% c("med", "discriminatory"))){
    stop("The non screeplot option is only available for 'med' or 
           'discriminatory' criteria.")
  }else if(crit == "med"){
    med_plot(out, ...)
  }else if(crit == "discriminatory"){
    discriminatory_plot(out, ...)
  }
  
}




## Extract kurtosis values ----------------------------------------------


#' Extract kurtosis values
#' 
#' Extract the kurtosis values for an invariant 
#' coordinate system obtained via an ICS transformation or for an object of 
#' class 'ICS_crit'.
#' @noRd
df_select_plot <- function(object, select = NULL, scale = FALSE,
                           screeplot = TRUE) UseMethod("df_select_plot")

df_select_plot.ICS <- function(object, select = NULL, scale = FALSE, 
                               screeplot = TRUE) {
  
  # Get the eigenvalues 
  data.frame(gen_kurtosis = ICS::gen_kurtosis(object, select = select, 
                                              scale = scale),
             select_IC = FALSE, crit = NA)
}

#' @noRd
df_select_plot.ICS_crit <- function(object, select = NULL, scale = FALSE, 
                                    screeplot = TRUE) {
  # If screeplot 
  if(isTRUE(screeplot)){
    # Get the eigenvalues 
    gen_kurtosis <- object$gen_kurtosis
    if (scale){
      gen_kurtosis <- gen_kurtosis / prod(gen_kurtosis)^(1/length(gen_kurtosis))
    } 
    data.frame(gen_kurtosis = gen_kurtosis,
               select_IC = names(gen_kurtosis) %in% object$select,
               crit = object$crit)
    
  }else{
    # Return the ICS_crit object
    object
  }
}

#' @noRd
df_select_plot.default <- function(object, select = NULL, scale = FALSE, 
                                   screeplot = TRUE){
  integer()
}

## Plots -------------------------------------------------------------------
#' @noRd
scree_plot <- function(df, type = c("dots", "lines"), int = 0.2, color = "grey",
                       alpha = 0.3, ...){
  
  # Initialisation
  select_IC <- as.integer(sort(gsub("IC.", "", row.names(df)[df$select_IC])))
  nb_IC <- nrow(df)
  crit <- paste("IC selected with the", unique(df$crit), "criterion")
  df_zones <-  data.frame(zone = vector(),
                          start = vector(),
                          end   = vector())
  
  if(length(select_IC) > 0){
    nb_first <- select_IC[which(df$select_IC == FALSE)[1]-1]
    nb_last <- rev(select_IC)[which(rev(df$select_IC) == FALSE)[1]-1]
    
    if(length(nb_first) > 0){
      df_zones <- rbind(df_zones, c(zone = crit, start = 1-int, 
                                    end = nb_first +int))
    }
    if(length(nb_last) > 0){
      df_zones <- rbind(df_zones, c(zone = crit, start = nb_last-int, 
                                    end = nb_IC+int))
    }
    
    colnames(df_zones) <-  c("zone", "start", "end")
    df_zones$zone <- factor(df_zones$zone, levels = unique(df_zones$zone))
    df_zones$start <- as.numeric(df_zones$start)
    df_zones$end <- as.numeric(df_zones$end)
    
  }
  
  
  # Initial plot 
  p <- df %>% 
    ggplot(aes(x = rownames(df), index, y = gen_kurtosis, group = 0))+
    geom_point() +
    {if(type == "lines") geom_line()} +
    theme_minimal() +
    #theme(text = element_text(size = 20)) +
    labs(x = NULL, y = "Generalized Kurtosis") +
    scale_x_discrete(limits = rownames(df))
  
  # Add shadow areas
  if(nrow(df_zones) > 0){
    p <- p + geom_rect(data = df_zones,
                       inherit.aes = FALSE,
                       aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                           fill = zone),
                       alpha = alpha)  +  scale_fill_manual('',
                                                            values = color)+
      theme(legend.position = "top")
  }
  
  p
  
}

#' @noRd
med_plot <- function(object, ...){
  # Get the absolute differences from the generalized kurtosis and the median
  gen_kurtosis_diff_med <- object$gen_kurtosis_diff_med
  df <- data.frame( gen_kurtosis =  gen_kurtosis_diff_med,
                    select_IC = names(gen_kurtosis_diff_med) %in% object$select,
                    crit = object$crit)
  # Draw the plot
  scree_plot(df, ... )+ 
    labs(y = "|Generalized kurtosis - median|")
  
}

#' @noRd
discriminatory_plot <- function(object,  size = 3, color = "lightblue", ...){
  # Get the discriminatory power values associated to the combinations of ICs
  # and order them
  power <- sort(object$power_combinations, decreasing = TRUE)
  df <- data.frame(power = power,
                   select_IC = factor(names(power),
                                      levels = rev(names(power))),
                   crit = object$crit)
  # Plot
  df %>%
    ggplot( aes(x = select_IC, y = power) ) +
    geom_segment( aes(x = select_IC, xend = select_IC, y = 0, yend = power), 
                  color = "grey") +
    geom_point(size = size, color = color) +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL, y = "Discriminatory Power") 
}



# Plot of the selected variables/components -------------------------------

#' Scatterplot Matrix with densities on the diagonal
#' 
#' Produce a scatterplot matrix of the component scores of a given dataframe 
#' or an invariant coordinate system obtained via an ICS transformation with 
#' densities on the diagonal for each cluster.
#' 
#' @param object a dataframe or \code{\link[=ICS-S3]{ICS}} class object
#' @param select a vector of indexes of variables to plot. If `NULL` or 
#' `FALSE`, all variables are selected. If `TRUE` only the first three and 
#' last three are considered.
#' @param clusters a vector indicating the clusters of the data to color the 
#' plot. By default `NULL`.
#' @param text_size_factor a numeric factor for controlling the `axis.text`  
#' and `strip.text`. 
#' @param colors a vector of colors to use. One color for each cluster.
#' @param ... not used.
#' @export
#' 
#' @rdname component_plot
#' 
#' @author Andreas Alfons and Aurore Archimbaud
#' @examples
#' \dontrun{
#' X <- iris[,1:4]
#' component_plot(X)
#' out <- ICS(X)
#' component_plot(out, select = c(1,4))
#' }
#' 
component_plot <- function(object, ...) UseMethod("component_plot")

#' @rdname component_plot
#' @method component_plot default
#' @import GGally
#' @importFrom scales hue_pal 
#' @export
component_plot.default <- function(object, select = TRUE, 
                                   clusters = NULL, 
                                   text_size_factor = 8/6.5, 
                                   colors = NULL, ...) {
  
  # Initialisation
  if(isFALSE(select)) select <- NULL
  
  # Get the data to plot 
  df <- df_component_plot(object, select = select)
  
  # Plot
  if(ncol(df) == 0){
    stop("You can apply 'component_plot' only to an object of class 'ICS' or a data.frame.")
  }else{
    # initialisation
    if(is.null(clusters)) clusters <- rep("", nrow(df))
    df_plot <- data.frame(df, clusters  = clusters)
    column_labels <- colnames(df)
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
}



## Extract selected variables ----------------------------------------------


#' Extract selected variables
#' 
#' Extract the selected variables for a given dataframe or an invariant 
#' coordinate system obtained via an ICS transformation.
#' 
#' @param object a dataframe or \code{\link[=ICS-S3]{ICS}} class object
#' @param select a vector of indexes of variables to plot. If `NULL` or 
#' `FALSE`, all variables are selected. If `TRUE` only the first and last three
#'  are considered.
#' @return A dataframe.
#' @noRd
df_component_plot <- function(object, select = NULL) UseMethod("df_component_plot")

df_component_plot.default <- function(object, select = NULL){
  data.frame()
}

#' @noRd
df_component_plot.ICS <- function(object, select = TRUE){
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
  df_scores
}

#' @noRd
df_component_plot.data.frame <- function(object, select = NULL){
  # Initialisation
  p <- ncol(object)
  if (isTRUE(select)){
    # not specified which components to plot, use defaults
    if (p <= 6L) select <- NULL
    else select <- c(seq(1,3), p-seq(2,0))
  }
  if (is.null(select)){
    select <- seq(1,p)
  }
  object[, select]
}

