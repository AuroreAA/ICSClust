
#' @export
normal_crit <- function(object, ...) UseMethod("normal_crit")

#' @import ICS
#' @export
normal_crit.ICS <- function(object, ...){
  normal_crit(ICS::components(object), ...)
}
#' Title
#'
#' @param index_max 
#' @param object 
#' @param level 
#' @param test 
#' @param index_only 
#'
#' @return
#' @export
#' @importFrom moments jarque.test anscombe.test bonett.test agostino.test
#' @importFrom stats shapiro.test
normal_crit.default <- function(object, level = 0.05,  test = c("jarque.test", "anscombe.test", "bonett.test", "agostino.test", "shapiro.test"), index_max = NULL, index_only = FALSE){
  
  # Initialization
  test <- match.arg(test)
  comp_index <- colnames(object)
  index_max <- ifelse(is.null(index_max), ncol(object)-1, index_max)

  # Apply marginal normality tests to all components and keep only the ones lower
  test_pvals <- apply(object, 2, test)
  test_pvals <- unlist(lapply(test_pvals, function(x) x$p.value))
  out <- list(crit = "normal", level = level,  index_max =  index_max, test = test, pvalues = test_pvals)
  comp_signif <- (test_pvals <= level)
  
  if(sum(comp_signif) == 0){
    index <- c()
    adjusted_levels <- level
  }else{
    # We select the components on the extreme left and right while they are not gaussian
    temp <- 1
    index <- c()
    adjusted_levels <- level
    while (temp <= index_max) {
      
      #for each iteration we compare the first and last p value
      
      left_pval <- test_pvals[1]
      right_pval <- rev(test_pvals)[1]
      
      if (left_pval < level & left_pval < right_pval) {
        
        #we select the first component if its p value is the smallest and significant
        index <- c(index, comp_index[1])
        comp_index <- comp_index[-1]
        test_pvals <- test_pvals[-1]
        
      } else if (right_pval < level)  {
        
        # we retrieve the selected component to only look at the next one
        index <- c(index, comp_index[length(comp_index)])
        comp_index <- comp_index[-length(comp_index)]
        test_pvals <- test_pvals[-length(test_pvals)]
        
      } else {
        
        break
        
      }
      
      temp <- temp + 1
      # we adjust the alpha level by dividing by the old weight and multiplying 
      # by the new one.
      test_pvals <- test_pvals * temp/(temp-1)
      adjusted_levels <- c(adjusted_levels, level/temp)
      
    }
  }
  if(is.null(index)) index <- c()
  
  if(index_only){
    index
  }else{
    append(out,  list(adjusted_levels =  adjusted_levels, index = index) )
  }  
  
}

#' @export
med_crit <- function(object, ...) UseMethod("med_crit")

#' @import ICS
#' @export
med_crit.ICS <- function(object, ...){
  med_crit(ICS::lambda(object), ...)
}

#'
#' @param index_max 
#' @param object The vector of lambdas from ICS object
#' @param index_only 
#'
#' @return
#' @export
#' 
#'
#' @examples
med_crit.default <- function(object, index_max = NULL, index_only = FALSE){
  # Initialization
  index_max <- ifelse(is.null(index_max), length(object)-1, index_max)
  
  # we take the components associated to the furthest eigenvalues from the median
  med_lambda <- median(object)
  lambda_diff <- sort(abs(object - med_lambda), decreasing = TRUE)
  out <- names(lambda_diff)[1:index_max]
  if (!index_only) out <- append(list(crit = "med", index_max =  index_max, 
                                      lambda = object, med_lambda = med_lambda,
                                      lambda_diff_med =  lambda_diff),
                                 list(index = out))
  out
  
}

#' @export
var_crit <- function(object, ...) UseMethod("var_crit")

#' @import ICS
#' @export
var_crit.ICS <- function(object, ...){
  var_crit(ICS::lambda(object), ...)
}


#' Rollvar to select the ICS component
#' 
#' This function selects the components to keep based on the rolling variance criteria used in `ICSboot`.
#'
#' @param object The vector of lambdas from ICS object
#' @param index_max The number of non-gaussian components under the null.
#' @param index_only 
#'
#' @return
#' @export
#' 
#' @import ICtest
#'
#' @examples
var_crit.default <- function(object, index_max = NULL, index_only = FALSE){
  # Initialization
  index_max <- ifelse(is.null(index_max), length(object)-1, index_max)
  d = length(object)
  # if the number of non-gaussian components is equal or higher to p-1,
  # it makes no sense to compute the rolling variance of one component
  if (index_max >= (d-1)){
    warning("The index_max is higher or equal to d-1 so it makes no sense to compute the rolling variance in this context.")
    out <- c()
  }else{
    orderD <- ICtest:::fixOrder(object, d-index_max)
    out <- names(object)[orderD$Order[0:index_max]]
    if (!index_only) out <- append(list(crit = "var", index_max = index_max, lambda = object, index = out), orderD)
  }
  
  out
  
}

#' @export
discriminatory_crit <- function(object, ...) UseMethod("discriminatory_crit")

#' @import ICS
#' @export
discriminatory_crit.ICS <- function(object, ...){
  discriminatory_crit(ICS::components(object), ...)
}

#' Selection of ICS components based on discriminatory power
#' 
#' This function selects the components to keep based on a discriminatory power (by default "eta2")
#'
#' @param object 
#' @param clusters 
#' @param index_max 
#' @param method 
#' @param index_only 
#' 
#' @return
#' @export
#' 
#' @importFrom heplots etasq
#'
#' @examples
discriminatory_crit.default <- function(object, clusters, method = "eta2", index_max = NULL, index_only = FALSE){
  # Initialization
  method <- match.arg(method)
  index_max <- ifelse(is.null(index_max), ncol(object)-1, index_max)
  # First we construct all potential combinations of first and last components
  # to analyze
  d <- ncol(object)
  IC_last <- rev(sapply(0:index_max, function(i){
    if(i == index_max){
      IC_last <- 0
    }else{
      IC_last <- d-((index_max-(i+1)):0)
    }
  }, simplify = FALSE))
  
  IC_first <- sapply(0:index_max, function(i){0:(index_max-i)})
  
  all_comb <- sapply(1:(index_max+1), function(i){
    IC_all = sort(c(IC_first[[i]], IC_last[[i]]))
    IC_all[IC_all != 0]
  })
  
  if(index_max == 1){
    all_comb <- data.frame(all_comb[1], all_comb[2])
  }
  
  
  # we compute the discriminatory power for each combination to identify which one has the highest power.
  if (method == "eta2"){
    all_power <- sapply(1:ncol(all_comb), function(i){
      eta2_power(object, clusters, index = all_comb[,i])
    })
    names(all_power) <- apply(all_comb, 2, function(x) 
      paste(paste("IC", x, sep = "."), collapse = ","))
    
    # We select the combination with the highest power
    ind <- which.max(all_power)
    index <- all_comb[,ind]
    power <- all_power[ind]
  }else{
    warning("No other method than 'eta2' has been implemented yet.")
    index <- c()
    power <- 0
  }
  
  out <- colnames(object)[index]
  if (!index_only) out <- list(crit = "discriminatory",
                               method = method, index_max = index_max, 
                               index = out, power = power,
                               power_combinations = all_power)
  out
}



eta2_power <- function(object, clusters, index){
  if(is.null(clusters)){
    warning("The 'clusters' argument is mandatory to compute the discriminatory power of the reduced data frame.")
  }else{
    df <- data.frame(clusters = as.factor(clusters), object[, index])
    
    # Univariate case: ANOVA
    if(length(index) == 1){
      
      ICS_mod <- lm(as.formula(paste("cbind(", colnames(df)[-1], ") ~ clusters")), data = df)
      etasq(ICS_mod)[1,1]
      
    }else{
      # Multivariate case: MANOVO with Wilks test
      
      ICS_mod <- manova(as.formula(paste("cbind(", paste(colnames(df)[-1], collapse = ",") , ") ~ clusters")), data = df)
      etasq(ICS_mod, test = "Wilks")[1,1]
    }
  }
}