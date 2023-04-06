
#' @export
normal_crit <- function(object, ...) UseMethod("normal_crit")

#' @import ICS
#' @export
normal_crit.ICS <- function(object, ...){
  normal_crit(ICS::components(object), ...)
}


#' Normal criterion
#'
#' @param max_select 
#' @param object 
#' @param level 
#' @param test 
#' @param select_only 
#'
#' @return
#' @export
#' @importFrom moments jarque.test anscombe.test bonett.test agostino.test
#' @importFrom stats shapiro.test
normal_crit.default <- function(object, level = 0.05,  
                                test = c("agostino.test", "jarque.test", 
                                         "anscombe.test", "bonett.test", 
                                         "shapiro.test"), 
                                max_select = NULL, select_only = FALSE){
  
  # Initialization
  test <- match.arg(test)
  comp_select <- colnames(object)
  max_select <- ifelse(is.null(max_select), ncol(object)-1, max_select)

  # Apply marginal normality tests to all components and keep only the ones lower
  test_pvals <- apply(object, 2, test)
  test_pvals <- unlist(lapply(test_pvals, function(x) x$p.value))
  out <- list(crit = "normal", level = level,  max_select =  max_select, 
              test = test, pvalues = test_pvals)
  comp_signif <- (test_pvals <= level)
  
  if(sum(comp_signif) == 0){
    select <- vector()
    adjusted_levels <- level
  }else{
    # We select the components on the extreme left and right while 
    # they are not gaussian
    temp <- 1
    select <- vector()
    adjusted_levels <- level
    while (temp <= max_select) {
      
      #for each iteration we compare the first and last p value
      
      left_pval <- test_pvals[1]
      right_pval <- rev(test_pvals)[1]
      
      if (left_pval < level & left_pval < right_pval) {
        
        #we select the first component if its p value is the smallest and 
        # significant
        select <- c(select, comp_select[1])
        comp_select <- comp_select[-1]
        test_pvals <- test_pvals[-1]
        
      } else if (right_pval < level)  {
        
        # we retrieve the selected component to only look at the next one
        select <- c(select, comp_select[length(comp_select)])
        comp_select <- comp_select[-length(comp_select)]
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
  if(select_only){
    select
  }else{
    append(out,  list(adjusted_levels =  adjusted_levels, select = select) )
  }  
  
}

#' @export
med_crit <- function(object, ...) UseMethod("med_crit")

#' @import ICS
#' @export
med_crit.ICS <- function(object, ...){
  med_crit(ICS::gen_kurtosis(object), ...)
}

#'
#' @param max_select 
#' @param object The vector of generalized kurtosis from ICS object
#' @param select_only 
#'
#' @return
#' @export
#' 
#'
#' @examples
med_crit.default <- function(object, nb_select = NULL, select_only = FALSE){
  # Initialization
  nb_select <- ifelse(is.null(nb_select), length(object)-1, nb_select)
  
  # we take the components associated to the furthest eigenvalues from the median
  med_gen_kurtosis <- median(object)
  gen_kurtosis_diff <- sort(abs(object - med_gen_kurtosis), decreasing = TRUE)
  out <- names(gen_kurtosis_diff)[seq(0, nb_select)]
  
  if (!select_only) out <- append(list(crit = "med", nb_select =  nb_select, 
                                      gen_kurtosis = object, 
                                      med_gen_kurtosis = med_gen_kurtosis,
                                      gen_kurtosis_diff_med =  gen_kurtosis_diff),
                                 list(select = out))
  out
  
}

#' @export
var_crit <- function(object, ...) UseMethod("var_crit")

#' @import ICS
#' @export
var_crit.ICS <- function(object, ...){
  var_crit(ICS::gen_kurtosis(object), ...)
}


#' Rollvar to select the ICS component
#' 
#' This function selects the components to keep based on the rolling variance
#' criteria used in `ICSboot`.
#'
#' @param object The vector of gen_kurtosis from ICS object
#' @param nb_select The number of non-gaussian components under the null.
#' @param select_only 
#'
#' @return
#' @export
#' @examples
var_crit.default <- function(object, nb_select = NULL, select_only = FALSE){
  # Initialization
  d <- length(object)
  nb_select <- ifelse(is.null(nb_select), d-1, nb_select)
  
  # if the number of non-gaussian components is equal or higher to p-1,
  # it makes no sense to compute the rolling variance of one component
  if (nb_select >= (d-1)){
    warning("The nb_select is higher or equal to the number of variables
            minus one so, it makes no sense to select some components based on
            the rolling variance of only one invariant component.")
    out <- vector()
  }else{
    orderD <- fixOrder(object, d-nb_select)
    out <- names(object)[orderD$Order[seq(0,nb_select)]]
    if (!select_only) out <- append(list(crit = "var", nb_select = nb_select,
                                         gen_kurtosis = object, select = out), 
                                    orderD)
  }
  
  out
  
}

#' Fix Order
#'
#' @param x 
#' @param nb_spherical
#'
#' @return
#' @examples
#' @importFrom RcppRoll roll_var 
fixOrder <- function (x, nb_spherical) 
{
  P <- length(x)
  Index <- seq(1, P)
  RollVarX <- RcppRoll::roll_var(x, nb_spherical)
  Start <- which.min(RollVarX)
  End <- Start + nb_spherical - 1
  Smallest <- seq(Start, End)
  Order <- c(Index[-Smallest], Index[Smallest])
  Ox <- x[Order]
 # RollVarOX <- roll_var(Ox, nb_spherical)
  RES <- list(RollVarX = RollVarX, Selected = Smallest, Order = Order, 
              xOrdered = Ox)
  RES
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
#' This function selects the components to keep based on a discriminatory power 
#' (by default "eta2")
#'
#' @param object 
#' @param clusters 
#' @param nb_select 
#' @param method 
#' @param select_only 
#' 
#' @return
#' @export
#' 
#' @importFrom heplots etasq
#'
#' @examples
discriminatory_crit.default <- function(object, clusters, method = "eta2", 
                                        nb_select = NULL, select_only = FALSE){
  # Initialization
  method <- match.arg(method)
  nb_select <- ifelse(is.null(nb_select), ncol(object)-1, nb_select)
  # First we construct all potential combinations of first and last components
  # to analyze
  d <- ncol(object)
  IC_last <- rev(sapply(seq(0, nb_select), function(i){
    if(i == nb_select){
      IC_last <- 0
    }else{
      IC_last <- d-(seq((nb_select-(i+1)),0))
    }
  }, simplify = FALSE))
  
  IC_first <- sapply(seq(0, nb_select), function(i){seq(0,(nb_select-i))})
  
  all_comb <- sapply(seq(1, (nb_select+1)), function(i){
    IC_all = sort(c(IC_first[[i]], IC_last[[i]]))
    IC_all[IC_all != 0]
  })
  
  if(nb_select == 1){
    all_comb <- data.frame(all_comb[1], all_comb[2])
  }
  
  
  # we compute the discriminatory power for each combination to identify which
  # one has the highest power.
  if (method == "eta2"){
    all_power <- sapply(seq(1, ncol(all_comb)), function(i){
      eta2_power(object, clusters, select = all_comb[,i])
    })
    names(all_power) <- apply(all_comb, 2, function(x) 
      paste(paste("IC", x, sep = "."), collapse = ","))
    
    # We select the combination with the highest power
    ind <- which.max(all_power)
    select <- all_comb[,ind]
    power <- all_power[ind]
  }else{
    warning("No other method than 'eta2' has been implemented yet.")
    select <- vector()
    power <- 0
  }
  
  out <- colnames(object)[select]
  if (!select_only) out <- list(crit = "discriminatory",
                               method = method, nb_select = nb_select, 
                               select = out, power = power,
                               power_combinations = all_power)
  out
}



eta2_power <- function(object, clusters, select){
  if(is.null(clusters)){
    warning("The 'clusters' argument is mandatory to compute the discriminatory 
            power of the reduced data frame.")
  }else{
    df <- data.frame(clusters = as.factor(clusters), object[, select])
    
    # Univariate case: ANOVA
    if(length(select) == 1){
      
      ICS_mod <- lm(as.formula(paste("cbind(", colnames(df)[-1], ") ~ clusters")),
                    data = df)
      etasq(ICS_mod)[1,1]
      
    }else{
      # Multivariate case: MANOVO with Wilks test
      
      ICS_mod <- manova(as.formula(paste("cbind(", 
                                         paste(colnames(df)[-1], collapse = ","), 
                                         ") ~ clusters")), data = df)
      etasq(ICS_mod, test = "Wilks")[1,1]
    }
  }
}