#' Title
#'
#' @param object Object of class 'ICS'
#' @param crit One of teh following criteria: 'normal', 'med', 'var', 
#' 'discriminatory'.
#' @param ... 
#' @param comp_max The maximal number of components to select.
#' @param k The number of known clusters.
#'
#' @return
#' @export
#'
#' @examples
#' @import ICS
ICS_comp_crit <- function(object, crit = "normal", crit_args = list(),
                          comp_max = NULL, groups_vec = NULL, ...){
  if (class(object) != "ICS") stop("'object' must be of class ICS")
  
  if(any(is.infinite(object$lambda))){
    base::simpleError("infinite eigenvalues")
  }else{
    comp_max <- ifelse(is.null(comp_max), ncol(object$scores)-1, comp_max)
    
    comp_crit <- switch(crit, 
           normal = normal_crit(df_scores = object$scores, crit_args = crit_args, comp_max = comp_max),
           med = med_crit(lambda_vec = object$lambda, comp_max = comp_max),
           var = var_crit(lambda_vec = object$lambda, comp_max = comp_max),
           discriminatory = discriminatory_crit(df_scores = object$scores, groups_vec, crit_args = list(method = "eta2"), comp_max = comp_max)
    )
    
    comp_crit
  }
  
}

#' Title
#'
#' @param crit_args 
#' @param comp_max 
#'
#' @return
#' @export
#' @importFrom moments jarque.test anscombe.test bonett.test agostino.test
#' @importFrom stats shapiro.test
#'
#' @examples
normal_crit <- function(df_scores, crit_args = list(level = 0.05,  test = "agostino.test"), comp_max = NULL){
  
  # Initialization
  level = crit_args$level
  test <- match.arg(crit_args$test, c("jarque.test", "anscombe.test", 
                                      "bonett.test", "agostino.test", 
                                      "shapiro.test"))
  comp_index <- 1:ncol(df_scores)
  
  
  # Apply marginal normality tests to all components and keep only the ones lower
  test_pvals <- apply(df_scores, 2, test)
  test_pvals <- unlist(lapply(test_pvals, function(x) x$p.value))
  comp_signif <- (test_pvals <= level)
  if(sum(comp_signif) == 0){
    selected <- NULL
  }else{
    # We select the components on the extreme left and right while they are not gaussian
    temp <- 1
    comp_selected <- c()
    
    while (temp <= comp_max) {
      
      #for each iteration we compare the first and last p value
      
      left_pval <- test_pvals[1]
      right_pval <- rev(test_pvals)[1]
      
      if (left_pval < level & left_pval < right_pval) {
        
        #we select the first component if its p value is the smallest and significant
        comp_selected <- c(comp_selected, comp_index[1])
        comp_index <- comp_index[-1]
        test_pvals <- test_pvals[-1]
        
      } else if (right_pval < level)  {
        
        # we retrieve the selected component to only look at the next one
        comp_selected <- c(comp_selected, comp_index[length(comp_index)])
        comp_index <- comp_index[-length(comp_index)]
        test_pvals <- test_pvals[-length(test_pvals)]
        
      } else {
        
        break
        
      }
      
      temp <- temp + 1
      # we adjust the alpha level by dividing by the old weight and multiplying 
      # by the new one.
      test_pvals <- test_pvals * temp/(temp-1)
      
    }
  }
  
  return(list(comp_selected = comp_selected))
  
}


#'
#' @param comp_max 
#' @param lambda_vec The vector of lambdas from ICS object
#'
#' @return
#' @export
#' 
#'
#' @examples
med_crit <- function(lambda_vec, comp_max = NULL){
  #we take the components associated to the furthest eigenvalues from the median
  med_lambda <- median(lambda_vec)
  comp_selected <- order(abs(lambda_vec - med_lambda), decreasing = TRUE)[1:comp_max]
  return(list(comp_selected = comp_selected))
}

#' Rollvar to select the ICS component
#' 
#' This function selects the components to keep based on the rolling variance criteria used in `ICSboot`.
#'
#' @param lambda_vec The vector of lambdas from ICS object
#' @param comp_max The number of non-gaussian components under the null.
#'
#' @return
#' @export
#' 
#' @import ICtest
#'
#' @examples
var_crit <- function(lambda_vec, comp_max = NULL){
  d = length(lambda_vec)
  # if the number of non-gaussian components is equal or higher to p-1,
  # it makes no sense to compute the rolling variance of one component
  if (comp_max >= (d-1)){
    warning("The comp_max is higher or equal to d-1 so it makes no sense to compute the rolling variance in this context.")
   comp_selected <- NULL
  }else{
    orderD <- ICtest:::fixOrder(lambda_vec, d-comp_max)
    comp_selected <- orderD$Order[0:comp_max]
  }
  return(list(comp_selected = comp_selected))
}

#' Selection of ICS components based on discriminatory power
#' 
#' This function selects the components to keep based on a discriminatory power (by default "eta2")
#'
#' @param df_scores 
#' @param crit_args 
#' @param groups_vec 
#' @param comp_max 
#' 
#' @return
#' @export
#' 
#' @importFrom heplots etasq
#'
#' @examples
discriminatory_crit <- function(df_scores, groups_vec,  crit_args = list(method = "eta2"), comp_max = NULL){
  # First we construct all potential combinations of first and last components
  # to analyze
  d <- ncol(df_scores)
  IC_last <- rev(sapply(0:comp_max, function(i){
    if(i == comp_max){
      IC_last <- 0
    }else{
      IC_last <- d-((comp_max-(i+1)):0)
    }
  }, simplify = FALSE))
  
  IC_first <- sapply(0:comp_max, function(i){0:(comp_max-i)})
  
  all_comb <- sapply(1:(comp_max+1), function(i){
    IC_all = sort(c(IC_first[[i]], IC_last[[i]]))
    IC_all[IC_all != 0]
  })
  
  if(comp_max == 1){
    all_comb <- data.frame(all_comb[1], all_comb[2])
  }
  
  
  # we compute the discriminatory power for each combination to identify which one has the highest power.
  if (crit_args$method == "eta2"){
    all_power <- sapply(1:ncol(all_comb), function(i){
      eta2_power(df_scores, groups_vec, comp_selected = all_comb[,i])
    })
    
    # We select the combination with the highest power
    ind <- which.max(all_power)
    comp_selected <- all_comb[,ind]
    power <- all_power[ind]
  }else{
    warning("No other method than 'eta2' has been implemented yet.")
    comp_selected <- NULL
    power <- 0
  }
  
  return(list(comp_selected = comp_selected, power = power))
  
}

eta2_power <- function(df_scores, groups_vec, comp_selected){
  if(is.null(groups_vec)){
    warning("The 'groups_vec' argument is mandatory to compute the discriminatory power of the reduced data frame.")
  }else{
    df <- data.frame(df_scores, groups = as.factor(groups_vec))
    
    # Univariate case: ANOVA
    if(length(comp_selected) == 1){
      
      ICS_mod <- lm(as.formula(paste("cbind(", colnames(df)[comp_selected], ") ~ groups")), data = df)
      etasq(ICS_mod)[1,1]
      
    }else{
    # Multivariate case: MANOVO with Wilks test
      
      ICS_mod <- manova(as.formula(paste("cbind(", paste(colnames(df)[comp_selected], collapse = ",") , ") ~ groups")), data = df)
      etasq(ICS_mod, test = "Wilks")[1,1]
    }
  }
}