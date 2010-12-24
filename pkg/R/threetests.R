waldtest.mlogit <- function(object, ..., hyp = c("fixed", "uncorrelated")){
  objects <- list(object, ...)
  nmodels <- length(objects)
  
  # if several models are provided, just use the default method
  if (nmodels > 1){
    return(waldtest.default(object, ...))
  }
  K <- length(colnames(model.matrix(object)))
  L <- length(object$freq)
  
  # guess the nature of the fitted model
  mixed.logit <- !is.null(object$call$rpar)
  heterosc.logit <- !is.null(object$call$heterosc) && object$call$heterosc
  nested.logit <- !is.null(object$call$nests)


  if (mixed.logit){
    hyp <- match.arg(hyp)
    correlation <- !is.null(attr(object$rpar, "covariance"))
    J <- length(object$rpar)
    if (correlation){
      rd.el <- K+(1:(J*(J+1)/2))
      diag.el <- K + c(1, cumsum(J:2)+1)
    }
    else rd.el <- K + (1:J)
    if (hyp == "uncorrelated"){
      if (!correlation) stop("no correlation")
      rd.el <- K+(1:(J*(J+1)/2))
      su <- rd.el[!(rd.el %in% diag.el)]
    }
    else su <- rd.el
  }
  
  if (heterosc.logit){
    su <- (K+1):(K+L-1)
    q <- rep(1, length(su))
  }
  
  if (nested.logit){
    su <- (K+1):length(coef(object))
    q <- rep(1, length(su))
  }  
  if (is.null(q)) wq <- coef(object)[su] else wq <- coef(object)[su] - q
  stat <- as.numeric(crossprod(wq,
                               crossprod(solve(vcov(object)[su, su]),
                                         wq)))

  names(stat) <- 'chisq'
    df <- c(df = length(su))
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  result <- list(statistic = stat,
                 parameter = df,
                 p.value = pval,
                 data.name = hyp,
                 method = "Wald test"
  #                 alternative = "unconstrainted model"
                 )
  class(result) <- 'htest'
    result

  
}

lrtest.mlogit <- function(object, ...){
  dots <- list(...)
  if (length(dots) == 0){
    model2 <- update(object, heterosc=FALSE, rpar = NULL,
                     start = NULL, nests = NULL,
                     gleontief = FALSE, method = 'nr')
    lrtest.default(object, model2)
  }
  else lrtest.default(object, ...)
}

scoretest <- function(object, ...){
  UseMethod("scoretest")
}

scoretest.mlogit <- function(object, ..., heterosc = FALSE, nests = NULL, rpar = NULL){
  objects <- list(object, ...)
  nmodels <- length(objects)
  
  # if several models are provided, just use the default method
  if (nmodels > 1){
    return(scoretest.default(object, ...))
  }
  heterosc.logit <- heterosc
  nested.logit <- !is.null(nests)
  mixed.logit <- !is.null(rpar)
  if (heterosc.logit + nested.logit + mixed.logit == 0) stop("an unconstrained model should be described")
  if (heterosc.logit + nested.logit + mixed.logit > 1) stop("only on unconstrained model should be described")
  if (heterosc.logit){
    alt.hyp = "heteroscedastic model"
    data.name = "heterosc = TRUE"
  }
  if (nested.logit){
    alt.hyp = "nested model"
    data.name = "rien"#as.character(nests)
  }
  if (mixed.logit){
    alt.hyp = "mixed model"
    data.name = as.character(rpar)
  }
  mc <- match.call()
  mc[[1]] <- as.name('update')
  mc[['iterlim']] <- 0
  mc[['method']] <- 'bfgs'
  mc[['start']] <- c(coef(object))
  mc[['print.level']] <- 0
  newmodel <- eval(mc, parent.frame())
  stat <- - sum(newmodel$gradient * solve(newmodel$hessian, newmodel$gradient))
  names(stat) <- "chisq"
  df <- length(coef(newmodel)) - length(coef(object))
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  result <- list(statistic = stat,
                 parameter = df,
                 p.value = pval,
                 data.name = data.name,
                 method = "score test",
                 alternative = alt.hyp
                 )
  class(result) <- 'htest'
  result
}


scoretest.default <- function(object, ...){
  new <- list(...)[[1]]
  cls <- class(object)[1]
  nmodels <- length(new)
  if (!inherits(new, 'formula') & !inherits(new, cls)) stop("the updating argument doesn't have a correct class")
  if (inherits(new, cls)){
    ncoefs <- names(coef(new))
    new <- formula(formula(new))
  }
  else ncoefs <- names(coef(update(object, new, iterlim = 0)))
  start <- numeric(length = length(ncoefs))
  names(start) <- ncoefs
  supcoef <- ! ncoefs %in% names(coef(object))
  start[names(coef(object))] <- coef(object)
  newmodel <- update(object, new, start= start, iterlim = 0)
  data.name <- paste(deparse(formula(newmodel)))
  alt.hyp <- "unconstrained model"
  stat <- - sum(newmodel$gradient * solve(newmodel$hessian, newmodel$gradient))
  names(stat) <- "chisq"
  df <- length(coef(newmodel)) - length(coef(object))
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  result <- list(statistic = stat,
                 parameter = df,
                 p.value = pval,
                 data.name = data.name,
                 method = "score test",
                 alternative = alt.hyp
                 )
  class(result) <- 'htest'
  result
}
    
