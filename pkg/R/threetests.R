waldtest.mlogit <- function(object, ..., hyp = NULL){
  objects <- list(object, ...)
  nmodels <- length(objects)
  specific.computation <- FALSE
  
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

  ###### Heteroscedastic logit model
  # the hypothesis is that J-1 parameters = 1
  if (heterosc.logit){
    su <- (K+1):(K+L-1)
    q <- rep(1, length(su))
  }
  
  ###### Nested logit Models
  # in case of nested model : is there a unique nest elasticity
  # if un.nest.el = FALSE constrained model may be
  #    - nested model with unique elasticity : hyp = "un.nest.el"
  #    - multinomial : hyp = "multinomial"
  # if un.nest.el = TRUE : constrained model is the multinomial logit model
  if (nested.logit){
    if (is.null(object$call$un.nest.el)) un.nest.el <- FALSE
    else un.nest.el <- object$call$un.nest.el
    if (!un.nest.el){
      if (is.null(hyp)) hyp <- "no.nests"
      if (!hyp %in% c("no.nests", "un.nest.el"))
        stop("hyp should be one of no.nests or un.nest.el")
    }
    else{
      if (!is.null(hyp)) warning("hyp is irrelevant in this setting and will be ignored")
    }
    if (un.nest.el){
      su <- K + 1
      q <- 1
      data.name <- "no nests"
    }
    else{
      su <- (K+1):length(coef(object))
      if (!is.null(hyp) && hyp == "un.nest.el"){
        R <- matrix(0, nrow = length(coef(object)), ncol = length(su) - 1)
        for (i in 1:ncol(R)){
          R[K + 1, i] <- 1
          R[K + 1 + i, i] <- -1
        }
        Rb <- crossprod(R, coef(object))
        VRV <- t(R) %*% vcov(object) %*% R
        stat <- as.numeric(crossprod(Rb,solve(VRV, Rb)))
        df <- c(df = length(su) - 1)
        specific.computation <- TRUE
        data.name <- "unique nest elasticity"
      }
      else{
        q <- rep(1, length(su))
        data.name <- "no nests"
      }
    }
  }
  
  ###### Mixed logit model
  # in case of mixed model : is it correlated or not
  # if correlated : constrained model may be
  #    - uncorrelated random model : hyp = "uncorrelated"
  #    - multinomial : hyp = "fixed"
  # if uncorrelated : constrained model is the multinomial logit model
  if (mixed.logit){
    if (is.null(object$call$correlation)) correlation <- FALSE
    else correlation <- object$call$correlation
    if (correlation){
      if (is.null(hyp)) hyp <- "fixed"
      if (!hyp %in% c("fixed", "uncorrelated")){
        stop("hyp should be one of fixed or uncorrelated")
      }
    }
    else{
      if (!is.null(hyp)) warning("hyp is irrelevant in this setting and will be ignored")
    }
    J <- length(object$rpar)
    if (correlation){
      rd.el <- K+(1:(J*(J+1)/2))
      diag.el <- K + c(1, cumsum(J:2)+1)
      if (hyp == "uncorrelated"){
        if (!correlation) stop("no correlation")
        rd.el <- K+(1:(J*(J+1)/2))
        su <- rd.el[!(rd.el %in% diag.el)]
      }
      else su <- rd.el
    }
    else su <- K + (1:J)
    q <- rep(0, length(su))
  }
  if (!specific.computation){
    if (is.null(q)) wq <- coef(object)[su] else wq <- coef(object)[su] - q
    stat <- as.numeric(crossprod(wq,
                                 crossprod(solve(vcov(object)[su, su]),
                                           wq)))
    df <- c(df = length(su))
  }
  names(stat) <- 'chisq'
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

scoretest.mlogit <- function(object, ...,
                             heterosc = FALSE, nests = NULL, rpar = NULL,
                             un.nest.el = FALSE, halton = NULL, panel = FALSE,
                             correlation = FALSE, R = 40){
  objects <- list(object, ...)
  nmodels <- length(objects)
  start.values <- c(coef(object))

  # if several models are provided, just use the default method
  if (nmodels > 1){
    return(scoretest.default(object, ...))
  }
  heterosc.logit <- heterosc
  nested.logit <- (!is.null(nests) || !is.null(object$nests))
  mixed.logit <- (!is.null(rpar) || correlation)
  if (heterosc.logit + nested.logit + mixed.logit == 0)
    stop("an unconstrained model should be described")
  if (heterosc.logit + nested.logit + mixed.logit > 1)
    stop("only one unconstrained model should be described")
  if (heterosc.logit){
    alt.hyp <- "heteroscedastic model"
    data.name <- "heterosc = TRUE"
  }
  if (nested.logit){
    init.nested.model <- !is.null(object$call$nests)
    if (init.nested.model){
      if (is.null(object$call$un.nest.el) || !object$call$un.nest.el){
        stop("irrelevant model for a score test")
      }
      J <- length(object$nests)
      start.values <- c(coef(object), rep(coef(object)[length(coef(object))], J - 1))
      data.name <- "un.nest.el = FALSE"
      alt.hyp <- "unique nest elasticity"
    }
    else{
      alt.hyp <- ifelse(un.nest.el, "nested model with a unique nest elasticity",
                        "nested model")
      nest.list <- c()
      for (i in 1:length(nests)){
        anest <- paste("c(\'",paste(nests[[i]],collapse="\',\'"),"\')", sep="")
        anest <- paste(names(nests)[i], " = ", anest, sep = "")
        nest.list <- c(nest.list, anest)
      }
      data.name = paste("nests = list(", paste(nest.list, collapse = ", "), ")", sep = "")
    }
  }
    
  if (mixed.logit){
    init.mixed.model <- !is.null(object$call$rpar)
    if (init.mixed.model){
      if (!is.null(object$call$correlation) && object$call$correlation) stop("not a relevant model for a score test")
      alt.hyp <- "uncorrelated random effects"
    }
    else{
      if (correlation) alt.hyp <- "no correlated random effects"
      else alt.hyp <- "no uncorrelated random effects"
    }
    data.name <- paste(names(rpar), paste("\'",as.character(rpar),"\'", sep = ""),
      collapse = ",", sep = "=")
    data.name <- paste("rpar", "(", data.name, ")", sep = "")
    if (init.mixed.model){
      J <- length(object$rpar)
      K <- ncol(model.matrix(object))
      sd <- coef(object)[-c(1:K)]
      rd.el <- K+(1:(J*(J+1)/2))
      diag.el <- K + c(1, cumsum(J:2)+1)
      start.values <- c(start.values[1:K], rep(0, length(rd.el)))
      start.values[diag.el] <- sd
    }
  }
  
  mc <- match.call()
  mc[[1]] <- as.name('update')
  mc[c('iterlim', 'method', 'start', 'print.level')] <- list(0, 'bfgs', start.values, 0)
  newmodel <- eval(mc, parent.frame())
  stat <- - sum(newmodel$gradient * solve(newmodel$hessian, newmodel$gradient))
  names(stat) <- "chisq"
  df <- c(df = length(coef(newmodel)) - length(coef(object)))
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
  if (!inherits(new, 'formula') & !inherits(new, cls))
    stop("the updating argument doesn't have a correct class")
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
    
