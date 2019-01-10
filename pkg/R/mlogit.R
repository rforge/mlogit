#' Multinomial logit model
#' 
#' Estimation by maximum likelihood of the multinomial logit model,
#' with alternative-specific and/or individual specific variables.
#' 
#' @name mlogit
#' @aliases mlogit print.mlogit suml fitted.mlogit predict.mlogit
#'     residuals.mlogit df.residual.mlogit terms.mlogit
#'     model.matrix.mlogit coef.mlogit update.mlogit summary.mlogit
#'     coef.summary.mlogit print.summary.mlogit logLik.mlogit
#' @importFrom stats as.formula coef dlnorm dnorm formula logLik
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom stats na.omit pchisq plnorm pnorm predict
#' @importFrom stats printCoefmat punif qlnorm qnorm qunif
#' @importFrom stats relevel reshape residuals rnorm runif
#' @importFrom stats terms update vcov dunif effects
#' @importFrom statmod gauss.quad
#' @importFrom MASS ginv
#' @importFrom zoo index
#' @param x,object an object of class \code{mlogit}
#' @param formula a symbolic description of the model to be estimated,
#' @param new an updated formula for the \code{update} method,
#' @param newdata a \code{data.frame} for the \code{predict} method,
#' @param returnData if \code{TRUE}, the data is returned as an
#'     attribute,
#' @param data the data: an \code{mlogit.data} object or an ordinary
#'     \code{data.frame},
#' @param subset an optional vector specifying a subset of
#'     observations for \code{mlogit}, an optional vector of
#'     coefficients to extract for the \code{coef} method,
#' @param weights an optional vector of weights,
#' @param na.action a function which indicates what should happen when
#'     the data contains '\code{NA}'s,
#' @param start a vector of starting values,
#' @param alt.subset a vector of character strings containing the
#'     subset of alternative on which the model should be estimated,
#' @param reflevel the base alternative (the one for which the
#'     coefficients of individual-specific variables are normalized to
#'     0),
#' @param nests a named list of characters vectors, each names being a
#'     nest, the corresponding vector being the set of alternatives
#'     that belong to this nest,
#' @param un.nest.el a boolean, if \code{TRUE}, the hypothesis of
#'     unique elasticity is imposed for nested logit models,
#' @param unscaled a boolean, if \code{TRUE}, the unscaled version of
#'     the nested logit model is estimated,
#' @param heterosc a boolean, if \code{TRUE}, the heteroscedastic
#'     logit model is estimated,
#' @param rpar a named vector whose names are the random parameters
#'     and values the distribution : \code{'n'} for normal, \code{'l'}
#'     for log-normal, \code{'t'} for truncated normal, \code{'u' }
#'     for uniform,
#' @param probit if \code{TRUE}, a multinomial porbit model is
#'     estimated,
#' @param R the number of function evaluation for the gaussian
#'     quadrature method used if \code{heterosc=TRUE}, the number of
#'     draws of pseudo-random numbers if \code{rpar} is not
#'     \code{NULL},
#' @param correlation only relevant if \code{rpar} is not \code{NULL},
#'     if true, the correlation between random parameters is taken
#'     into account,
#' @param halton only relevant if \code{rpar} is not \code{NULL}, if
#'     not \code{NULL}, halton sequence is used instead of
#'     pseudo-random numbers. If \code{halton=NA}, some default values
#'     are used for the prime of the sequence (actually, the primes
#'     are used in order) and for the number of elements
#'     droped. Otherwise, \code{halton} should be a list with elements
#'     \code{prime} (the primes used) and \code{drop} (the number of
#'     elements droped).
#' @param random.nb only relevant if \code{rpar} is not \code{NULL}, a
#'     user-supplied matrix of random,
#' @param panel only relevant if \code{rpar} is not \code{NULL} and if
#'     the data are repeated observations of the same unit ; if
#'     \code{TRUE}, the mixed-logit model is estimated using panel
#'     techniques,
#' @param estimate a boolean indicating whether the model should be
#'     estimated or not: if not, the \code{model.frame} is returned,
#' @param seed ,
#' @param digits the number of digits,
#' @param width the width of the printing,
#' @param outcome a boolean which indicates, for the \code{fitted} and
#'     the \code{residuals} methods whether a matrix (for each choice,
#'     one value for each alternative) or a vector (for each choice,
#'     only a value for the alternative chosen) should be returned,
#' @param type one of \code{outcome} (probability of the chosen
#'     alternative), \code{probabilities} (probabilities for all the
#'     alternatives), \code{parameters} for individual-level random
#'     parameters for the fitted method, how the correlated random
#'     parameters should be displayed : \code{"chol"} for the
#'     estimated parameters (the elements of the Cholesky
#'     decomposition matrix), \code{"cov"} for the covariance matrix
#'     and \code{"cor"} for the correlation matrix and the standard
#'     deviations,
#' @param fixed if \code{FALSE} (the default), constant coefficients
#'     are not returned,
#' @param ... further arguments passed to \code{mlogit.data} or
#'     \code{mlogit.optim}.
#' 
#' @details For how to use the formula argument, see
#'     \code{\link{mFormula}}.
#' 
#' The \code{data} argument may be an ordinary \code{data.frame}. In
#' this case, some supplementary arguments should be provided and are
#' passed to \code{\link{mlogit.data}}. Note that it is not necessary
#' to indicate the choice argument as it is deduced from the formula.
#' 
#' The model is estimated using the \code{\link{mlogit.optim}}
#' function.
#' 
#' The basic multinomial logit model and three important extentions of
#' this model may be estimated.
#' 
#' If \code{heterosc=TRUE}, the heteroscedastic logit model is estimated.
#' \code{J-1} extra coefficients are estimated that represent the scale
#' parameter for \code{J-1} alternatives, the scale parameter for the reference
#' alternative being normalized to 1. The probabilities don't have a closed
#' form, they are estimated using a gaussian quadrature method.
#' 
#' If \code{nests} is not \code{NULL}, the nested logit model is estimated.
#' 
#' If \code{rpar} is not \code{NULL}, the random parameter model is
#' estimated.  The probabilities are approximated using simulations
#' with \code{R} draws and halton sequences are used if \code{halton}
#' is not \code{NULL}. Pseudo-random numbers are drawns from a
#' standard normal and the relevant transformations are performed to
#' obtain numbers drawns from a normal, log-normal, censored-normal or
#' uniform distribution. If \code{correlation=TRUE}, the correlation
#' between the random parameters are taken into account by estimating
#' the components of the cholesky decomposition of the covariance
#' matrix. With G random parameters, without correlation G standard
#' deviations are estimated, with correlation G * (G + 1) /2
#' coefficients are estimated.

#' @return An object of class \code{"mlogit"}, a list with elements: \itemize{
#' \item{coefficients}{the named vector of coefficients,}
#' \item{logLik}{the value of the log-likelihood,}
#' \item{hessian}{the hessian of the log-likelihood at convergence,}
#' \item{gradient}{the gradient of the log-likelihood at convergence,}
#' \item{call}{the matched call,}
#' \item{est.stat}{some information about the estimation (time used,
#' optimisation method),}
#' \item{freq}{the frequency of choice,}
#' \item{residuals}{the residuals,}
#' \item{fitted.values}{the fitted values,}
#' \item{formula}{the formula (a \code{mFormula} object),}
#' \item{expanded.formula}{the formula (a \code{formula} object),}
#' \item{model}{the model frame used,}
#' \item{index}{the index of the choice and of the alternatives.}
#' }
#' @export
#' @author Yves Croissant
#' @seealso
#' \code{\link{mlogit.data}} to shape the data. \code{\link{multinom}} from
#' package \code{nnet} performs the estimation of the multinomial logit model
#' with individual specific variables. \code{\link{mlogit.optim}} for details
#' about the optimization function.
#' @references
#' McFadden, D. (1973) Conditional Logit Analysis of Qualitative Choice
#' Behavior, in P. Zarembka ed., \emph{Frontiers in Econometrics}, New-York:
#' Academic Press.
#' 
#' McFadden, D. (1974) ``The Measurement of Urban Travel Demand'',
#' \emph{Journal of Public Economics}, 3, pp. 303-328.
#' 
#' Train, K. (2004) \emph{Discrete Choice Modelling, with Simulations},
#' Cambridge University Press.
#' @keywords regression
#' @examples
#' ## Cameron and Trivedi's Microeconometrics p.493 There are two
#' ## alternative specific variables : price and catch one individual
#' ## specific variable (income) and four fishing mode : beach, pier, boat,
#' ## charter
#' 
#' data("Fishing", package = "mlogit")
#' Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
#' 
#' ## a pure "conditional" model
#' summary(mlogit(mode ~ price + catch, data = Fish))
#' 
#' ## a pure "multinomial model"
#' summary(mlogit(mode ~ 0 | income, data = Fish))
#' 
#' ## which can also be estimated using multinom (package nnet)
#' library("nnet")
#' summary(multinom(mode ~ income, data = Fishing))
#' 
#' ## a "mixed" model
#' m <- mlogit(mode ~ price+ catch | income, data = Fish)
#' summary(m)
#' 
#' ## same model with charter as the reference level
#' m <- mlogit(mode ~ price+ catch | income, data = Fish, reflevel = "charter")
#' 
#' ## same model with a subset of alternatives : charter, pier, beach
#' m <- mlogit(mode ~ price+ catch | income, data = Fish,
#'             alt.subset = c("charter", "pier", "beach"))
#' 
#' ## model on unbalanced data i.e. for some observations, some
#' ## alternatives are missing
#' # a data.frame in wide format with two missing prices
#' Fishing2 <- Fishing
#' Fishing2[1, "price.pier"] <- Fishing2[3, "price.beach"] <- NA
#' mlogit(mode~price+catch|income, Fishing2, shape="wide", choice="mode", varying = 2:9)
#' 
#' # a data.frame in long format with three missing lines
#' data("TravelMode", package = "AER")
#' Tr2 <- TravelMode[-c(2, 7, 9),]
#' mlogit(choice~wait+gcost|income+size, Tr2, shape = "long",
#'        chid.var = "individual", alt.var="mode", choice = "choice")
#' 
#' ## An heteroscedastic logit model
#' data("TravelMode", package = "AER")
#' hl <- mlogit(choice ~ wait + travel + vcost, TravelMode,
#'              shape = "long", chid.var = "individual", alt.var = "mode",
#'              method = "bfgs", heterosc = TRUE, tol = 10)
#' 
#' ## A nested logit model
#' TravelMode$avincome <- with(TravelMode, income * (mode == "air"))
#' TravelMode$time <- with(TravelMode, travel + wait)/60
#' TravelMode$timeair <- with(TravelMode, time * I(mode == "air"))
#' TravelMode$income <- with(TravelMode, income / 10)
#' # Hensher and Greene (2002), table 1 p.8-9 model 5
#' TravelMode$incomeother <- with(TravelMode, ifelse(mode %in% c('air', 'car'), income, 0))
#' nl <- mlogit(choice~gcost+wait+incomeother, TravelMode,
#'              shape='long', alt.var='mode',
#'              nests=list(public=c('train', 'bus'), other=c('car','air')))
#' # same with a comon nest elasticity (model 1)
#' nl2 <- update(nl, un.nest.el = TRUE)
#' 
#' ## a probit model
#' \dontrun{
#' pr <- mlogit(choice ~ wait + travel + vcost, TravelMode,
#'              shape = "long", chid.var = "individual", alt.var = "mode",
#'              probit = TRUE)
#' }
#' 
#' ## a mixed logit model
#' \dontrun{
#' rpl <- mlogit(mode ~ price+ catch | income, Fishing, varying = 2:9,
#'               shape = 'wide', rpar = c(price= 'n', catch = 'n'),
#'               correlation = TRUE, halton = NA,
#'               R = 10, tol = 10, print.level = 0)
#' summary(rpl)
#' rpar(rpl)
#' cor.mlogit(rpl)
#' cov.mlogit(rpl)
#' rpar(rpl, "catch")
#' summary(rpar(rpl, "catch"))
#' }
#' 
#' # a ranked ordered model
#' data("Game", package = "mlogit")
#' g <- mlogit(ch~own|hours, Game, choice='ch', varying = 1:12,
#'             ranked=TRUE, shape="wide", reflevel="PC")
mlogit <- function(formula, data, subset, weights, na.action, start= NULL,
                   alt.subset = NULL, reflevel= NULL,
                   nests = NULL, un.nest.el = FALSE, unscaled = FALSE,
                   heterosc = FALSE, rpar = NULL, probit = FALSE,
                   R = 40, correlation = FALSE, halton = NULL, random.nb = NULL,
                   panel = FALSE, estimate = TRUE, seed = 10, ...){
    start.time <- proc.time()
    callT <- match.call(expand.dots = TRUE)
#    callF <- match.call(expand.dots = FALSE)
#    formula <- callF$formula <- mFormula(formula)
    formula <- callT$formula <- mFormula(formula)
    wlogit <- length(formula)[2] == 4
    nframe <- length(sys.calls())
    heterosc.logit <- heterosc

    nested.logit <- ! is.null(nests)
    pair.comb.logit <- FALSE
    if (! is.null(nests) && length(nests) == 1){
        if (is.character(nests) && nests == "pcl"){
            nested.logit <- TRUE
            pair.comb.logit <- TRUE
        }
        if (is.logical(nests)){
            nested.logit <- nests
            pair.comb.logit <- FALSE
        }
    }
    if (nested.logit) attr(nests, "unique") <- ifelse(un.nest.el, TRUE, FALSE)

    mixed.logit <- ! is.null(rpar)

    multinom.logit <- ! heterosc & ! nested.logit & is.null(rpar) & ! probit & ! wlogit
    if (heterosc.logit + nested.logit + mixed.logit + probit > 1)
        stop("only one of heterosc, rpar, nests and probit can be used")
    if (multinom.logit){
        if (is.null(callT$method)) callT$method <- 'nr'
        if (is.null(callT$print.level)) callT$print.level <- 0
    }
    
    # 1 ############################################################
    #  check whether arguments for mlogit.data are present: if so run
    #  mlogit.data
    ################################################################

    mldata <- callT
    response.name <- paste(deparse(attr(formula, "lhs")[[1]]))
    m <- match(c("data", "choice", "shape", "varying", "sep",
                 "alt.var", "chid.var", "alt.levels", "group.var",
                 "opposite", "drop.index", "id.var", "ranked"),
               names(mldata), 0L)
    use.mlogit.data <- sum(m[- 1]) > 0
    if (use.mlogit.data){
        mldata <- mldata[c(1L, m)]
        mldata[[1L]] <- as.name("mlogit.data")
        mldata$choice <- response.name
   #    data <- eval(mldata, sys.frame(which = nframe))
        data <- eval(mldata, parent.frame())
    }

    # 2 ###########################################################
    # model.frame (with the data provided or the one computed by
    # mlogit.data
    ###############################################################

    mf <- callT
    m <- match(c("formula", "data", "subset", "na.action", "weights"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    #  mf$drop.unused.levels <- TRUE
    mf$formula <- formula
    mf[[1L]] <- as.name("model.frame")
    mf$data <- data # fix the bug when the data is called mldata
    #  mf <- eval(mf, sys.frame(which = nframe))
    mf <- eval(mf, parent.frame())
    # ensures that the response is logical
    mf[[1]] <- tological(mf[[1]])
        
    # change the reference level of the response if required
    if (! is.null(reflevel)) attr(mf, "index")[["alt"]] <- relevel(attr(mf, "index")[["alt"]], reflevel)
    index <- attr(mf, "index")
    alt <- index[["alt"]]
    chid <- index[["chid"]]

    if (panel){
        if (! mixed.logit) stop("panel is only relevant for mixed logit models")
        id <- index[["id"]]
        if (is.null(id)) stop("no individual index")
        # bug, only works when all the alternatives are available for all the choice situations
        #        id <- split(index[["id"]], alt)[[1]]
        id <- unique(index[, c("chid", "id")])$id
    }
    else id <- NULL
    
    # compute the relevent subset if required
    if (! is.null(alt.subset)){
        # we keep only choices that belong to the subset
        choice <- alt[model.response(mf)]
        choice <- choice %in% alt.subset
        unid <- unique(chid)
        names(choice) <- as.character(unid)
        id.kept <- choice[as.character(chid)]
        # we keep only the relevant alternatives
        alt.kept <- alt %in% alt.subset
        # the relevant subset for the data.frame and the indexes
        mf <- mf[id.kept & alt.kept, , drop = FALSE]
        alt <- alt[id.kept & alt.kept , drop = TRUE]
        chid <- chid[id.kept & alt.kept , drop = TRUE]
    }

    if (nested.logit){
        if (is.logical(nests) && nests){
            if (is.null(index(mf)$group)){
                stop("no grouping variable")
            }
            else{
                nests <- unique(data.frame(alt = as.character(alt),
                                           group = index(mf)$group))
                nests <- split(nests$alt, nests$group)
            }
        }
    }
    
    # balanced the data.frame i.e. insert rows with NA when an
    # alternative is not relevant
    alt.un <- unique(alt)
    chid.un <- unique(chid)
    alt.lev <- levels(alt)
    J <- length(alt.lev)
    n <- length(chid.un)
    T <- length(alt.un)
    balanced <- TRUE
    if (nrow(mf) != (n * T)){
        omf <- mf
        # check the relevance of the following line
        rownames(mf) <- paste(chid, alt, sep = ".")
        all.rn <- as.character(t(outer(chid.un, alt.un, paste, sep = ".")))
        mf <- mf[all.rn, ]
        rownames(mf) <- all.rn
        chid <- rep(chid.un, each = T)
        alt <- rep(alt.un, n)
        index <- data.frame(chid = chid, alt = alt, row.names = rownames(mf))
        if (! is.null(index(mf)$group)){
            ra <- index(mf)[c("alt", "group")][! duplicated(index(mf)$alt), ]
            gps <- ra$group
            names(gps) <- ra$alt
            index$group <- gps[index$alt]
        }
        
        balanced <- FALSE
    }
    else omf <- mf
    
    attr(mf, "formula") <- formula
    # if estimate is FALSE, return the data.frame
    if (! estimate) return(mf)

    # 3 ###########################################################
    # extract the elements of the model
    ###############################################################

    y <- model.response(mf)
    choice <- na.omit(alt[y])
    # extract the X matrix for the standard deviation estimation
    if (length(formula)[2] == 4){
        sformula <- formula(as.Formula(formula), rhs = 4)
        Xs <- model.matrix(sformula, omf)[! duplicated(index(omf)$chid), ]
        formula <- mFormula(formula(as.Formula(formula), rhs = 1:3))
    }
    else Xs <- NULL
    X <- model.matrix(formula, mf)
    K <- ncol(X)
    df.residual <- n - K
    colnamesX <- colnames(X)
    if (any(names(mf) == "(weights)")){
        weights <- matrix(mf[["(weights)"]], ncol = J, byrow = TRUE)
        weights <- as.numeric(apply(weights, 1, min, na.rm = TRUE))
        weights <- weights / mean(weights)
    }
    else weights <- NULL
    freq <- table(alt[y])
    # Xl and yl are lists of length J which contains n matrix / vector
    # of covariates and response (a boolean) ; yv is a vector that
    # contains the chosen alternative
    Xl <- vector(length = J, mode = "list")
    names(Xl) <- levels(alt)
    for (i in levels(alt))  Xl[[i]] <- X[alt == i, , drop = FALSE]
    yl <- split(y, alt)
    yl <- lapply(yl, function(x){x[is.na(x)] <- FALSE ; x})
    attr(yl, "chid") <- as.character(levels(chid))
    attr(yl, "id") <- as.character(levels(id))
  
    if (probit){
        # for probit the response is a vector that contains the chosen
        # alternative as a numeric ; NA values of y are replaced by FALSE
        # so that the chosen alternative is correctly returned
        y2 <- y
        y2[is.na(y2)] <- FALSE
        yv <- as.numeric(alt[y2])
        # DX is a list of covariates first differenced with respect with the
        # chosen alternative
        DX <- vector("list", length = J - 1)
        DX <- lapply(DX, function(x) return(matrix(NA, nrow = n, ncol = K)))
    
        for (i in 1:n){
            any <- yv[i]
            j <- 1
            newj <- 1
            for (j in 1:J){
                if (j != any){
                    DX[[newj]][i,] <- Xl[[j]][i, ] - Xl[[any]][i,]
                    newj <- newj + 1
                }
            }
        }
    }

    # 4 ###########################################################
    # Compute the starting values
    ###############################################################

    # first give names to the supplementary coefficients and values if
    # start is null

    sup.coef <- numeric(0)
    names.sup.coef <- character(0)
    if (nested.logit){
        L <- length(nests)
        if (un.nest.el){
            if (is.null(start) || length(start) == K) sup.coef <- c(iv = 1)
            names.sup.coef <- 'iv'
        }
        else{
            if (is.null(start) || length(start) == K) sup.coef <- rep(1, L)
            names.sup.coef <- paste("iv", names(nests), sep = ":")
        }
    }
    if (pair.comb.logit){
        if (un.nest.el){
            if (is.null(start)) sup.coef <- c(iv = 1)
            names.sup.coef <- 'iv'
        }
        else{
            names.sup.coef <- NULL
            for (i in 1:(J-1)){
                names.sup.coef <- c(names.sup.coef, paste('iv', alt.lev[i], alt.lev[(i+1):J], sep = "."))
            }
            sup.coef <- rep(1, length(names.sup.coef))
            # in case we have to suppress one nest
            ## sup.coef <- sup.coef[-1]
            ## names.sup.coef <- names.sup.coef[-1]
        }
    }
    if (heterosc.logit){
        if (is.null(start) || length(start) == K) sup.coef <- rep(1, J - 1)
        names.sup.coef <- paste("sp", alt.lev[-1], sep = ".")
    }
    if (mixed.logit){
        unknowndist <- rpar[! (rpar %in% c("cn", "ln", "tn", "n", "u", "t", "zbu", "zbt"))]
        if (length(unknowndist) > 0){
            udstr <- paste("unknown distribution", paste(unique(unknowndist), collapse = ", "))
            stop(udstr)
        }        
        names.rpar <- names(rpar)
        names.rpar.sig <- names.rpar[! rpar %in% c("zbu", "zbt")]
        names.fixed <- colnamesX[! colnamesX %in% names.rpar]
        # the names of the correlated and uncorrelated random
        # parameters in the order of the X matrix

        if (is.logical(correlation)){
            if (correlation) correlation <- names(rpar) else correlation <- character(0)
        }
        if (any (! names(rpar) %in% colnamesX)) stop("unknown random parameter")
        if (any (! correlation %in% names(rpar))) stop("unknown random parameter in the correlation vector")


        uncorrelated <- setdiff(names(rpar), correlation)
        fixedpar <- setdiff(colnamesX, names(rpar))
        singlepar <- names(rpar)[rpar %in% c("zbu", "zbt")]
        utwopars <- intersect(names(rpar)[! rpar %in% c("zbu", "zbt")], uncorrelated)       
        correlated <-   colnamesX[sort(match(correlation,  colnamesX))]
        uncorrelated <- colnamesX[sort(match(uncorrelated, colnamesX))]
        fixedpar <- colnamesX[sort(match(fixedpar, colnamesX))]
        randompar <- colnamesX[sort(match(names(rpar), colnamesX))]
        singlepar <- colnamesX[sort(match(singlepar, colnamesX))]
        utwopars <- colnamesX[sort(match(utwopars, colnamesX))]

        Kc <- length(correlated)
        Ku <- length(uncorrelated)
        Ko <- length(singlepar)
        
        names.sup.coef <- c()
        sup.coef <- c()
        if (Ku - Ko){
            if (is.null(start) || length(start) == K) sup.coef <- c(sup.coef, rep(0.1, Ku - Ko))
            names.sup.coef <- paste("sd", utwopars, sep = ".")
        }
        if (Kc){
            if (is.null(start) || length(start) == K){
                sup.coef <- c(sup.coef, rep(0.1, 0.5 * Kc * (Kc + 1)))
            }
            names.sup.coef <- c(names.sup.coef,
                                names.rpar(correlated, prefix = "chol"))
        }
        if (is.null(start) || length(start) == K) names(sup.coef) <- names.sup.coef
    }
    if (probit){
        names.sup.coef <- c()
        for (i in 2:J){
            names.sup.coef <- c(names.sup.coef,
                                paste(alt.lev[i], alt.lev[i:J], sep = "."))
        }

        if (is.null(start) || length(start) == K){
            corrMat <- matrix(0.5, J - 1, J - 1)
            diag(corrMat) <- 1
            corrMat <- chol(corrMat)
            sup.coef <- (t(corrMat)[! upper.tri(corrMat)])
            names(sup.coef) <- names.sup.coef
        }
    }

    if (wlogit){
        Xs <- Xs[, -1, drop = FALSE]
        if (is.null(start) || length(start) == K) sup.coef <- c(sup.coef, rep(0.01, ncol(Xs)))
        names.sup.coef <- c(names.sup.coef, paste("sig", colnames(Xs), sep = "."))
    }
    
    ## start can be :
    ##   1. NULL, in this case estimate the multinomial logit model,
    ##   2. a vector of length K ; then add starting values for the
    ##   supplementary coefficients,
    ##   3. a full set ; then just name the coefs.

    if (is.null(start)){
        callst <- callT
        start <- rep(0, K)
        names(start) <- colnamesX
        callst$start <- start
        callst$print.level <- 0
        if (! multinom.logit){
            callst$nests <- callst$rpar <- callst$constPar <- callst$iterlim <- NULL
            callst$heterosc <- callst$panel <- callst$probit <- FALSE
            callst$print.level <- 0
            if (length(callst$formula)[2] == 4) callst$formula <- mFormula(formula(callst$formula, rhs = 1:3))
            #      start <- coef(eval(callst, sys.frame(which=nframe)))
            #      start <- coef(eval(callst, parent.frame()))
            #      start <- coef(eval(callst, parent.frame))
            #      retour sur nframe ????
            start <- coef(eval(callst, sys.frame(which=nframe)))
            
            if (mixed.logit){
                ln <- names(rpar[rpar == "ln"])
                start[ln] <- log(start[ln])
            }
        }
    }
    if (length(start) == K){
        names(start) <- colnamesX
        if (! multinom.logit){
            names(sup.coef) <- names.sup.coef
            if (probit) start <- start * sqrt(3) / pi
            start <- c(start, sup.coef)
        }
    }
    else{
        if (! multinom.logit) names(start) <- c(colnamesX, names.sup.coef)
        else names(start) <- colnamesX
    }

    # if constPar is numeric, insert the relevant value in the start
    # vector and transform the constPar vector to character

    # 5 ###################################################################
    # Estimate the model using mlogit.nlm and passing the correct arguments
    #######################################################################

    opt <- callT
    if (! is.null(opt$constPar)){
        theconstPar <- eval(opt$constPar)
        if (is.numeric(theconstPar)){
            if (is.null(names(theconstPar))) stop('the numeric constPar vector should be named')
            start[names(theconstPar)] <- theconstPar
            opt$constPar <- names(theconstPar)
        }
    }
    if (probit) if (is.null(opt$constPar)) opt$constPar <- names.sup.coef[1]
    opt$start <- start
    m <- match(c("method", "print.level", "iterlim",
                 "start", "constPar","tol", "ftol", "steptol"),
               names(opt), 0L)
    opt <- opt[c(1L, m)]
    opt[[1]] <- as.name('mlogit.optim')
    opt$logLik <- as.name('lnl.slogit')
    opposite <- TRUE
    if (is.null(weights)) weights <- 1
    opposite <- ifelse(opposite, -1, +1)
    opt[c('weights', 'opposite')] <- list(as.name('weights'), as.name('opposite'))
    if (! probit) opt[c('X', 'y')] <- list(as.name('Xl'), as.name('yl'))
    else opt[c('X', 'y')] <- list(as.name('DX'), as.name('yv'))
    if (wlogit) opt[c('logLik', 'Xs')] <- list(as.name('lnl.wlogit'), as.name('Xs'))

    if (mixed.logit){
        opt$logLik <- as.name('lnl.rlogit')
        opt[c('R', 'seed', 'id', 'rpar', 'correlation', 'halton', 'Xs')] <-
            list(as.name('R'), as.name('seed'), as.name('id'), as.name('rpar'),
                 as.name('correlation'), as.name('halton'), as.name('Xs'))
    }
    if (probit){
        opt$logLik <- as.name('lnl.mprobit')
        opt[c('R', 'seed')] <-
            list(as.name('R'), as.name('seed'))
    }
    if (heterosc.logit){
        opt$logLik <- as.name('lnl.hlogit')
        rn <- gauss.quad(R, kind = "laguerre")
        opt[c('rn')] <- list(as.name('rn'))
    }
    if (nested.logit){
        opt$logLik <- as.name('lnl.nlogit')
        opt$nests <- as.name('nests')
        opt$un.nest.el <- as.name('un.nest.el')
        opt$unscaled <- as.name('unscaled')
    }
    if (pair.comb.logit){
        opt$logLik <- as.name('lnl.nlogit')
        alt1 <- rep(alt.lev, c((J-1):0))
        alt2 <- alt.lev[unlist(lapply(2:J, function(x) x:J))]
        names.nests <- paste(alt1, alt2, sep = ".")
        nests <- mapply(function(x,y) c(x,y), alt1, alt2, SIMPLIFY = FALSE)
        names(nests) <- names.nests
        # in case we have to suppress one nest
        # lnests <- lnests[-1]
        opt$nests <- nests
        opt$unscaled <- as.name('unscaled')
        opt$un.nest.el <- as.name('un.nest.el')
    }
    ## doesn't work with parent.frame
    x <- eval(opt, sys.frame(which = nframe))
    
    # 6 ###########################################################
    # put the result in form
    ###############################################################

    # some general features
    n <- sum(freq)
    x$est.stat$elaps.time <- proc.time() - start.time
    logLik <- structure( - as.numeric(x$optimum),
                        df = length(x$coefficients),
                        null = sum(freq * log(freq / n)),
                        class = "logLik"
                        )

    if (mixed.logit) rpar <- make.rpar(rpar, correlation, x$coefficients, NULL) else rpar <- NULL
    if (! (nested.logit | pair.comb.logit)) nests <- NULL
  
    # if no hessian is returned, use the BHHH approximation
    if (is.null(attr(x$optimum, 'hessian'))) hessian <- - crossprod(attr(x$optimum, 'gradi'))
    else hessian <- - attr(x$optimum, 'hessian')
    fitted <- attr(x$optimum, "fitted")
    probabilities <- attr(x$optimum, "probabilities")
    linpred <- attr(x$optimum, "linpred")
    resid <- Reduce("cbind", yl) - fitted
    attr(x$coefficients, "fixed") <- attr(x$optimum, "fixed")
    attr(x$coefficients, "sup") <- names.sup.coef
    gradient <- -  attr(x$optimum, "gradient")
    gradient <- - attr(x$optimum, "gradi")
    if (mixed.logit) indpar <- attr(x$optimum, "indpar") else indpar <- NULL
    # compute the probabilities for all the alternatives for
    # heteroscedastic and the probit model
    if (probit | heterosc){
        opt$logLik <- opt$iterlim <- opt$method <- opt$print.level <- opt$tol <- opt$constPar <- NULL
        names(opt)[[2]] <- 'param'
        opt[[2]] <- x$coefficients
        opt$gradient <- FALSE
        opt[[1]] <- as.name('lnl.hlogit')
        if (probit) opt[[1]] <- as.name('lnl.mprobit') else opt[[1]] <- as.name('lnl.hlogit')
        probabilities <- c()
        for (k in 1:J){
            if (probit){
                they <- rep(k, n)
                opt$y <- they
                DX <- vector("list", length = J - 1)
                DX <- lapply(DX, function(x) return(matrix(NA, nrow = n, ncol = K)))
                for (i in 1:n){
                    any <- k
                    j <- 1
                    newj <- 1
                    for (j in 1:J){
                        if (j != any){
                            DX[[newj]][i, ] <- Xl[[j]][i, ] - Xl[[any]][i, ]
                            newj <- newj + 1
                        }
                    }
                }
                opt$X <- DX
            }
            if (heterosc){
                they <- vector(mode='list', length= J)
                they <- lapply(they, function(x) rep(FALSE, n))
                they[[k]] <- rep(TRUE, n)
                names(they) <- names(yl)
                opt$y <- they
            }
            # doesn't work with parent.frame
            probabilities <- cbind(probabilities,
                                   attr(eval(opt, sys.frame(which = nframe)), 'fitted'))
        }
        colnames(probabilities) <- alt.lev
        attr(x$optimum, "probabilities") <- probabilities
    }

    # Compute the covariance matrix of the errors
    if (multinom.logit | mixed.logit){
        alt.names <- colnames(probabilities)
        J <- length(alt.names)
        Omega <- matrix(0, J, J, dimnames = list(alt.names, alt.names))
        diag(Omega) <- pi ^ 2 / 6
    }
    if (probit){
        S <- matrix(0, J - 1, J - 1)
        S[!upper.tri(S)] <- x$coefficients[-c(1:K)]
        Mi <- list()
        M <- rbind(0, diag(J - 2))
        Mi[[1]] <- diag(J - 1)
        for (i in 2:(J - 1)){
            Mi[[i]] <- cbind(M[, 0:(i - 2), drop = FALSE], -1, M[, ((i - 1):(J - 2))])
        }
        Mi[[J]] <- cbind(M, -1)
        names(Mi) <- alt.lev
        Omega <- lapply(Mi, function(x) tcrossprod(x %*% S))
        for (i in 1:J){
            colnames(Omega[[i]]) <- rownames(Omega[[i]]) <- alt.lev[-i]
        }
    }
    if (nested.logit | pair.comb.logit){
        alt.names <- colnames(probabilities)
        J <- length(alt.names)
        Omega <- matrix(0, J, J, dimnames = list(alt.names, alt.names))
        for (i in 1:length(nests)){
            anEl <- x$coefficients[paste('iv', names(nests)[i], sep = ".")]
            alts <- nests[[i]]
            M <- length(alts)
            for (m in 1:M){
                for (n in 1:M){
                    Omega[alts[m], alts[n]] <- (1 - anEl^2)
                }
            }
        }
        diag(Omega) <- 1
        Omega <- Omega * tcrossprod(rep(pi/sqrt(6), J))
    }
    if (heterosc.logit){
        alt.names <- colnames(probabilities)
        J <- length(alt.names)
        Omega <- matrix(0, J, J, dimnames = list(alt.names, alt.names))
        diag(Omega) <- pi^2 / 6
        for (i in 2:J){
            analt <- alt.names[i]
            Omega[analt, analt] <- pi ^ 2 / 6 * x$coefficients[paste('sp', analt, sep = '.')]^2
        }
    }
    if (wlogit) Omega <- "plus tard"

    mfindex <- index(mf)
    mf$probabilities <- as.numeric(t(probabilities))
    if (! is.null(linpred)) mf$linpred <- as.numeric(t(linpred))
    mf <- structure(mf,
                    class = c("mlogit.data", "data.frame"),
                    index = mfindex)

    result <- structure(
        list(
            coefficients  = x$coefficients,
            logLik        = logLik,
            gradient      = gradient,
            hessian       = hessian,
            est.stat      = x$est.stat,
            fitted.values = fitted,
            probabilities = probabilities,
            linpred       = linpred,
            indpar        = indpar,
            residuals     = resid,
            omega         = Omega,
            rpar          = rpar,
            nests         = nests,
            model         = mf,
            freq          = freq,
            formula       = formula,
            call          = callT),
        class = 'mlogit'
    ) 
    #result$Mi <- Mi
    #result$alt.lev <- alt.lev
    result
}

tological <- function(x){
    if (! is.logical(x)){
        if (is.factor(x)){
            if (length(levels(x)) != 2)
                stop("the number of levels for the choice variable should equal two")
            x <- x == levels(x)[2]
        }
        if (is.numeric(x)) x <- x != 0
    }
    x
}

suml <- function(x){
    n <- length(x)
    if (!is.null(dim(x[[1]]))){
        d <- dim(x[[1]])
        s <- matrix(0,d[1],d[2])
        for (i in 1:n){
            x[[i]][is.na(x[[i]])] <- 0
            s <- s+x[[i]]
        }
    }
    else{
        s <- rep(0,length(x[[n]]))
        for (i in 1:n){
            x[[i]][is.na(x[[i]])] <- 0
            s <- s+x[[i]]
        }
    }
    s
}
