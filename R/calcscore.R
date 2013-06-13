calcscore <-
function(forecast, outcome, fam="pow", param=c(2,rep(1/NCOL(forecast),NCOL(forecast))), data, scaling=FALSE){

    cl <- match.call()
    m <- match(c("forecast", "outcome"), names(cl), 0L)
    argnms <- as.character(cl[m])

    ## Get data
    ## FIXME More elegant way to do this?
    if(!missing(data)){
        if(argnms[1] %in% names(data)){
            forecast <- eval(substitute(data$y, list(y=argnms[1])))
        }
        if(argnms[2] %in% names(data)){
            outcome <- eval(substitute(data$y, list(y=argnms[2])))
        }
    }
    
    ## Error checks:
    ## Eventually want to make sure beta family is not used with
    ## >2 alternatives.
    if(NCOL(forecast)>2) stop("Scoring >2 alternatives not yet implemented.")
    if(is.matrix(forecast)){
        if(any(apply(forecast,1,sum)!=1)){
            stop("For some items, the forecasts do not sum to 1.")
        }
    }
    if(NROW(forecast)!=NROW(outcome)){
        stop("Number of rows of forecast matrix does not equal number of outcomes.")
    }
    ## Check beta family params
    if(fam=="beta"){
        if(any(param <= -1)) stop("Beta family parameters must be greater than -1")
    }
    ## If outcome is matrix, convert to vector
    if(!is.null(dim(outcome))){
        if(any(dim(outcome)==1)){
            outcome <- as.numeric(outcome)
        } else {
            if(nrow(outcome)!=nrow(forecast)){
                stop("Number of outcome rows != Number of forecast rows")
            }
        }
        outcome <- apply(outcome, 1, function(x) which(x==1))
        ## TODO Take this out when have multiple alternatives:
        outcome <- 2-outcome
    }
    ## TODO For fam=pow or sph, check to ensure that baseline params sum to 1.
    ## (only important for more than 2 alternatives)
    
    ## END ERROR CHECKING
    
    ## Create data matrix
    if(NCOL(forecast)==1) forecast <- cbind(forecast, 1-forecast)
    datmat <- cbind(forecast, outcome)
    ## Obtain unscaled scores
    sc <- scoreitems(param, datmat, fam)
    ## Scale if desired
    if(scaling){
        scalefactor <- scalescores(param, fam)

        if(fam=="beta"){
            ## TODO consider other scaling for beta family?
            sc <- sc/scalefactor
        } else {
            sc <- (sc - scalefactor[1])/diff(scalefactor)
        }
    }
    if(any(is.na(sc))){
        stop("Problem with score calculation.  Ensure parameter values are valid.")
    }
    sc
}
