betafam <-
function(p, d, param){
    ## Define integrand:
    ## Inf and NaN arise when 0 is raised to a negative power.
    ## This happens when answ==p. 
    "betaint" <- function(p, d, param){
      ifelse(d==p, 0,
             (d*(1-p) + (1-d)*p)*(p^(param[1]-1))*((1-p)^(param[2]-1)))
    }

    lbnd <- d*p
    ubnd <- d + (1-d)*p

    res <- try(integrate(betaint, lbnd, ubnd, d=d, param=param, subdivisions=100)$value, silent=TRUE)

    if(inherits(res,"try-error")){
        warning("A score evaluates to infinity.")
        res <- Inf
    }
        #stop("Maximum possible score is probably infinity. Try scaling=FALSE.")

    res
}


powfam <-
function(p, d, param){
    ## Obtain score from power family with baseline for a single
    ## forecast.
    gam <- param[1]
    cpar <- param[2]

    ## Switch baseline forecast depending on d
    cpar <- cpar*d + (1-cpar)*(1-d)

    ri <- p*d + (1-p)*(1-d)

    tm1 <- ((ri/cpar)^(gam-1) - 1)/(gam - 1)
    tm2 <- ((ri^(gam)/cpar^(gam-1)) + ((1-ri)^(gam)/(1-cpar)^(gam-1)) - 1)/gam

    -(tm1 - tm2)
}


sphfam <-
function(p, d, param){
    ## Obtain score from pseudospherical family with baseline for a single
    ## forecast.
    gam <- param[1]
    cpar <- param[2]

    ri <- p*d + (1-p)*(1-d)
    ## Switch baseline forecast depending on d
    cpar <- cpar*d + (1-cpar)*(1-d)

    tm1 <- (ri/cpar)/((ri^(gam)/cpar^(gam-1) + (1-ri)^(gam)/(1-cpar)^(gam-1))^(1/gam))
    
    -(1/(gam-1)) * (tm1^(gam-1) - 1)
}
