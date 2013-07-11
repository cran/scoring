scalescores <-
function(pars, fam="pow"){
  ps <- seq(0,1,.01)

  if(fam=="beta"){
      ## TODO Match scaling in paper?
      xplier <- max(betafam(0, d=1, param=pars),
                    betafam(1, d=0, param=pars))

      if(xplier == Inf){
          warning("Scaling does not work because maximum possible score is Inf.")
          xplier <- 1
      }
      
  } else if(fam=="pow"){
      sc1 <- sapply(ps, function(p) powfam(p, 1, param=pars))
      sc0 <- sapply(ps, function(p) powfam(p, 0, param=pars))
      xplier <- c(min(c(sc1,sc0)),max(c(sc1,sc0)))
      if(any(is.na(c(sc1,sc0)))){
          warning("Scaling does not work because maximum possible score is Inf.")
          xplier <- c(0,1)
      }
  } else if(fam=="sph"){
      sc1 <- sapply(ps, function(p) sphfam(p, 1, param=pars))
      sc0 <- sapply(ps, function(p) sphfam(p, 0, param=pars))
      xplier <- c(min(c(sc1,sc0)),max(c(sc1,sc0)))
      if(any(is.na(c(sc1,sc0)))){
          warning("Scaling does not work because maximum possible score is Inf.")
          xplier <- c(0,1)
      }
  }

  xplier
}
