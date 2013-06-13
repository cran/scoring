plotscore <-
function(param=c(2,.5), fam="pow", scaling=FALSE, legend=TRUE, ...){
    ## Plot scoring rules (for two-alternative rules only)
    p <- seq(.01,.99,.01)

    sc1 <- calcscore(p, rep(1,length(p)), fam, param, scaling=scaling)
    sc0 <- calcscore(p, rep(0,length(p)), fam, param, scaling=scaling)

    if(scaling){
        yl <- c(min(sc1,sc0), max(sc1,sc0))
    } else {
        yl <- c(min(sc1,sc0)-.5, max(sc1,sc0)+.5)
    }

    main.arg <- list(x=p, y=sc1)
    supplied <- list(...)
    default <- list(type="l", ylim=yl, xlab="Forecast", ylab="Score")
    nomatch <- setdiff(c("type","xlab","ylab","ylim"), names(supplied))
    plot.args <- c(main.arg, supplied, default[nomatch])

    do.call(plot, plot.args)
    lines(p, sc0, lty=2)
    if(legend) legend(.8, yl[2] - .1, c("d=1","d=0"), lty=c(1,2))
}
