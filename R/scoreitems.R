scoreitems <- function(param, data, fam){
    scorefun <- paste(fam,"fam",sep="")

    item.res <- apply(data, 1, function(x){
        do.call(scorefun, list(x[1], x[length(x)], param))
    })
    
    item.res
}
