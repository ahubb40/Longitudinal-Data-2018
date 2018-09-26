clx.lincom <- function(fm, dfcw, cluster,comps,rounded=4,labs=NULL){
   M <- length(unique(cluster))
   N <- length(cluster)
   dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
   u <- apply(estfun(fm),2,
                function(x) tapply(x, cluster, sum))
   vc <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
   ests = coef(fm)
   linear.ests=as.vector(comps%*%ests)
   vcests=t(comps)%*%vc%*%comps
   ses=sqrt(diag(vcests))
   pvalue=(2*(1-pnorm(abs(linear.ests/ses))))
   l95ci = (linear.ests - 1.96 * ses)
   beta = (linear.ests)
   u95ci = (linear.ests + 1.96 * ses)
   summ = cbind(Est=format(round(beta,rounded),nsmall=rounded),CI=paste(format(round(l95ci,rounded),nsmall=rounded),format(round(u95ci,rounded),nsmall=rounded),sep=" - "), pvalue=format(round(pvalue,rounded+1),nsmall=rounded))  
   rownames(summ)=labs
return(summ) 
}