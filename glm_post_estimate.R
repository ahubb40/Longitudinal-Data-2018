glm.post.estimate=function(glmob,comps,labs=NULL,rounded=3,exponentiate=FALSE) {
  if(is.matrix(comps)==FALSE) {
    comps=t(as.matrix(comps))}
  vc=vcov(glmob)
  ests = coef(glmob)
  linear.ests=as.vector(comps%*%ests)
  vcests=comps%*%vc%*%t(comps)
  ses=sqrt(diag(vcests))
  pvalue=(2*(1-pnorm(abs(linear.ests/ses))))
  if(exponentiate) {
    l95ci = exp(linear.ests - 1.96 * ses)
    exp_beta = exp(linear.ests)
    u95ci = exp(linear.ests + 1.96 * ses)
    summ = cbind(Ratio.est=format(round(exp_beta,rounded),nsmall=rounded),CI=paste(format(round(l95ci,rounded),nsmall=rounded),format(round(u95ci,rounded),nsmall=rounded),sep=" - "), pvalue=format(round(pvalue,rounded),nsmall=rounded))     
    rownames(summ)=labs
  }
  if(exponentiate==FALSE) {
    l95ci = (linear.ests - 1.96 * ses)
    beta = (linear.ests)
    u95ci = (linear.ests + 1.96 * ses)
    summ = cbind(Est=format(round(beta,rounded),nsmall=rounded),CI=paste(format(round(l95ci,rounded),nsmall=rounded),format(round(u95ci,rounded),nsmall=rounded),sep=" - "), pvalue=format(round(pvalue,rounded+1),nsmall=rounded))  
    rownames(summ)=labs
  }
  return(summ) }
