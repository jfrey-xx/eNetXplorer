## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("eNetXplorer")

## ---- message=FALSE-----------------------------------------------------------
data_gen <- function(n_inst, covmat, seed=123) {
    library (expm);
    set.seed(seed)
    data <- matrix(rnorm(n_inst*ncol(covmat)),ncol=ncol(covmat))%*%sqrtm(covmat)
    predictor=data[,-1]
    rownames(predictor) = paste0("Inst.",1:n_inst)
    colnames(predictor) = paste0("Feat.",1:(ncol(covmat)-1))
    list(response=data[,1],predictor=predictor)
}

## -----------------------------------------------------------------------------
covmat_gen <- function(n_pred, block_size, r_resp, r_block) {
    covmat = matrix(rep(1.e-3,(n_pred+1)**2),ncol=(n_pred+1))
    for (i_pred in 1:block_size) {
        for (j_pred in (i_pred+1):(block_size+1)) {
            if (i_pred==1) {
                covmat[i_pred,j_pred] = r_resp
            } else {
                covmat[i_pred,j_pred] = r_block
            }
            covmat[j_pred,i_pred] = covmat[i_pred,j_pred]
        }
    }
    for (i_pred in 1:n_pred) {
        covmat[i_pred,i_pred] = 1
    }
    covmat
}

## ---- message=FALSE-----------------------------------------------------------
data = data_gen(n_inst=50, covmat_gen(n_pred=60, block_size=5, r_resp=0.5, r_block=0.35))

## ---- echo=FALSE, message=FALSE, fig.height = 4.5, fig.width = 6.5, fig.align = "left"----
library(gplots)
library(RColorBrewer)
cor_mat = cor(data$predictor)
cor_max = max(abs(range(cor_mat[-seq(1,ncol(cor_mat)^2,ncol(cor_mat)+1)])))
n_breaks = 10
breaks = seq(-cor_max,cor_max,2*cor_max/(n_breaks-1))
heatmap.2(cor_mat,col=redgreen,breaks=breaks,scale="none",dendrogram="none",Rowv=F,Colv=F,margins=c(5,8),cexRow=0.5,cexCol=0.5,trace="none")

## ---- echo=FALSE, fig.height = 3.6, fig.width = 5.5, fig.align = "left"-------
plot(cor(data$predictor,data$response),xaxt="n",xlab="predictor",ylab="correlation to response")
axis(side=1, at=1:ncol(data$predictor),labels=colnames(data$predictor), las=2, cex.axis=0.5)

## -----------------------------------------------------------------------------
library(eNetXplorer)

## ---- warning=FALSE, eval=FALSE, tidy=TRUE------------------------------------
#  fit_def = eNetXplorer(x=data$predictor,y=data$response,family="gaussian")

## ---- warning=FALSE, eval=FALSE, tidy=TRUE------------------------------------
#  fit = eNetXplorer(x=data$predictor,y=data$response,family="gaussian",alpha=seq(0,1,by=0.1),n_run=1000,n_perm_null=250,seed=123)

## ---- echo=FALSE--------------------------------------------------------------
# OR, for our purposes, we upload the object previously generated:
load("Case1_r3_fit.Robj")

## -----------------------------------------------------------------------------
summary(fit)

## ---- warning=FALSE, fig.height = 4, fig.width = 5.5, fig.align = "left"------
plot(fit, plot.type="summary") 

## ---- warning=FALSE, tidy=TRUE, fig.height = 4.2, fig.width = 5.5, fig.align = "left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="featureCaterpillar", stat=c("coef"))

## ---- warning=FALSE, tidy=TRUE, fig.height = 4.5, fig.width = 6.5, fig.align = "left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="featureHeatmap", stat=c("coef"),notecex=1.5) 

## ---- warning=FALSE, tidy=TRUE, fig.height = 4, fig.width = 5.5, fig.align = "left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="lambdaVsQF") 

## ---- warning=FALSE, tidy=TRUE, fig.height = 4, fig.width = 5.5, fig.align = "left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="measuredVsOOB") 

## -----------------------------------------------------------------------------
data = data_gen(n_inst=50, covmat_gen(n_pred=60, block_size=1, r_resp=0.7, r_block=0.35))

## ---- echo=FALSE, fig.height = 4.5, fig.width = 6.5, fig.align = "left"-------
library(gplots)
library(RColorBrewer)
cor_mat = cor(data$predictor)
cor_max = max(abs(range(cor_mat[-seq(1,ncol(cor_mat)^2,ncol(cor_mat)+1)])))
n_breaks = 10
breaks = seq(-cor_max,cor_max,2*cor_max/(n_breaks-1))
heatmap.2(cor_mat,col=redgreen,breaks=breaks,scale="none",dendrogram="none",Rowv=F,Colv=F,margins=c(5,8),cexRow=0.5,cexCol=0.5,trace="none")

## ---- echo=FALSE, fig.height = 3.6, fig.width = 5.5, fig.align ="left"--------
plot(cor(data$predictor,data$response),xaxt="n",xlab="predictor",ylab="correlation to response")
axis(side=1, at=1:ncol(data$predictor),labels=colnames(data$predictor), las=2, cex.axis=0.5)

## ---- warning=FALSE, eval=FALSE, tidy=TRUE------------------------------------
#  fit = eNetXplorer(x=data$predictor,y=data$response,family="gaussian",alpha=seq(0,1,by=0.1),n_run=1000,n_perm_null=250,seed=123)

## ---- echo=FALSE--------------------------------------------------------------
# OR, for our purposes, we upload the object previously generated:
load("Case2_r3_fit.Robj")

## -----------------------------------------------------------------------------
summary(fit)

## ---- warning=FALSE, fig.height = 4, fig.width = 5.5, fig.align ="left"-------
plot(fit, plot.type="summary") 

## ---- warning=FALSE, tidy=TRUE, fig.height = 4.2, fig.width = 5.5, fig.align ="left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="featureCaterpillar", stat=c("coef"))

## ---- warning=FALSE, tidy=TRUE, fig.height = 4.5, fig.width = 6.5, fig.align ="left"----
plot(fit, alpha.index = which.max(fit$model_QF_est), plot.type="featureHeatmap", stat=c("coef"),notecex=1.5) 

## ---- eval=FALSE--------------------------------------------------------------
#  data(H1N1_Flow)

## ---- eval=FALSE--------------------------------------------------------------
#  data(Leukemia_miR)

## ---- eval=FALSE--------------------------------------------------------------
#  expr_full = Leuk_miR_full$expression_matrix
#  miR_filter = rep(F,nrow(Leuk_miR_full$miR_metadata))
#  miR_filter[apply(expr_full,2,mean)>1.2] = T
#  sample_filter = rep(T,nrow(Leuk_miR_full$sample_metadata))
#  sample_filter[Leuk_miR_full$sample_metadata$sample_class=="Normal"] = F
#  expr_filtered = expr_full[sample_filter,miR_filter]
#  miR_filtered = Leuk_miR_full$miR_metadata[miR_filter,]
#  sample_filtered = Leuk_miR_full$sample_metadata[sample_filter,]

