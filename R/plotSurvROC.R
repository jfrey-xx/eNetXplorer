plotSurvROC <- function (
x, alpha.index=NULL, survAUC_time, survAUC_method=c("KM","NNE"), survAUC_lambda=NULL, survAUC_span=NULL,
xlab="False positive rate (1 - Specificity)", ylab="True positive rate (Sensitivity)", cex.lab=1,
main=NULL, col.main="black", cex.main=0.95, status0="censored", status1="event", ...)
{
    if (is.null(alpha.index)) {
        alpha.index = 1:length(x$alpha)
    }
    survAUC_method = match.arg(survAUC_method)
    if (is.null(survAUC_time)) {
        stop("Error: survAUC_time must be provided")
    }
    y = x$response
    for (i_alpha in alpha.index) {
        pred_mean = x$predicted_values[[i_alpha]][,1]
        if ((survAUC_method=="NNE")&is.null(survAUC_span)&is.null(survAUC_lambda)) {
            survAUC_span = 0.25*nrow(y)^(-0.20) # default value suggested per "survivalROC" documentation
        }
        for (pred_time in survAUC_time) {
            survROC = survivalROC(Stime=y[,"time"],status=y[,"status"],marker=pred_mean,
            predict.time=pred_time,method=survAUC_method,lambda=survAUC_lambda,span=survAUC_span,...)
            plot(survROC$FP, survROC$TP, type="l", xlim=c(0,1), ylim=c(0,1),
            xlab=xlab, ylab=ylab)
            abline(0,1,lty=3)
            if (is.null(main)) {
                n_status0 = sum((y[,"status"]==0)&(y[,"time"]>=pred_time))
                n_status1 = sum((y[,"status"]==1)&(y[,"time"]<=pred_time))
                perc_status0 = round(100*n_status0/(n_status0+n_status1))
                perc_status1 = round(100*n_status1/(n_status0+n_status1))
                main_top = paste0("alpha=",x$alpha[i_alpha]," ; timepoint=",pred_time)
                main_center = paste0(status0,": n=",n_status0," (",perc_status0,"%) ; ",status1,": n=",n_status1," (",perc_status1,"%)")
                if ((x$survAUC)&(identical(x$survAUC_time,survAUC_time))&(identical(x$survAUC_method,survAUC_method))&(identical(x$survAUC_lambda,survAUC_lambda))&(identical(x$survAUC_span,survAUC_span))) {
                    AUC_05 = round(x$AUC_perc05[which(survAUC_time==pred_time),i_alpha],2)
                    AUC_50 = round(x$AUC_perc50[which(survAUC_time==pred_time),i_alpha],2)
                    AUC_95 = round(x$AUC_perc95[which(survAUC_time==pred_time),i_alpha],2)
                    main_bottom = paste0("AUC=",AUC_50," (95% CI: ",AUC_05,"-",AUC_95,")")
                } else {
                    main_bottom = paste0("AUC=",round(survROC$AUC,2))
                }
                main.title = paste0(main_top,"\n",main_center,"\n",main_bottom)
            } else {
                main.title = main
            }
            title(main=main.title,cex.main=cex.main,col.main=col.main)
        }
    }
}
