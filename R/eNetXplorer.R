eNetXplorer <- function(x, y, family=c("gaussian","binomial","multinomial"),
alpha=seq(0,1,by=0.2), nlambda=100, nlambda.ext=NULL,
seed=NULL, scaled=T, n_fold=5, n_run=100, n_perm_null=25, QF.FUN=NULL, QF_label=NULL, cor_method=c("pearson","kendall","spearman"), binom_method=c("accuracy","precision","recall","Fscore","specificity","auc"),
    multinom_method=c("avg accuracy", "avg precision","avg recall","avg Fscore"),
    binom_pos=NULL, fscore_beta=NULL, fold_distrib_fail.max=100, ...)
{
    family = match.arg(family)
    if (family=="gaussian") {
        res <- eNetXplorerGaussian(x=x, y=y, family=family, alpha=alpha, nlambda=nlambda, nlambda.ext=nlambda.ext,seed=seed, scaled=scaled, n_fold=n_fold, n_run=n_run,n_perm_null=n_perm_null, QF.FUN=QF.FUN, QF_label=QF_label, cor_method=match.arg(cor_method), ...)
    } else if (family=="binomial") {
        res <- eNetXplorerBinomial(x=x, y=y, family=family, alpha=alpha, nlambda=nlambda, nlambda.ext=nlambda.ext, seed=seed, scaled=scaled, n_fold=n_fold, n_run=n_run,
        n_perm_null=n_perm_null, QF.FUN=QF.FUN, QF_label=QF_label,
        binom_method=match.arg(binom_method), binom_pos=binom_pos, fscore_beta=fscore_beta,
        fold_distrib_fail.max=fold_distrib_fail.max, ...)
    } else if (family=="multinomial") {
        res <- eNetXplorerMultinomial(x=x, y=y, family=family, alpha=alpha, nlambda=nlambda, nlambda.ext=nlambda.ext, seed=seed, scaled=scaled, n_fold=n_fold, n_run=n_run,
        n_perm_null=n_perm_null, QF.FUN=QF.FUN, QF_label=QF_label,
        multinom_method=match.arg(multinom_method), fscore_beta=fscore_beta,
        fold_distrib_fail.max=fold_distrib_fail.max, ...)
    }
    res$call <- match.call()
    class(res) <- "eNetXplorer"
    res
}
