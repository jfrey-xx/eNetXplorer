Hacking eNetXplorer to set specific lambda values with new `lambda` option (mutually exclusive with `nlambda.ext`, which takes precedence. 

Also add the option to manually specify the `foldid` of data. Warning: vector should be same length as data, and `n_fold` should match. If the order is important (e.g. to implement leave one subject out), use `sample_foldid=FALSE`, and, because it becomes irrelevant, `n_run=1` (in the original code each run randomise the foldid vector). Increase as much `n_perm_null`.

Note that by default eNetXplorer remove the intercept, i.e. the first column of the matrix... even if you remove already it beforehand. Hence leave the intercept (but might require `scaled=FALSE` otherwise crash during z-score computation), or use the new option `keep_intercept=TRUE`.

At the moment only applicable for "gaussian" family.

TODO: manually select the lambda to be studied (e.g. look at features) instead of just "best lambda".

TODO: sanity check for `foldid` and `n_fold` paramaters.