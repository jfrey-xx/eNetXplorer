Hacking eNetXplorer to set specific lambda values with new `lambda` option (mutually exclusive with `nlambda.ext`, which takes precedence. 

Also add the option to manually specify the `foldid` of data. Warning: vector should be same length as data, and `n_fold` should match.

At the moment only applicable for "gaussian" family.

TODO: manually select the lambda to be studied (e.g. look at features) instead of just "best lambda".

TODO: sanity check for `foldid` and `n_fold` paramaters.