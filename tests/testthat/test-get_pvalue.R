# T1. The function accepts only a survival object
# T1.1 An error when an object of a different type is used
# T1.2 The function accepts a survival object
# 
# T2. The function excecutes the test the null hypothesis of no equality across strata or a linear trend test across strata
# T2.1 An error when another method is requested eg method = "blah"
# T2.2 No error when method = "Equality"
# T2.3 No error when method = "Trend"
# 
# T3. The function tests the null hypothesis of no difference across strata when the requested ptype is: method = "Equality"
# T3.1 An error When the number of strata < 2
# T3.2 An error when `ptype` is out of scope
# T3.3 Return the default ptype when `ptype` is NULL
# T3.4 An error when the requested `ptype` is "Custom" and `rho` is not specified
# T3.5 An error when the requested `statlist` is not part of the default
# T3.6 An error when the requested `statlist` is NULL
# T3.7 The `rho` arguments adds tests to ptype when not 0, 1 or 1.5
# 
# T4. The function tests the null hypothesis of a linear trend across strata when the requested ptype is: method = "Trend"
# T4.1 An error When the number of strata < 3
# T4.2 An error when `ptype` is out of scope
# T4.3 Return the default ptype when `ptype` is NULL
# T4.4 An error when the number of strata in the survfit_object does not match the length of the scores vector
# T4.5 An error when the requested `statlist` is not part of the default
# T4.6 An error when the requested `statlist` is NULL
# T4.7 An error when the length of `scores` does not match the number of strata

