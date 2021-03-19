

# get_pvalue - Results to compare against ---------------------------------

  ref1 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=0)
  ref2 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1)
  ref3 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=1.5)
  ref4 <- survival::survdiff(formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte, rho=2.4)

  get_pvalue_ref1 <- data.frame(
    `Equality across strata` = "Log-Rank",
    Chisq = ref1$chisq,
    df = length(ref1$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref1$chisq, length(ref1$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref2 <- data.frame(
    `Equality across strata` = "Wilcoxon",
    Chisq = ref2$chisq,
    df = length(ref2$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref2$chisq, length(ref2$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref3 <- data.frame(
    `Equality across strata` = "Tarone-Ware",
    Chisq = ref3$chisq,
    df = length(ref3$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref3$chisq, length(ref3$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  get_pvalue_ref4 <- data.frame(
    `Equality across strata` = paste0("Harrington and Fleming test (rho = ", 2.4, ")"),
    Chisq = ref4$chisq,
    df = length(ref4$n)-1,
    `p-value` = .pvalformat(stats::pchisq(ref4$chisq, length(ref4$n) - 1, lower.tail = FALSE)),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  get_pvalue_ref <- base::rbind.data.frame(
    get_pvalue_ref1,
    get_pvalue_ref2,
    get_pvalue_ref3,
    get_pvalue_ref4,
    make.row.names = F
  )

  get_pvalue_ref134 <- base::rbind.data.frame(
    get_pvalue_ref1,
    get_pvalue_ref3,
    get_pvalue_ref4,
    make.row.names = F
  )








