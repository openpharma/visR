data(lung)

surv_eq <- "survival::Surv(time, status) ~ sex"

## lung data as is
lung%>%
  group_by(sex) %>%
  summarize(n=n_distinct(row_number()))

## lung data vignette: selection was done
lung_vignet <- lung %>%
  filter(!is.na(wt.loss), !is.na(status), !is.na(ph.ecog)) %>%
  mutate(AVAL = time, CNSR  = ifelse(status == 2, 0, 1))

lung_vignet %>%
  group_by(sex) %>%
  summarize(n=n_distinct(row_number()))

survfit(Surv(time, status) ~ sex, data = lung_vignet)

(fit <- vr_KM_est(data=lung_vignet, strata = "sex"))

get_pvalue(fit) %>%
vr_render_table("Equality between Strata", "Summary table with test of equality over strata", "source:  blah")

visR:::vr_render_gt