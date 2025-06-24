# Required packages
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")

library(car)      # for Levene's Test
library(broom)    # for tidy outputs
library(knitr)    # for tables

spss_ttest <- function(data, group, value, group_labels = NULL, digits = 5) {
  # Ensure group is factor and relabel if needed
  data[[group]] <- as.factor(data[[group]])
  if (!is.null(group_labels)) levels(data[[group]]) <- group_labels

  # Levene's Test
  lev <- car::leveneTest(data[[value]], data[[group]])

  # Means & SDs for info
  means <- tapply(data[[value]], data[[group]], mean, na.rm=TRUE)
  sds   <- tapply(data[[value]], data[[group]], sd, na.rm=TRUE)
  ns    <- tapply(data[[value]], data[[group]], function(x) sum(!is.na(x)))

  # t-test: both equal var & unequal var
  t_eq   <- t.test(data[[value]] ~ data[[group]], var.equal = TRUE)
  t_uneq <- t.test(data[[value]] ~ data[[group]], var.equal = FALSE)

  # SPSS-style table
  table <- data.frame(
    check.names = FALSE,
    row.names = c(
      "Levene's Test for Equality of Variances",
      "  F",
      "  Sig.",
      "t-test for Equality of Means",
      "  t",
      "  df",
      "  Sig. (2-tailed)",
      "  Mean Difference",
      "  Std. Error Difference",
      "  95% CI Lower",
      "  95% CI Upper"
    ),
    "Equal variances assumed" = c(
      "",
      round(lev$`F value`[1], digits),
      format.pval(lev$`Pr(>F)`[1], digits = digits),
      "",
      round(t_eq$statistic, digits),
      round(t_eq$parameter, digits),
      format.pval(t_eq$p.value, digits = digits),
      round(diff(rev(t_eq$estimate)), digits),
      round(t_eq$stderr, digits),
      round(t_eq$conf.int[1], digits),
      round(t_eq$conf.int[2], digits)
    ),
    "Equal variances not assumed" = c(
      "",
      "", # Levene's F only shown once
      "", # Levene's p only shown once
      "",
      round(t_uneq$statistic, digits),
      round(t_uneq$parameter, digits),
      format.pval(t_uneq$p.value, digits = digits),
      round(diff(rev(t_uneq$estimate)), digits),
      round(t_uneq$stderr, digits),
      round(t_uneq$conf.int[1], digits),
      round(t_uneq$conf.int[2], digits)
    )
  )

  # Output
  cat("\nIndependent Samples Test\n")
  cat("====================================================\n")
  print(knitr::kable(table, align = "c", format = "markdown"))
  cat("\n")

  # Optionally, print group means/SDs as SPSS does above this table:
  info <- data.frame(
    Group = names(means),
    Mean = round(as.numeric(means), digits),
    `Std. Deviation` = round(as.numeric(sds), digits),
    N = as.numeric(ns),
    check.names = FALSE
  )
  cat("Group Statistics\n")
  print(knitr::kable(info, align = "c", format = "markdown"))
  cat("\nNote. Both equal and unequal variance t-tests are reported as in SPSS. See Levene's test for variance assumption.\n")
}
##TESTING
df <- data.frame(grouping = c(1,2,1,1,2,1,1,2,2), scores = c(1,4,3,2,5,7,2,2,3))
spss_ttest_flextable(df, group = "grouping", value = "scores")
