# Required packages
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")

library(car)      # for Levene's Test
library(broom)    # for tidy outputs
library(knitr)    # for tables
library(flextable)
library(dplyr)

SPSS_ttest_flextable <- function(data, group, value, group_labels = NULL, digits = 5) {
  # Ensure group is factor and relabel if needed
  data[[group]] <- as.factor(data[[group]])
  if (!is.null(group_labels)) levels(data[[group]]) <- group_labels
 # Ensure that there is exactly 2 groups
  
  # Levene's Test
  lev <- car::leveneTest(data[[value]], data[[group]])
  
  # Means & SDs for info
  means <- tapply(data[[value]], data[[group]], mean, na.rm=TRUE)
  sds   <- tapply(data[[value]], data[[group]], sd, na.rm=TRUE)
  ns    <- tapply(data[[value]], data[[group]], function(x) sum(!is.na(x)))
  df_name <- deparse(substitute(value))
  cleaned_name <- gsub("\"", "", df_name)
  
  
  # t-test: both equal var & unequal var
  t_eq   <- t.test(data[[value]] ~ data[[group]], var.equal = TRUE)
  t_uneq <- t.test(data[[value]] ~ data[[group]], var.equal = FALSE)
#imma explore how to setup the flextable
  sample.dataframe <- data.frame(BlankCol = c(cleaned_name
                                            ,""),Grouping = names(means), 
                               N = as.numeric(ns), 
                               Mean = round(as.numeric(means), digits),
                               `Std. Deviation` = round(as.numeric(sds), digits), 
                               `Std Error Mean` = `Std. Deviation`/sqrt(N))


  ft <- flextable(sample.dataframe)
  ft <- set_caption(ft, "Group Statistics")
#Now i need to fix the BlankCol to an empty string
  ft <- set_header_labels(ft, BlankCol = "")

#Then shade columns 1 and 2 and add a border line
# to add border i'll use the officer library
  library(officer)
  small_border <- fp_border(color = "lightgray", width = 1)
  ft <- border_inner_h(ft, part = "body", border = fp_border(color = "gray", width = 1))
  ft <- border_inner_v(ft, part = "body", border = small_border)
  ft <- bg(ft, i = 1:nrow(sample.dataframe), j = 1:2, bg = "lightgray", part = "body")
#Modify the fonts to courier new
# Header: Times New Roman, regular
  ft <- font(ft, part = "header", fontname = "Arial Narrow")
  ft <- bold(ft, part = "header", bold = FALSE)
  ft <- fontsize(ft, part = "header", size = 11)

# Body: Courier New
  ft <- font(ft, part = "body", fontname = "Arial Narrow")
  ft <- fontsize(ft, part = "body", size = 11)
# Build df2
  df_2 <- data.frame(
    blankcol1 = c(cleaned_name, ""),
    blankcol2 = c("Equal Variances assumed", "Equal variances not assumed"),
    `F` = c(round(lev$`F value`[1], digits), "" ),
    Sig = c(format.pval(lev$`Pr(>F)`[1], digits = digits), ""),
    `t` = c(round(t_eq$statistic, digits),round(t_uneq$statistic, digits)),
    df = c(round(t_eq$parameter, digits), round(t_uneq$parameter, digits)),
    `Sig. (2-tailed)` =c(format.pval(t_eq$p.value, digits = digits), format.pval(t_uneq$p.value, digits = digits)),
    `Mean Difference` = c(round(diff(rev(t_uneq$estimate)), digits),round(diff(rev(t_uneq$estimate)), digits)),
    `Std. Error Difference` = c( round(t_eq$stderr, digits), round(t_uneq$stderr, digits)),
    `95% Confidence Lower` = c(round(t_eq$conf.int[1], digits), round(t_uneq$conf.int[1], digits)),
    `95% Confidence Upper` = c( round(t_eq$conf.int[2], digits), round(t_uneq$conf.int[2], digits))
  )
  
  #start the intial flextable2
  ft2 <- flextable(df_2)
  
  ft2 <- add_header_row(ft2,  
                        colwidths = c(2, 2, 7),
                        values = c("","Levene's Test for Equality of Variance","t-test for Equality of Means")
  )
  ft2 <- align(ft2, align = "center" , part = "header")
  ft2 <- set_header_labels(ft2, blankcol1 = '')
  ft2 <- set_header_labels(ft2, blankcol2 = '')
  ft2 <- set_header_labels(ft2, X95..Confidence.Lower = "95% Confidence Lower")
  ft2 <- set_header_labels(ft2, X95..Confidence.Upper = "95% Confidence Upper")
  ft2<- border_inner_h(ft2,border = fp_border(color = "transparent"), part = "header")
  
}
