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
  
  # Levene's Test
  lev <- car::leveneTest(data[[value]], data[[group]])
  
  # Means & SDs for info
  means <- tapply(data[[value]], data[[group]], mean, na.rm=TRUE)
  sds   <- tapply(data[[value]], data[[group]], sd, na.rm=TRUE)
  ns    <- tapply(data[[value]], data[[group]], function(x) sum(!is.na(x)))
  
  # t-test: both equal var & unequal var
  t_eq   <- t.test(data[[value]] ~ data[[group]], var.equal = TRUE)
  t_uneq <- t.test(data[[value]] ~ data[[group]], var.equal = FALSE)
#imma explore how to setup the flextable
  sample.dataframe <- data.frame(BlankCol = c("Scores" #should be referenced to one of the inputs when building the function
                                            ,""),Grouping = names(means), 
                               N = as.numeric(ns), 
                               Mean = round(as.numeric(means), digits),
                               `Std. Deviation` = round(as.numeric(sds), digits), 
                               `Std Error Mean` = c(1.32464, 1.23134))


  ft <- flextable(sample.dataframe)
  ft <- set_caption(ft, "Group Statistics")
#Now i need to fix the BlankCol to an empty string
  ft <- set_header_labels(ft, BlankCol = "")
  ft
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
  return(ft)
}

## Now i need to draft the second table
