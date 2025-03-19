library(dplyr)
library(visbuilder)

dfm <- mtcars %>% select(wt, mpg, cyl)
dfm$name <- rownames(dfm)

head(dfm[, c("name", "wt", "mpg", "cyl")])
dfm$mpg_z <- (dfm$mpg -mean(dfm$mpg))/sd(dfm$mpg)
dfm$mpg_grp <- factor(ifelse(dfm$mpg_z < 0, "low", "high"), levels = c("low", "high"))
dfm <- dfm %>%
  group_by(mpg_grp) %>%
  arrange(mpg_z, .by_group = TRUE)

dfm$name <- factor(dfm$name, levels = rev(dfm$name))

barplot.horizontal.filled(dat = dfm)


