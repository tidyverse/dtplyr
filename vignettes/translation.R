library(data.table)

# Special considerations
# * need partial_eval() pass to handle quosures (and do n() -> .N etc)
# * use step environment to only need to specially handle quosures that aren't
#   in that environment
# * how to write eager data.table methods to maximise code reuse?

# df %>% slice(2:n) -> df[2:.N]
# df %>% slice(2:n) -> df[ .SD[2:.N], by = g]
# df %>% sample_n(10) -> df[sample(.I, 10),]
# df %>% sample_n(10) -> df[, .SD[sample(.I, 10)], by = g]

# always use i - grouping has no effect
# df1 %>% left_join(df2) -> df1[df2, on = c()]
# df1 %>% right_join(df2) -> df2[df1, on = c()]
# df1 %>% anti_join(df2) -> df1[!df2, on = c(?)]
# df1 %>% semi_join(df2) -> df1[unique(df1[df2, which = TRUE, nomatch = 0L])]
# df1 %>% semi_join(df2) -> d1[!(d1[!d2, on = "x"]), on = "x"]

# special
# df1 %>% inner_join(df2) -> merge(df1, df2, all = FALSE)
# df1 %>% outer_join(df2) -> merge(df1, df2, all = TRUE)
