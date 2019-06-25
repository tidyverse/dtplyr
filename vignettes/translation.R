# Need to turn this into an Rmarkdown document which describes the
# basic process of translation in some detail.

library(data.table)

# Special considerations
# * need partial_eval() pass to handle quosures (and do n() -> .N etc)
# * use step environment to only need to specially handle quosures that aren't
#   in that environment
# * group_by gets key = FALSE argument? or key_by()?
# * how to write eager data.table methods to maximise code reuse?

# j -----------------------

# use i if not grouped (and for some simple cases when grouped)
# just j when grouped - but you can still put the j in the .SD's j
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
# df %>% head(n) -> head(df, n)

# Other notes ---------------

# Order is important when combining filter and summarise
# dt %>% group_by(g) %>% filter(x > 1) %>% summarise(x = sum(x)) ->
# dt[x > 1, .(x = sum(x)), by = g]

# dt %>% group_by(g) %>% summarise(x = sum(x)) %>% filter(x > 1) ->
# dt[, .(x = sum(x)), by = g][x > 1]

# dt[x > 1, .(x = sum(x)), by = g]
# is equivlent to
# dt[x > 1][, .(x = sum(x)), by = g]
# not
# dt[, .(x = sum(x)), by = g][x > 1]

# df1 %>% left_join(df2) %>% group_by(g) %>% summarise(x = mean(x))
# df1[df2, .(x = mean(x)), by = .(g)]

# Can't combine mutates with anything else, because it triggers
# modification in place
#
# df1 %>% arrange(x) %>% group_by(g) %>% mutate(y = y - min(y))
# df1[order(x)][, .(y := y - min(y)), by = g]
#
# df1 %>% group_by(g) %>% mutate(y = y - min(y)) %>% arrange(x)
# df1[, .(y := y - min(y)), by = g][order(x)]

# dt %>% group_by(g) %>% filter(x > mean(x)) %>% summarise(x = sum(x)) ->
# dt[, .SD[x > mean(x), x = sum(x)], by = g]


