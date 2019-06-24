# Need to turn this into an Rmarkdown document which describes the
# basic process of translation in some detail.

library(data.table)

# Two modes: eager (direct operation on data.table) and lazy
# (created tbl_dt or similar). This is likely to break existing
# code but it should be worth it, since it'll make the resulting
# code much closer to idiomatic data.table performance (and we can
# always implement more optimisations later).

# Special considerations
# * need partial_eval() pass to handle quosures (and do n() -> .N etc)
# * use step environment to only need to specially handle quosures that aren't
#   in that environment
# * need to analyse mutate() for internal references
# * need to detect simple filters () for key optimisations
#   - ==, <, >, %in% involving only symbols and constants. Or just ungroup() ?
# * group_by gets key = FALSE argument? or key_by()?
# * how to write eager data.table methods to maximise code reuse?
# * lazy_dt(immutable = FALSE)
# * need to track whether or not operation has created an implicit copy
#   to ensure that mutate doesn't create an extra copy

# j -----------------------
# df %>% select(a:b) ->  df[, .(a, b, c)]
# df %>% rename(x = y) ->  df[, .(a, b, c, x = y)]
# df %>% summarise(x = mean(x)) -> df[, .(x = mean(x))]

# mutate - needs a copy
# df %>% mutate(x2 = x * 2) -> df[, x2 := x * 2] # NEEDS COPY
# df %>% mutate(x2 = x * 2, y2 = y * 2) -> df[, c(x2, y2) := .(x * 2, y * 2)] # NEEDS COPY
# df %>% mutate(x2 = x * 2, x4 = x2 * 4) -> df[, x2 := x * 2][, x4 := x2 * 2] # NEEDS COPY
# df %>% transmute(x2 = x * 2, x4 = x2 * 2) -> has to be mutate + select ?

# use i if not grouped (and for some simple cases when grouped)
# just j when grouped - but you can still put the j in the .SD's j
# df %>% filter(x == 1) -> df[x == 1]
# df %>% filter(x == max(x)) -> df[, .SD[x == max(x)], by = g]
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

# needs to preserve grouping, but not generate by
# df %>% arrange(x, y) -> df[order(x, y)]
# df %>% head(n) -> df[seq_len(min(!!n, .N), ]

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


