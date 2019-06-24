df <- vroom::vroom("G1_1e7_1e2_0_0.csv")
dt <- data.table(df)


# Q1 ----------------------------------------------------------------------
bench::mark(
  dplyr = dt %>% group_by(id1) %>% summarise(sum(v1)),
  direct = dt[, .(v1 = sum(v1)), by = id1],
  min_iterations = 3,
  check = FALSE
)

dt %>% group_by(id1) %>% summarise(sum(v1))

# Q2 ----------------------------------------------------------------------
DF %>%
  group_by(id1, id2) %>%
  summarise(v1 = sum(v1))

DT[, .(v1=sum(v1)), by=.(id1, id2)]

# Q3 ----------------------------------------------------------------------
DF %>%
  group_by(id3) %>%
  summarise(v1 = sum(v1), v3 = mean(v3))

DT[, .(v1=sum(v1), v3=mean(v3)), by=id3]

# Q4 ----------------------------------------------------------------------
DF %>%
  group_by(id4) %>%
  summarise_at("mean", c("v1", "v2", "v3"))

DT[, lapply(.SD, mean), by=id4, .SDcols=v1:v3])

# Q5 ----------------------------------------------------------------------
DF %>%
  group_by(id6) %>%
  summarise_at("sum", c("v1", "v2", "v3"))

DT[, lapply(.SD, sum), by=id6, .SDcols=v1:v3])

# Q6 ----------------------------------------------------------------------
DF %>%
  group_by(id2, id4) %>%
  summarise(median_v3 = median(v3), sd_v3 = sd(v3))

DT[, .(median_v3=median(v3), sd_v3=sd(v3)), by=.(id2, id4)])

# Q7 ----------------------------------------------------------------------
DF %>%
  group_by(id2, id4) %>%
  summarise(range_v1_v2 = max(v1) - min(v2))

DT[, .(range_v1_v2=max(v1)-min(v2)), by=.(id2, id4)]

# Q8 ----------------------------------------------------------------------
DF %>%
  select(id2, id4, largest2_v3 = v3) %>%
  arrange(desc(largest2_v3)) %>%
  group_by(id2, id4) %>%
  filter(row_number() <= 2L)

DT[order(-v3), .(largest2_v3 = head(v3, 2L)), by=.(id2, id4)]

# Q9 ----------------------------------------------------------------------
DF %>%
  group_by(id2, id4) %>%
  summarise(r2 = cor(v1, v2)^2)

DT[, .(r2=cor(v1, v2)^2), by=.(id2, id4)]

# Q10 ----------------------------------------------------------------------
DF %>%
  group_by(id1, id2, id3, id4, id5, id6) %>%
  summarise(v3 = sum(v3), count = n())

DT[, .(v3=sum(v3), count=.N), by=id1:id6]
