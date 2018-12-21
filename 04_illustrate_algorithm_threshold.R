rm(list = ls())

source("R/threshold.R")

load("output/Japan_Pref_Epi_Params.rda")

# The background influenza activity level is higher than the empirical epidemic 
# threshold of 1.0 weekly influenza cases per sentinel
# pref <- "Okinawa"
pref <- "Tokyo"
s <- "2012/2013"
x <- subset(pref.flu1, season == s & Prefecture == pref)
dat <- data.frame(t = x$weeknum_of_season, y = x$flu.sentinel)
epi.params <- interp.ec(dat)
# 3 consective weeks at epidemic start
idx.start <- ceil(epi.params$epi.start)
cw.start <- dat[idx.start:(idx.start + 2), ]
# 3 consective weeks at epidemic end
idx.end <- floor(epi.params$epi.end)
cw.end <- dat[idx.end:(idx.end - 2), ]

p <- ggplot(data = dat, aes(t, y)) + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_line() + 
  geom_point(shape = 1) + 
  geom_point(data = cw.start, aes(t, y), shape = 1, color = "red") + 
  geom_point(data = subset(cw.start, t == idx.start), aes(t, y), color = "red") + 
  geom_point(data = cw.end, aes(t, y), shape = 1, color = "red") + 
  geom_point(data = subset(cw.end, t == idx.end), aes(t, y), color = "red") + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  labs(x = "Week number", y = "Number of influenza\n cases per sentinel", 
       title = glue("{pref}, {s}")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

pdf("figs/threshold_ec_illustration.pdf", width = 6, height = 3)
print(p)
dev.off()