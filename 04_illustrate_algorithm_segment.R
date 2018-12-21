rm(list = ls())

source("R/segment.R")

load("output/Japan_Pref_Epi_Params.rda")

# The background influenza activity level is higher than the empirical epidemic 
# threshold of 1.0 weekly influenza cases per sentinel
pref <- "Okinawa"
# pref <- "Tokyo"
s <- "2012/2013"
x <- subset(pref.flu1, season == s & Prefecture == pref)
ec <- data.frame(t = x$weeknum_of_season, y = x$flu.sentinel)
epi.params <- segment.ec(ec)

# segment
(pk.idx <- which.max(ec$y))
epi.peak <- ec$t[pk.idx]
epi.peak.num <- ec$y[pk.idx]
half1 <- ec[1:pk.idx, ]
half2 <- ec[pk.idx:nrow(ec), ]
library(segmented)
fit.lm1 <- lm(y ~ t, data = half1)
fit.seg1 <- segmented(fit.lm1, seg.Z = ~t, psi = half1$t[2])
half1$y.hat <- predict(fit.seg1)
psi1 <- fit.seg1$psi[2]
idx <- half1$t < psi1
t1 <- half1$t[idx]
y1 <- half1$y.hat[idx]
library(Hmisc)
res <- approxExtrap(x = t1[1:2], y = y1[1:2], 
                    xout = psi1)
y.psi1 <- res$y
t2 <- half1$t[!idx]
y2 <- half1$y.hat[!idx]
broken.line1 <- data.frame(t = c(t1, psi1, t2), y = c(y1, y.psi1, y2))

fit.lm2 <- lm(y ~ t, data = half2)
fit.seg2 <- segmented(fit.lm2, seg.Z = ~t, psi = half2$t[2])
half2$y.hat <- predict(fit.seg2)
psi2 <- fit.seg2$psi[2]
idx <- half2$t < psi2
t1 <- half2$t[idx]
y1 <- half2$y.hat[idx]
res <- approxExtrap(x = t1[1:2], y = y1[1:2], 
                    xout = psi2)
y.psi2 <- res$y
t2 <- half2$t[!idx]
y2 <- half2$y.hat[!idx]
broken.line2 <- data.frame(t = c(t1, psi2, t2), y = c(y1, y.psi2, y2))


p <- ggplot(data = ec, aes(t, y)) + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_line() + 
  geom_point(shape = 1) + 
  geom_line(data = broken.line1, aes(t, y), color = "red", linetype = "dashed") + 
  geom_point(data = subset(broken.line1, t == psi1), aes(t, y), color = "red") + 
  geom_line(data = broken.line2, aes(t, y), color = "red", linetype = "dashed") + 
  geom_point(data = subset(broken.line2, t == psi2), aes(t, y), color = "red") + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  labs(x = "Week number", y = "Number of influenza\n cases per sentinel", 
       title = glue("{pref}, {s}")) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

pdf("figs/segment_ec_illustration.pdf", width = 6, height = 3)
print(p)
dev.off()