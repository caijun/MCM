rm(list = ls())

source("R/curvature.R")
source("R/helper.R")

load("output/Japan_Flu_Sentinel.rda")
load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)
library(plyr)

df <- subset(pref.flu1, season == "2014/2015" & Prefecture == "Saga")
ec <- data.frame(t = df$weeknum_of_season, y = df$flu.sentinel)
x <- calc.curvature(ec, n = 5, h = 5)

# sensitivity analyses of n and h for MCM
nh <- expand.grid(n = c(3, 5, 7), h = c(4.0, 6.0, 8.0, 10.0))

mcm.sa <- ddply(pref.flu1, .(Prefecture, season), function(df) {
  ec <- data.frame(t = df$weeknum_of_season, y = df$flu.sentinel)
  pref.res <- ddply(nh, .(n, h), function(par) {
    curvature.ec(ec, par$n, par$h, smoothing = FALSE)
  })
  return(pref.res)
})

mcm.sa <- mcm.sa %>% 
  mutate(pre.season.ending = pre.season.ending(season)) %>% 
  mutate(epi.start.date = pre.season.ending + epi.start * 7, 
         epi.end.date = pre.season.ending + epi.end * 7, 
         epi.peak.date = pre.season.ending + epi.peak * 7)

library(latex2exp)
library(glue)
library(cowplot)
# compare epidemic params from MCM with those from those from ETM
# epidemic onset
plist <- list()
sa.epi.start <- data.frame()
for (i in 1:nrow(nh)) {
  ni <- nh[i, "n"]
  hi <- nh[i, "h"]
  nh.mcm <- subset(mcm.sa, n == ni & h == hi)
  pd <- data.frame(epi.params.etm[c(1:2)], 
                   etm = epi.params.etm$epi.start, 
                   mcm = nh.mcm$epi.start)
  lmfit <- lm(etm ~ mcm, data = pd)
  b0 <- coef(lmfit)[1]
  b1 <- coef(lmfit)[2]
  (r2 <- summary(lmfit)$r.squared)
  (pval <- anova(lmfit)$'Pr(>F)'[1])
  
  p <- ggplot() + 
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
    geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
                se = FALSE) + 
    geom_point(data = pd, aes(x = mcm, y = etm), shape = 1) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 30), 
                       breaks = seq(0, 30, by = 5)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 30), 
                       breaks = seq(0, 30, by = 5)) + 
    labs(x = "MCM", y = "ETM", 
         title = TeX(glue("$\\mathit{{m}}$ = {ni}, $\\mathit{{h}}$ = {format(hi, nsmall = 1)}"))) + 
    theme_classic() +  
    theme(axis.line = element_blank(), 
          panel.border = element_rect(color = "black", size = 1, fill = NA), 
          plot.title = element_text(hjust = 0.5, face = "bold"))
  p <- ggdraw(p) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.45, 0.8, size = 12) + 
    draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
               0.45, 0.74, size = 12)
  plist[[i]] <- p
  
  res <- data.frame(n = ni, h = hi, b0, b1, r2, pval, row.names = NULL)
  sa.epi.start <- rbind(sa.epi.start, res)
}

outfile <- glue("figs/sensitivity_analyses/sa_epi_start.pdf")
pdf(outfile, width = 9, height = 12)
p <- plot_grid(plotlist = plist, align = "hv", ncol = 3)
# now add the title
title <- ggdraw() + draw_label("Epidemic onset", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.04, 1)) # rel_heights values control title margins
dev.off()


# epidemic end
plist <- list()
sa.epi.end <- data.frame()
for (i in 1:nrow(nh)) {
  ni <- nh[i, "n"]
  hi <- nh[i, "h"]
  nh.mcm <- subset(mcm.sa, n == ni & h == hi)
  pd <- data.frame(epi.params.etm[c(1:2)], 
                   etm = epi.params.etm$epi.end, 
                   mcm = nh.mcm$epi.end)
  lmfit <- lm(etm ~ mcm, data = pd)
  b0 <- coef(lmfit)[1]
  b1 <- coef(lmfit)[2]
  (r2 <- summary(lmfit)$r.squared)
  (pval <- anova(lmfit)$'Pr(>F)'[1])
  
  p <- ggplot() + 
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
    geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
                se = FALSE) + 
    geom_point(data = pd, aes(x = mcm, y = etm), shape = 1) + 
    scale_x_continuous(expand = c(0, 0), limits = c(25, 55), 
                       breaks = seq(25, 55, by = 5)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(25, 55), 
                       breaks = seq(25, 55, by = 5)) + 
    labs(x = "MCM", y = "ETM", 
         title = TeX(glue("$\\mathit{{m}}$ = {ni}, $\\mathit{{h}}$ = {format(hi, nsmall = 1)}"))) + 
    theme_classic() +  
    theme(axis.line = element_blank(), 
          panel.border = element_rect(color = "black", size = 1, fill = NA), 
          plot.title = element_text(hjust = 0.5, face = "bold"))
  p <- ggdraw(p) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.67, 0.28, size = 12) + 
    draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
               0.67, 0.22, size = 12)
  plist[[i]] <- p
  
  res <- data.frame(n = ni, h = hi, b0, b1, r2, pval, row.names = NULL)
  sa.epi.end <- rbind(sa.epi.end, res)
}

outfile <- glue("figs/sensitivity_analyses/sa_epi_end.pdf")
pdf(outfile, width = 9, height = 12)
p <- plot_grid(plotlist = plist, align = "hv", ncol = 3)
title <- ggdraw() + draw_label("Epidemic end", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.04, 1))
dev.off()


# epidemic duration
plist <- list()
sa.epi.duration <- data.frame()
for (i in 1:nrow(nh)) {
  ni <- nh[i, "n"]
  hi <- nh[i, "h"]
  nh.mcm <- subset(mcm.sa, n == ni & h == hi)
  pd <- data.frame(epi.params.etm[c(1:2)], 
                   etm = epi.params.etm$epi.duration, 
                   mcm = nh.mcm$epi.duration)
  lmfit <- lm(etm ~ mcm, data = pd)
  b0 <- coef(lmfit)[1]
  b1 <- coef(lmfit)[2]
  (r2 <- summary(lmfit)$r.squared)
  (pval <- anova(lmfit)$'Pr(>F)'[1])
  
  p <- ggplot() + 
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
    geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
                se = FALSE) + 
    geom_point(data = pd, aes(x = mcm, y = etm), shape = 1) + 
    scale_x_continuous(expand = c(0, 0), limits = c(5, 40), 
                       breaks = seq(5, 40, by = 5)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(5, 40), 
                       breaks = seq(5, 40, by = 5)) + 
    labs(x = "MCM", y = "ETM", 
         title = TeX(glue("$\\mathit{{m}}$ = {ni}, $\\mathit{{h}}$ = {format(hi, nsmall = 1)}"))) + 
    theme_classic() +  
    theme(axis.line = element_blank(), 
          panel.border = element_rect(color = "black", size = 1, fill = NA), 
          plot.title = element_text(hjust = 0.5, face = "bold"))
  p <- ggdraw(p) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
    draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
               0.65, 0.24, size = 12)
  plist[[i]] <- p
  
  res <- data.frame(n = ni, h = hi, b0, b1, r2, pval, row.names = NULL)
  sa.epi.duration <- rbind(sa.epi.duration, res)
}

outfile <- glue("figs/sensitivity_analyses/sa_epi_duration.pdf")
pdf(outfile, width = 9, height = 12)
p <- plot_grid(plotlist = plist, align = "hv", ncol = 3)
title <- ggdraw() + draw_label("Epidemic duration", fontface = 'bold')
plot_grid(title, p, ncol = 1, rel_heights = c(0.04, 1))
dev.off()
