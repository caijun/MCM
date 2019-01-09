rm(list = ls())

load("output/Japan_Flu_Sentinel.rda")
load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)
library(glue)

epi.params.etm <- epi.params.etm %>% 
  dplyr::filter(Prefecture != "Japan")

epi.params.srm <- epi.params.srm %>% 
  dplyr::filter(Prefecture != "Japan")

epi.params.mcm <- epi.params.mcm %>% 
  dplyr::filter(Prefecture != "Japan")

# NA epidemic parameters estimated by ETM --------------------------------
na.season.etm <- epi.params.etm %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

epi.params.etm %>% 
  dplyr::filter(is.na(epi.duration))

# NA epidemic parameters estimated by SRM ----------------------------------
na.season.srm <- epi.params.srm %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

# NA epidemic parameters estimated by MCM --------------------------------------
na.season.mcm <- epi.params.mcm %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

# summarise of epidemic parameters estimated by ETM ----------------------
summary.season.etm <- epi.params.etm %>% 
  group_by(season) %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

summary.etm <- epi.params.etm %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

# summarise of epidemic parameters estimated by SRM ------------------------
summary.season.srm <- epi.params.srm %>% 
  group_by(season) %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

summary.srm <- epi.params.srm %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

# summarise of epidemic parameters estimated by MCM ----------------------------
summary.season.mcm <- epi.params.mcm %>% 
  group_by(season) %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

summary.mcm <- epi.params.mcm %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   max.epi.start = round(max(epi.start, na.rm = TRUE), 1), 
                   min.epi.start = round(min(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2), 
                   mean.epi.duration = round(mean(epi.duration, na.rm = TRUE), 1), 
                   sd.epi.duration = round(as.numeric(sd(epi.duration, na.rm = TRUE)), 1)
  )

# summarise of epidemic parameters estimated by MCM ----------------------------
IQR.season.mcm <- epi.params.mcm %>% 
  group_by(season) %>% 
  dplyr::summarise(mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   IQR.epi.start.num = round(IQR(epi.start.num, na.rm = TRUE), 2), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   IQR.epi.end.num = round(IQR(epi.end.num, na.rm = TRUE), 2)
  )


# boxplot of epidemic parameters -----------------------------------------------
epi.params.etm1 <- epi.params.etm %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "ETM")

epi.params.srm1 <- epi.params.srm %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "SRM")

epi.params.mcm1 <- epi.params.mcm %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "MCM")

epi.params <- rbind(epi.params.etm1, epi.params.srm1, 
                    epi.params.mcm1) %>% 
  mutate(method = factor(method, levels = c("ETM", "MCM", "SRM"))) %>% 
  mutate(season = gsub("20", "", season))

library(ggsci)
library(cowplot)
p1 <- ggplot(epi.params, aes(season, epi.start, fill = method)) + 
  geom_boxplot(position = position_dodge(0.8), width = 0.6) + 
  labs(x = "Season", y = "Epidemic onset (weeks)") + 
  scale_fill_npg(name = "Method") + 
  theme_classic()

p2 <- ggplot(epi.params, aes(season, epi.end, fill = method)) + 
  geom_boxplot(position = position_dodge(0.8), width = 0.6) + 
  labs(x = "Season", y = "Epidemic end (weeks)") + 
  scale_fill_npg() + 
  theme_classic() + 
  theme(legend.position = "none")

p3 <- ggplot(epi.params, aes(season, epi.duration, fill = method)) + 
  geom_boxplot(position = position_dodge(0.8), width = 0.6) + 
  labs(x = "Season", y = "Epidemic duration (weeks)") + 
  scale_fill_npg() + 
  theme_classic() + 
  theme(legend.position = "none")

p4 <- ggplot(epi.params, aes(season, epi.start.num, fill = method)) + 
  geom_boxplot(position = position_dodge(0.8), width = 0.6) + 
  labs(x = "Season", y = "Epidemic onset intensity") + 
  scale_fill_npg() + 
  theme_classic() + 
  theme(legend.position = "none")

p5 <- ggplot(epi.params, aes(season, epi.end.num, fill = method)) + 
  geom_boxplot(position = position_dodge(0.8), width = 0.6) + 
  labs(x = "Season", y = "Epidemic ending intensity") + 
  scale_fill_npg() + 
  theme_classic() + 
  theme(legend.position = "none")

# arrange the three plots in a single row
prow1 <- plot_grid(p1 + theme(legend.position = "none"), p2, p3, nrow = 1)
prow1

# arrange the last two plots in a single row
prow2 <- plot_grid(p4, p5, NULL, nrow = 1)
prow2

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend <- get_legend(p1)

pdf("figs/cmp_epi_params_boxplot.pdf", width = 9, height = 6)
p <- plot_grid(prow1, prow2, align = "hv", nrow = 2)
ggdraw() + 
  draw_plot(p) + 
  draw_plot(legend, x = 0.25, y = -0.25)
dev.off()


library(latex2exp)
# 1:1 plot of epidemic onset
pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.start, 
                 srm = epi.params.srm$epi.start)
lmfit <- lm(etm ~ srm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

ss <- subset(epi.params.etm, is.na(epi.start))
cor.test(epi.params.srm$epi.start, epi.params.etm$epi.start, 
         use = "complete.obs")

p1 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = srm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = srm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  labs(x = "SRM", y = "ETM", 
       title = "Epidemic onset") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p1 <- ggdraw(p1) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.45, 0.8, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.45, 0.74, size = 12)


pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.start, 
                 mcm = epi.params.mcm$epi.start)
lmfit <- lm(etm ~ mcm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.mcm$epi.start, epi.params.etm$epi.start, 
         use = "complete.obs")

p2 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = mcm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  labs(x = "MCM", y = "ETM", 
       title = "Epidemic onset") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p2 <- ggdraw(p2) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.45, 0.8, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.45, 0.74, size = 12)

# 1:1 plot of epidemic end
pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.end, 
                 srm = epi.params.srm$epi.end)
lmfit <- lm(etm ~ srm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.srm$epi.end, epi.params.etm$epi.end, 
         use = "complete.obs")

p3 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = srm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = srm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(25, 55),
                     breaks = seq(25, 55, by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(25, 55),
                     breaks = seq(25, 55, by = 5)) +
  labs(x = "SRM", y = "ETM", 
       title = "Epidemic end") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p3 <- ggdraw(p3) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.30, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)

pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.end, 
                 mcm = epi.params.mcm$epi.end)
lmfit <- lm(etm ~ mcm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.mcm$epi.end, epi.params.etm$epi.end, 
         use = "complete.obs")

p4 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = mcm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(25, 55), 
                     breaks = seq(25, 55, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(25, 55), 
                     breaks = seq(25, 55, by = 5)) + 
  labs(x = "MCM", y = "ETM", 
       title = "Epidemic end") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p4 <- ggdraw(p4) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)

# 1:1 plot of epidemic duration
pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.duration, 
                 srm = epi.params.srm$epi.duration)
lmfit <- lm(etm ~ srm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.srm$epi.duration, epi.params.etm$epi.duration, 
         use = "complete.obs")

p5 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = srm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = srm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(5, 40),
                     breaks = seq(5, 40, by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(5, 40),
                     breaks = seq(5, 40, by = 5)) +
  labs(x = "SRM", y = "ETM", 
       title = "Epidemic duration") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p5 <- ggdraw(p5) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} - {format(abs(round(b1, 2)), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ < 0.01, $$\\mathit{{p}}$ = {format(round(pval, 2), nsmall = 2)}")), 
             0.65, 0.24, size = 12)

pd <- data.frame(epi.params.etm[c(1:2)], 
                 etm = epi.params.etm$epi.duration, 
                 mcm = epi.params.mcm$epi.duration)
lmfit <- lm(etm ~ mcm, data = pd)
summary(lmfit)
coef(lmfit)
b0 <- coef(lmfit)[1]
b1 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.mcm$epi.duration, epi.params.etm$epi.duration, 
         use = "complete.obs")

p6 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = mcm, y = etm), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = mcm, y = etm)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(5, 40), 
                     breaks = seq(5, 40, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(5, 40), 
                     breaks = seq(5, 40, by = 5)) + 
  labs(x = "MCM", y = "ETM", 
       title = "Epidemic duration") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p6 <- ggdraw(p6) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b0, 2), nsmall = 2)} + {format(round(b1, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)


pdf("figs/cmp_epi_params_timings_regression.pdf", width = 9, height = 6)
p12 <- plot_grid(p1, p2, align = "hv", ncol = 1)
p34 <- plot_grid(p3, p4, align = "hv", ncol = 1)
p56 <- plot_grid(p5, p6, align = "hv", ncol = 1)
plot_grid(p12, p34, p56, align = "hv", ncol = 3)
dev.off()
