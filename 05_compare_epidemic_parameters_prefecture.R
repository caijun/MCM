rm(list = ls())

load("output/Japan_Flu_Sentinel.rda")
load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)
library(glue)

epi.params.threshold <- epi.params.threshold %>% 
  dplyr::filter(Prefecture != "Japan")

epi.params.segment <- epi.params.segment %>% 
  dplyr::filter(Prefecture != "Japan")

epi.params.curvature <- epi.params.curvature %>% 
  dplyr::filter(Prefecture != "Japan")

# NA epidemic parameters estimated by threshold --------------------------------
na.season.threshold <- epi.params.threshold %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

epi.params.threshold %>% 
  dplyr::filter(is.na(epi.duration))

# NA epidemic parameters estimated by segment ----------------------------------
na.season.segment <- epi.params.segment %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

# NA epidemic parameters estimated by MCM --------------------------------------
na.season.curvature <- epi.params.curvature %>% 
  group_by(season) %>% 
  dplyr::summarise(epi.start = sum(is.na(epi.start)), 
                   epi.end = sum(is.na(epi.end)), 
                   epi.duration = sum(is.na(epi.duration)), 
                   epi.start.num = sum(is.na(epi.start.num)), 
                   epi.end.num = sum(is.na(epi.end.num))
  )

# summarise of epidemic parameters estimated by threshold ----------------------
summary.season.threshold <- epi.params.threshold %>% 
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
write.csv(summary.season.threshold, file = "output/summary_season_threshold.csv", 
          quote = F, row.names = F)

summary.threshold <- epi.params.threshold %>% 
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

# summarise of epidemic parameters estimated by segment ------------------------
summary.season.segment <- epi.params.segment %>% 
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
write.csv(summary.season.segment, file = "output/summary_season_segment.csv", 
          quote = F, row.names = F)

summary.segment <- epi.params.segment %>% 
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
summary.season.curvature <- epi.params.curvature %>% 
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
write.csv(summary.season.curvature, file = "output/summary_season_curvature.csv", 
          quote = F, row.names = F)

summary.curvature <- epi.params.curvature %>% 
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
IQR.season.curvature <- epi.params.curvature %>% 
  group_by(season) %>% 
  dplyr::summarise(mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   IQR.epi.start.num = round(IQR(epi.start.num, na.rm = TRUE), 2), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 2), 
                   IQR.epi.end.num = round(IQR(epi.end.num, na.rm = TRUE), 2)
  )


# boxplot of epidemic parameters -----------------------------------------------
epi.params.threshold1 <- epi.params.threshold %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "ETM")

epi.params.segment1 <- epi.params.segment %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "SRM")

epi.params.curvature1 <- epi.params.curvature %>% 
  select(Prefecture:epi.duration.num) %>% 
  mutate(method = "MCM")

epi.params <- rbind(epi.params.threshold1, epi.params.segment1, 
                    epi.params.curvature1) %>% 
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
  labs(x = "Season", y = "Epidemic ending (weeks)") + 
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

pdf("figs/compare_epi_params_boxplot.pdf", width = 9, height = 6)
p <- plot_grid(prow1, prow2, align = "hv", nrow = 2)
ggdraw() + 
  draw_plot(p) + 
  draw_plot(legend, x = 0.25, y = -0.25)
dev.off()


library(latex2exp)
# 1:1 plot of epidemic onset
pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.start, 
                 segment = epi.params.segment$epi.start)
lmfit <- lm(threshold ~ segment, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

ss <- subset(epi.params.threshold, is.na(epi.start))
cor.test(epi.params.segment$epi.start, epi.params.threshold$epi.start, 
         use = "complete.obs")

p1 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = segment, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = segment, y = threshold)) + 
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
p1 <- ggdraw(p1) + draw_label(TeX(glue("$\\mathit{{y}}$ = {round(b1, 2)} + {format(round(b2, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.45, 0.8, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.45, 0.74, size = 12)


pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.start, 
                 curvature = epi.params.curvature$epi.start)
lmfit <- lm(threshold ~ curvature, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.curvature$epi.start, epi.params.threshold$epi.start, 
         use = "complete.obs")

p2 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = curvature, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = curvature, y = threshold)) + 
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
p2 <- ggdraw(p2) + draw_label(TeX(glue("$\\mathit{{y}}$ = {round(b1, 2)} + {round(b2, 2)}$\\mathit{{x}}$")), 0.45, 0.8, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.45, 0.74, size = 12)

# 1:1 plot of epidemic end
pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.end, 
                 segment = epi.params.segment$epi.end)
lmfit <- lm(threshold ~ segment, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.segment$epi.end, epi.params.threshold$epi.end, 
         use = "complete.obs")

p3 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = segment, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = segment, y = threshold)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(25, 55),
                     breaks = seq(25, 55, by = 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(25, 55),
                     breaks = seq(25, 55, by = 5)) +
  labs(x = "SRM", y = "ETM", 
       title = "Epidemic ending") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p3 <- ggdraw(p3) + draw_label(TeX(glue("$\\mathit{{y}}$ = {round(b1, 2)} + {round(b2, 2)}$\\mathit{{x}}$")), 0.65, 0.30, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)

pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.end, 
                 curvature = epi.params.curvature$epi.end)
lmfit <- lm(threshold ~ curvature, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.curvature$epi.end, epi.params.threshold$epi.end, 
         use = "complete.obs")

p4 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = curvature, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = curvature, y = threshold)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(25, 55), 
                     breaks = seq(25, 55, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(25, 55), 
                     breaks = seq(25, 55, by = 5)) + 
  labs(x = "MCM", y = "ETM", 
       title = "Epidemic ending") + 
  theme_classic() +  
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.title = element_text(hjust = 0.5, face = "bold"))
p4 <- ggdraw(p4) + draw_label(TeX(glue("$\\mathit{{y}}$ = {round(b1, 2)} + {round(b2, 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)

# 1:1 plot of epidemic duration
pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.duration, 
                 segment = epi.params.segment$epi.duration)
lmfit <- lm(threshold ~ segment, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.segment$epi.duration, epi.params.threshold$epi.duration, 
         use = "complete.obs")

p5 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = segment, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = segment, y = threshold)) + 
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
p5 <- ggdraw(p5) + draw_label(TeX(glue("$\\mathit{{y}}$ = {format(round(b1, 2), nsmall = 2)} - {abs(round(b2, 2))}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ < 0.01, $$\\mathit{{p}}$ = {round(pval, 2)}")), 
             0.65, 0.24, size = 12)

pd <- data.frame(epi.params.threshold[c(1:2)], 
                 threshold = epi.params.threshold$epi.duration, 
                 curvature = epi.params.curvature$epi.duration)
lmfit <- lm(threshold ~ curvature, data = pd)
summary(lmfit)
coef(lmfit)
b1 <- coef(lmfit)[1]
b2 <- coef(lmfit)[2]
(r2 <- summary(lmfit)$r.squared)
(pval <- anova(lmfit)$'Pr(>F)'[1])

cor.test(epi.params.curvature$epi.duration, epi.params.threshold$epi.duration, 
         use = "complete.obs")

p6 <- ggplot() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_smooth(data = pd, aes(x = curvature, y = threshold), method = "lm", 
              se = FALSE) + 
  geom_point(data = pd, aes(x = curvature, y = threshold)) + 
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
p6 <- ggdraw(p6) + draw_label(TeX(glue("$\\mathit{{y}}$ = {round(b1, 2)} + {format(round(b2, 2), nsmall = 2)}$\\mathit{{x}}$")), 0.65, 0.3, size = 12) + 
  draw_label(TeX(glue("$R^2$ = {format(round(r2, 2), nsmall = 2)}, $$\\mathit{{p}}$ < 0.001")), 
             0.65, 0.24, size = 12)


pdf("figs/compare_epi_params_timings_regression.pdf", width = 9, height = 6)
p12 <- plot_grid(p1, p2, align = "hv", ncol = 1)
p34 <- plot_grid(p3, p4, align = "hv", ncol = 1)
p56 <- plot_grid(p5, p6, align = "hv", ncol = 1)
plot_grid(p12, p34, p56, align = "hv", ncol = 3)
dev.off()
