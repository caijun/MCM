rm(list = ls())

load("output/Japan_Flu_Sentinel.rda")
load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)
library(glue)

# plot ts of weekly number of cases per sentinel with epidemic parameters for 
# each prefecture
prefs <- unique(pref.flu1$Prefecture)
for (pref in prefs) {
  print(pref)
  threshold <- subset(epi.params.threshold, Prefecture == pref)
  segment <- subset(epi.params.segment, Prefecture == pref)
  curvature <- subset(epi.params.curvature, Prefecture == pref)
  
  outfile <- glue("figs/case_sentinel_with_epi_params/{pref}.pdf")
  pdf(file = outfile, width = 8, height = 4)
  pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
  p <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
    geom_line() + 
    geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
    geom_vline(xintercept = segment$pre.season.ending, color = "gray", 
               linetype = "dashed") + 
    geom_vline(xintercept = threshold$epi.start.date, color = "green3") + 
    geom_vline(xintercept = segment$epi.start.date, color = "blue3") + 
    geom_vline(xintercept = curvature$epi.start.date, color = "yellow3") + 
    geom_vline(xintercept = threshold$epi.end.date, color = "green3") + 
    geom_vline(xintercept = segment$epi.end.date, color = "blue3") + 
    geom_vline(xintercept = curvature$epi.end.date, color = "yellow3") + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    labs(title = pref, x = "Weekending date", 
         y = "Number of influenza cases per sentinel") + 
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  print(p)
  dev.off()
}

# Japan and 3 representative prefectures: Hokkaido (northernmost), Tokyo (middle), 
# and Okinawa (southernmost)
pref <- "Japan"
threshold <- subset(epi.params.threshold, Prefecture == pref)
segment <- subset(epi.params.segment, Prefecture == pref)
curvature <- subset(epi.params.curvature, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p1 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = segment$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = threshold$epi.start.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.start.date, color = "yellow3") + 
  geom_vline(xintercept = threshold$epi.end.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.end.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.end.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = pref, x = "Weekending date", 
       y = "Number of ILI cases per sentinel") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p1)

pref <- "Hokkaido"
threshold <- subset(epi.params.threshold, Prefecture == pref)
segment <- subset(epi.params.segment, Prefecture == pref)
curvature <- subset(epi.params.curvature, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p2 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = segment$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = threshold$epi.start.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.start.date, color = "yellow3") + 
  geom_vline(xintercept = threshold$epi.end.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.end.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.end.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = pref, x = "Weekending date", 
       y = "Number of ILI cases per sentinel") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p2)

pref <- "Tokyo"
threshold <- subset(epi.params.threshold, Prefecture == pref)
segment <- subset(epi.params.segment, Prefecture == pref)
curvature <- subset(epi.params.curvature, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p3 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = segment$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = threshold$epi.start.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.start.date, color = "yellow3") + 
  geom_vline(xintercept = threshold$epi.end.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.end.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.end.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = pref, x = "Weekending date", 
       y = "Number of ILI cases per sentinel") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p3)

pref <- "Okinawa"
threshold <- subset(epi.params.threshold, Prefecture == pref)
segment <- subset(epi.params.segment, Prefecture == pref)
curvature <- subset(epi.params.curvature, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p4 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = segment$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = threshold$epi.start.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.start.date, color = "yellow3") + 
  geom_vline(xintercept = threshold$epi.end.date, color = "green3") + 
  geom_vline(xintercept = segment$epi.end.date, color = "blue3") + 
  geom_vline(xintercept = curvature$epi.end.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = pref, x = "Weekending date", 
       y = "Number of ILI cases per sentinel") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p4)

library(cowplot)
pdf("figs/overlap_epi_params_timings_with_ts.pdf", width = 8, height = 6)
p <- plot_grid(p1, p2, p3, p4, align = "hv", ncol = 2)
print(p)
dev.off()
