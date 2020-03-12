rm(list = ls())

load("output/Japan_Flu_Sentinel.rda")
load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)
library(glue)
library(showtext)

showtext_auto()
font_add('SimSun', regular = '~/Library/Fonts/SimSun.ttf')

# plot ts of weekly number of cases per sentinel with epidemic parameters for 
# each prefecture
prefs <- unique(pref.flu1$Prefecture)
for (pref in prefs) {
  print(pref)
  etm <- subset(epi.params.etm, Prefecture == pref)
  srm <- subset(epi.params.srm, Prefecture == pref)
  mcm <- subset(epi.params.mcm, Prefecture == pref)
  
  outfile <- glue("figs/case_sentinel_with_epi_params/{pref}.pdf")
  pdf(file = outfile, width = 8, height = 4)
  pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
  p <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
    geom_line() + 
    geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
    geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
               linetype = "dashed") + 
    geom_vline(xintercept = etm$epi.start.date, color = "green3") +
    geom_vline(xintercept = srm$epi.start.date, color = "blue3") +
    geom_vline(xintercept = mcm$epi.start.date, color = "yellow3") +
    geom_vline(xintercept = etm$epi.end.date, color = "green3") +
    geom_vline(xintercept = srm$epi.end.date, color = "blue3") +
    geom_vline(xintercept = mcm$epi.end.date, color = "yellow3") +
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
# epidemic onset
pref <- "Japan"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p1 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.start.date, color = "green3") + 
  geom_vline(xintercept = srm$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = mcm$epi.start.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(a)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = 0, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p1)

pref <- "Hokkaido"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p2 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.start.date, color = "green3") + 
  geom_vline(xintercept = srm$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = mcm$epi.start.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(b)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p2)

pref <- "Tokyo"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p3 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.start.date, color = "green3") + 
  geom_vline(xintercept = srm$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = mcm$epi.start.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(c)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p3)

pref <- "Okinawa"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p4 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.start.date, color = "green3") + 
  geom_vline(xintercept = srm$epi.start.date, color = "blue3") + 
  geom_vline(xintercept = mcm$epi.start.date, color = "yellow3") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  # labs(title = "", x = "Weekending date", 
  #      y = "Number of ILI cases per sentinel", tag = "(d)") + 
  labs(title = "", x = "周结束日期", 
       y = "Number of ILI cases per sentinel", tag = "(d)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(family = "SimSun", size = 12), 
        axis.title.y = element_blank(), 
        plot.tag.position = c(0.96, 0.8), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = 0, l = 0, r = 0.25, unit = "cm"))
print(p4)

library(patchwork)
pcom <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
pgrob <- patchworkGrob(pcom)

library(cowplot)
# add common y axis label
ylab <- ggdraw() + 
  draw_label("平均每个哨点报告病例数", angle = 90, fontfamily = "SimSun", size = 12)
  # draw_label("Number of ILI cases per sentinel", angle = 90)

pdf("figs/overlap_epi_onset_with_ts.pdf", width = 6, height = 7)
p <- plot_grid(ylab, NULL, pgrob, nrow = 1, rel_widths = c(0.1, -0.03, 1.5))
print(p)
dev.off()

system("pdfcrop figs/overlap_epi_onset_with_ts.pdf figs/overlap_epi_onset_with_ts.pdf")


# epidemic end
pref <- "Japan"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p1 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.end.date, color = "green3") +
  geom_vline(xintercept = srm$epi.end.date, color = "blue3") +
  geom_vline(xintercept = mcm$epi.end.date, color = "yellow3") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(a)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = 0, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p1)

pref <- "Hokkaido"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p2 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.end.date, color = "green3") +
  geom_vline(xintercept = srm$epi.end.date, color = "blue3") +
  geom_vline(xintercept = mcm$epi.end.date, color = "yellow3") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(b)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p2)

pref <- "Tokyo"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p3 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.end.date, color = "green3") +
  geom_vline(xintercept = srm$epi.end.date, color = "blue3") +
  geom_vline(xintercept = mcm$epi.end.date, color = "yellow3") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "", x = "Weekending date", 
       y = "Number of ILI cases per sentinel", tag = "(c)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag.position = c(0.96, 0.76), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = -0.1, l = 0, r = 0.25, unit = "cm"))
print(p3)

pref <- "Okinawa"
etm <- subset(epi.params.etm, Prefecture == pref)
srm <- subset(epi.params.srm, Prefecture == pref)
mcm <- subset(epi.params.mcm, Prefecture == pref)

pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
p4 <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
  geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = srm$pre.season.ending, color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = etm$epi.end.date, color = "green3") +
  geom_vline(xintercept = srm$epi.end.date, color = "blue3") +
  geom_vline(xintercept = mcm$epi.end.date, color = "yellow3") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  # labs(title = "", x = "Weekending date", 
  #      y = "Number of ILI cases per sentinel", tag = "(d)") + 
  labs(title = "", x = "周结束日期", 
       y = "Number of ILI cases per sentinel", tag = "(d)") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(family = "SimSun", size = 12), 
        axis.title.y = element_blank(), 
        plot.tag.position = c(0.96, 0.8), 
        plot.tag = element_text(face = "bold"), 
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        plot.margin = margin(t = -0.1, b = 0, l = 0, r = 0.25, unit = "cm"))
print(p4)

library(patchwork)
pcom <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
pgrob <- patchworkGrob(pcom)

library(cowplot)
# add common y axis label
ylab <- ggdraw() + 
  draw_label("平均每个哨点报告病例数", angle = 90, fontfamily = "SimSun", size = 12)
  # draw_label("Number of ILI cases per sentinel", angle = 90)

pdf("figs/overlap_epi_end_with_ts.pdf", width = 6, height = 7)
p <- plot_grid(ylab, NULL, pgrob, nrow = 1, rel_widths = c(0.1, -0.03, 1.5))
print(p)
dev.off()

system("pdfcrop figs/overlap_epi_end_with_ts.pdf figs/overlap_epi_end_with_ts.pdf")