rm(list = ls())

load("output/Japan_Pref_Epi_Params.rda")

library(tidyverse)

# Now we have the conclusion that MCM outperforms SRM for estimating epidemic onset.
# Application of MCM
# Prefecture-specific epidemic thresholds: epidemic onset, epidemic end and 
# corresponding number of influenza cases per sentinel averaged across 6 seasons 
# for each prefecture
epi.params <- epi.params.mcm %>% 
  dplyr::filter(Prefecture != "Japan")

mean.epi.params0 <- epi.params %>% 
  group_by(Prefecture) %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 2), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 2), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 3), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 2)
  )

mean.epi.params <- epi.params %>% 
  group_by(Prefecture) %>% 
  dplyr::summarise(mean.epi.start = round(mean(epi.start, na.rm = TRUE), 1), 
                   sd.epi.start = round(sd(epi.start, na.rm = TRUE), 1), 
                   mean.epi.start.num = round(mean(epi.start.num, na.rm = TRUE), 1), 
                   sd.epi.start.num = round(sd(epi.start, na.rm = TRUE), 1), 
                   mean.epi.end = round(mean(epi.end, na.rm = TRUE), 1), 
                   sd.epi.end = round(sd(epi.end, na.rm = TRUE), 1), 
                   mean.epi.end.num = round(mean(epi.end.num, na.rm = TRUE), 1), 
                   sd.epi.end.num = round(sd(epi.end.num, na.rm = TRUE), 1)
  )

sum(mean.epi.params$mean.epi.start.num < 1.0)
sum(mean.epi.params$mean.epi.end.num > 1.0)

# significant correlation between epidemic onset threshold and epidemic ending threshold
# If h = 3.0 or 3.5, the Pearson's correlation would become insignifcant.
cor.test(mean.epi.params$mean.epi.start.num, mean.epi.params$mean.epi.end.num, 
         method = "pearson")

# write prefecture-specific mean(sd) of epidemic onset intensity and epidemic 
# end intensity into csv
# order prefecture by increasing id
pref.attr <- read.csv("data/Japan_Prefecture.csv", stringsAsFactors = FALSE)
res <- mean.epi.params %>% 
  select(Prefecture, mean.epi.start.num, sd.epi.start.num, mean.epi.end.num, 
         sd.epi.end.num) %>% 
  mutate(mean.sd.epi.start.num = paste0(format(mean.epi.start.num, nsmall = 1), 
                                        " (", format(sd.epi.start.num, nsmall = 1), ")"), 
         mean.sd.epi.end.num = paste0(format(mean.epi.end.num, nsmall = 1), 
                                      " (", format(sd.epi.end.num, nsmall = 1), ")")) %>%
  left_join(pref.attr[c("ID", "Prefecture")], by = "Prefecture") %>%
  select(ID, Prefecture, mean.sd.epi.start.num, mean.sd.epi.end.num) %>%
  arrange(ID)
write.csv(res, file = "output/pref_epi_thresholds.csv", row.names = F, 
          quote = F)

# order prefectures by increasing mean epidemic onset intensity
mean.epi.params0 <- mean.epi.params0 %>%
  arrange(mean.epi.start.num)

mean.epi.params <- mean.epi.params %>% 
  mutate(Prefecture = factor(Prefecture, 
                             levels = rev(as.character(mean.epi.params0$Prefecture))))

p1 <- ggplot(data = mean.epi.params) + 
  geom_bar(aes(x = Prefecture, y = mean.epi.start.num, fill = mean.epi.start), 
           stat = 'identity', width = 1, color = "gray30") + 
  geom_text(aes(x = Prefecture, y = mean.epi.start.num, 
                label = format(mean.epi.start.num, nsmall = 1), hjust = 1.1), color = "gray50", 
            position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  scale_x_discrete(expand = expand_scale(mult = c(0, 0.02))) + 
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.01)), 
                     breaks = seq(0, 1.5, by = 0.5)) + 
  scale_fill_gradient(limits = c(12, 17), breaks = seq(12, 17, by = 1),
                      low = "white", high = "red",
                      guide = guide_colorbar(title = "Mean epidemic onset\n (weeks)",
                                             title.position = "top",
                                             title.hjust = 0.5,
                                             barwidth = 8, 
                                             nbin = 1000, 
                                             ticks.colour = "black",
                                             frame.colour = "black",
                                             direction = "horizontal")) +
  labs(y = "Epidemic onset threshold") + 
  coord_flip() + 
  theme_classic(base_size = 12) + 
  theme(legend.position = c(0.7, 0.8))

# order prefectures by increasing mean epidemic ending intensity
mean.epi.params0 <- mean.epi.params0 %>%
  arrange(mean.epi.end.num)

mean.epi.params <- mean.epi.params %>% 
  mutate(Prefecture = factor(Prefecture, 
                             levels = rev(as.character(mean.epi.params0$Prefecture))))

p2 <- ggplot(data = mean.epi.params) + 
  geom_bar(aes(x =  Prefecture, y = mean.epi.end.num, fill = mean.epi.end), 
           stat = 'identity', width = 1, color = "gray30") + 
  geom_text(aes(x = Prefecture, y = mean.epi.end.num, 
                label = format(mean.epi.end.num, nsmall = 1), hjust = 1.1), color = "gray50", 
            position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  scale_x_discrete(expand = expand_scale(mult = c(0, 0.02))) + 
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.01)), 
                     breaks = seq(0, 3, by = 0.5)) + 
  scale_fill_gradient(limits = c(31, 43), breaks = seq(31, 43, by = 2), 
                      low = "white", high = "red", 
                      guide = guide_colorbar(title = "Mean epidemic end\n (weeks)",
                                             title.position = "top", 
                                             title.hjust = 0.5, 
                                             barwidth = 8, 
                                             nbin = 1000, 
                                             ticks.colour = "black",
                                             frame.colour = "black", 
                                             direction = "horizontal")) + 
  labs(y = "Epidemic ending threshold") + 
  coord_flip() + 
  theme_classic(base_size = 12) + 
  theme(legend.position = c(0.7, 0.8))

pdf(file = "figs/pref_epi_thresholds.pdf", width = 10, height = 7)
library(cowplot)
plot_grid(p1, p2, ncol = 2, labels = c("(a)", "(b)"), label_x = 0.9, label_y = 0.96)
dev.off()