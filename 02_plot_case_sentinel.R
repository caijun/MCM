rm(list = ls())

load("output/Japan_Flu_Sentinel.rda")

library(tidyverse)

# plot weekly number of cases per sentinel for each prefecture
library(glue)
prefs <- unique(pref.flu1$Prefecture)
for (pref in prefs) {
  print(pref)
  outfile <- glue("figs/case_sentinel/{pref}.pdf")
  pdf(file = outfile, width = 6, height = 4)
  pref.flu.sentinel <- subset(pref.flu1, Prefecture == pref)
  p <- ggplot(pref.flu.sentinel, aes(weekending, flu.sentinel)) + 
    geom_line() + 
    geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
    scale_x_date(date_breaks = "1 year") + 
    labs(title = pref, x = "Weekending date", 
         y = "Number of influenza cases per sentinel")
  print(p)
  dev.off()
}

# plot weekly number of influenza cases per sentinel ts for each prefecture in 
# one page using facet, ordered by latitude
pref.attr <- read.csv("data/Japan_Prefecture.csv", stringsAsFactors = FALSE)

pref.attr <- pref.attr %>%
  arrange(-Latitude)

pref.flu1 <- pref.flu1 %>%
  mutate(Prefecture = factor(Prefecture, levels = pref.attr$Prefecture))

outfile <- glue("figs/pref_case_sentinel_ts.pdf")
pdf(file = outfile, width = 12, height = 8)
ggplot(pref.flu1, aes(weekending, flu.sentinel)) + 
  geom_line() +
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  labs(x = "Weekending date", y = "Number of influenza cases per sentinel") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  facet_wrap(~Prefecture, nrow = 8, scales = "free_y") +
  theme_minimal() + 
  theme(panel.grid.minor.y = element_blank(), 
        axis.text.x = element_text(angle = 90))
dev.off()
