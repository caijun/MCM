rm(list = ls())

source("R/curvature.R")

load("output/Japan_Pref_Epi_Params.rda")

pref <- "Okinawa"
# pref <- "Tokyo"
s <- "2012/2013"
x <- subset(pref.flu1, season == s & Prefecture == pref)
ec <- data.frame(t = x$weeknum_of_season, y = x$flu.sentinel)
n <- 5
epi.params <- curvature.ec(ec, n = n, h = 5.0)
res <- calc.curvature(ec, n = n, h = 5.0)
# linearly extrapolate to pad data at the front and rear
pad <- (n - 1) / 2
ec1 <- pad.ec(ec, pad)

library(ggplot2)
library(ggforce)
library(latex2exp)
library(glue)

# curvature plot for given week number i
curvature.plot <- function(i, r = 5) {
  j <- i + pad
  t <- c(head(ec1$t, pad), tail(ec1$t, pad))
  y <- c(head(ec1$y, pad), tail(ec1$y, pad))
  padding.points <- data.frame(t, y)
  t <- ec1$t[(j - pad):(j + pad)]
  y <- ec1$y[(j - pad):(j + pad)]
  fitted.points <- data.frame(t, y)
  t <- ec1$t[j]
  y <- ec1$y[j]
  current.point <- data.frame(t, y)
  circle <- res[i, ]
  # the horizontal vector starting from the tangent point and its length is r
  tp <- circle[c("tp_x", "tp_y")]
  hv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                   xend = tp$tp_x + r, yend = tp$tp_y)
  # the tangent vector starting from the tangent point and its length is also r
  tv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                   xend = tp$tp_x + r * cos(circle$theta / 180 * pi),
                   yend = tp$tp_y + r * sin(circle$theta / 180 * pi))
  # the normal vector starting from the tangent point to the fitting circle center 
  # and its length is r
  nv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                   xend = tp$tp_x + r * cos((circle$theta + 90) / 180 * pi),
                   yend = tp$tp_y + r * sin((circle$theta + 90) / 180 * pi))
  p <- ggplot() + 
    geom_point(data = ec, aes(x = t, y = y), shape = 1) + 
    geom_arc(data = circle, aes(x0 = x0, y0 = y0, r = r, start = 0, end = 2*pi), 
             color = "green") + 
    geom_point(data = padding.points, aes(x = t, y = y), shape = 3) + 
    geom_point(data = fitted.points, aes(x = t, y = y), shape = 1, color = "red") + 
    geom_point(data = current.point, aes(x = t, y = y), color = "red") + 
    geom_segment(data = nv, aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "green") + 
    geom_segment(data = tv, aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "blue") + 
    geom_segment(data = hv, aes(x = x, y = y, xend = xend, yend = yend), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "black") + 
    geom_point(data = tp, aes(x = tp_x, y = tp_y), shape = 1, color = "blue") + 
    geom_text(aes(x = 50, y = 30, label = TeX(glue("Current week: {i}"), output = "character")), parse = TRUE) + 
    geom_text(aes(x = 50, y = 28, label = TeX(glue("R = {round(circle$r, 2)}"), output = "character")), parse = TRUE) + 
    geom_text(aes(x = 50, y = 26, label = TeX(glue("$\\theta$ = {round(circle$theta, 2)}$\\degree$"), output = "character")), parse = TRUE) + 
    coord_fixed() + 
    scale_x_continuous(limits = c(-5, 60), breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(limits = c(-5, max(ec$y))) +
    labs(x = "Week number", y = "Number of influenza cases per sentinel", 
         title = glue("{pref}, {s}")) +  
    # theme_bw()
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  print(p)
}

# animation for all curvature plots in the whole season
file.remove(list.files(path = "figs/animation/", full.names = TRUE))
season <- gsub("/", "-", s, fixed = TRUE)

out <- lapply(ec$t, function(i) {
  outfile <- glue("figs/animation/{pref}_{season}_week{formatC(i, width = 2, flag = '0')}.png")
  png(outfile, width = 2400, height = 1800, res = 300)
  curvature.plot(i)
  dev.off()
})

# convert pngs to one gif using ImageMagick
cmd <- glue("convert -delay 100 -density 300 figs/animation/*.png -layers optimize figs/animation/{pref}_{season}.gif")
system(cmd)


# curvature plot at epidemic onset
pdf("figs/curvature_plot_epi_onset.pdf", width = 8, height = 6)
# curvature.plot(i, circle$r)
i <- which(res$tp_x == epi.params$epi.start)
circle <- res[i, ]
r <- circle$r
j <- i + pad
t <- c(head(ec1$t, pad), tail(ec1$t, pad))
y <- c(head(ec1$y, pad), tail(ec1$y, pad))
padding.points <- data.frame(t, y)
t <- ec1$t[(j - pad):(j + pad)]
y <- ec1$y[(j - pad):(j + pad)]
fitted.points <- data.frame(t, y)
t <- ec1$t[j]
y <- ec1$y[j]
current.point <- data.frame(t, y)
circle <- res[i, ]
# the horizontal vector starting from the tangent point and its length is r
tp <- circle[c("tp_x", "tp_y")]
hv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                 xend = tp$tp_x + r, yend = tp$tp_y)
# the tangent vector starting from the tangent point and its length is also r
tv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                 xend = tp$tp_x + r * cos(circle$theta / 180 * pi),
                 yend = tp$tp_y + r * sin(circle$theta / 180 * pi))
# the normal vector starting from the tangent point to the fitting circle center 
# and its length is r
nv <- data.frame(x = tp$tp_x, y = tp$tp_y,
                 xend = tp$tp_x + r * cos((circle$theta + 90) / 180 * pi),
                 yend = tp$tp_y + r * sin((circle$theta + 90) / 180 * pi))
# curve to indicate the directional angle \theta of the tangent vector
arc.df <- data.frame(x = tp$tp_x + 0.3 * r, y = tp$tp_y, 
                     xend = tp$tp_x + 0.3 * r * cos(circle$theta / 180 * pi), 
                     yend = tp$tp_y + 0.3 * r * sin(circle$theta / 180 * pi))

p <- ggplot() + 
  geom_point(data = ec, aes(x = t, y = y), shape = 1) + 
  geom_arc(data = circle, aes(x0 = x0, y0 = y0, r = r, start = 0, end = 2*pi), 
           color = "green") + 
  geom_point(data = padding.points, aes(x = t, y = y), shape = 3) + 
  geom_point(data = fitted.points, aes(x = t, y = y), shape = 1, color = "red") + 
  geom_point(data = current.point, aes(x = t, y = y), color = "red") + 
  geom_segment(data = nv, aes(x = x, y = y, xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.02, "npc")), color = "green") + 
  geom_segment(data = tv, aes(x = x, y = y, xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.02, "npc")), color = "blue") + 
  geom_segment(data = hv, aes(x = x, y = y, xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.02, "npc")), color = "black") + 
  geom_point(data = tp, aes(x = tp_x, y = tp_y), shape = 1, color = "blue") + 
  geom_text(aes(x = nv$x, y = nv$y - 1.1, label = "P")) + 
  geom_text(aes(x = hv$xend + 0.9, y = hv$yend, label = "X")) + 
  geom_text(aes(x = tv$xend + 0.7, y = tv$yend + 0.4, label = "Q")) + 
  geom_text(aes(x = nv$xend - 0.7, y = nv$yend + 0.2, label = "O")) + 
  geom_curve(data = arc.df, aes(x = x, y = y, xend = xend, yend = yend), color = "blue") + 
  geom_text(aes(x = nv$x + 0.2, y = nv$y + 1.2, label = TeX("$\\theta$", output = "character")), parse = TRUE) + 
  
  geom_text(aes(x = 50, y = 30, label = TeX(glue("Current week: {i}"), output = "character")), parse = TRUE) + 
  # geom_text(aes(x = 50, y = 30, label = TeX(glue("Tangent week: {format(round(circle$tp_x, 1), nsmall = 1)}"), output = "character")), parse = TRUE) + 
  geom_text(aes(x = 50, y = 28, label = TeX(glue("R = {round(circle$r, 2)}"), output = "character")), parse = TRUE) + 
  geom_text(aes(x = 50, y = 26, label = TeX(glue("$\\theta$ = {round(circle$theta, 2)}$\\degree$"), output = "character")), parse = TRUE) + 
  coord_fixed() + 
  scale_x_continuous(limits = c(-5, 60), breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(limits = c(-5, max(ec$y))) +
  labs(x = "Week number", y = "Number of influenza cases per sentinel", 
       title = glue("{pref}, {s}")) +  
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p)
dev.off()


# MCM illustration plots
pd <- cbind(ec, res)
p1 <- ggplot(data = pd, aes(t, y)) + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = 5.0, color = "red", linetype = "dashed") + 
  geom_line() + 
  geom_point(shape = 1) + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  labs(x = "", y = "Number of influenza\n cases per sentinel") + 
  theme_classic() + 
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        axis.text.x = element_blank())

p2 <- ggplot(data = pd, aes(t, curvature)) + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_line() + 
  geom_point(shape = 1) + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  labs(x = "", y = "Curvature") + 
  theme_classic() + 
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        axis.text.x = element_blank())

p3 <- ggplot(data = pd, aes(t, theta)) + 
  geom_rect(aes(xmin = 0, xmax = epi.params$epi.peak, ymin = 0, ymax = 90), 
            fill = "gray") + 
  geom_rect(aes(xmin = epi.params$epi.peak, xmax = 53, ymin = 270, ymax = 375), 
            fill = "gray") + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_hline(yintercept = c(90, 180, 270), color = "red", linetype = "dashed") + 
  geom_line() + 
  geom_point(shape = 1) + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 375), breaks = seq(0, 360, by = 90), expand = c(0, 0)) + 
  labs(x = "", y = TeX("$\\theta$")) + 
  theme_classic() + 
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA), 
        axis.text.x = element_blank())

x <- c(which(pd$tp_x == epi.params$epi.start), which(pd$tp_x == epi.params$epi.end))
y <- pd$new.curvature[x]
pt.df <- data.frame(x = x, y = y)
p4 <- ggplot(data = pd, aes(t, new.curvature)) + 
  # geom_point(data = pd, aes(t, curvature), shape = 1) + 
  geom_line() + 
  geom_point(shape = 1) + 
  geom_vline(xintercept = epi.params$epi.peak, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.start, color = "blue", linetype = "dashed") + 
  geom_vline(xintercept = epi.params$epi.end, color = "blue", linetype = "dashed") + 
  geom_point(data = pt.df, aes(x, y), shape = 19, color = "blue") + 
  scale_x_continuous(limits = c(0, 53), breaks = seq(0, 55, by = 5), expand = c(0, 0)) + 
  labs(x = "Week number", y = "Filtered curvature") + 
  theme_classic() + 
  theme(axis.line = element_blank(), 
        panel.border = element_rect(color = "black", size = 1, fill = NA))

library(cowplot)
p1234 <- cowplot::plot_grid(p1, NULL, p2, NULL, p3, NULL, p4, 
                            rel_heights = c(1, -0.12, 1, -0.12, 1, -0.12, 1), 
                            labels = c("(a)", "", "(b)", "", "(c)", "", "(d)"), 
                            align = "v", ncol = 1, 
               label_x = 0.11, label_y = 0.96)
# now add the title
title <- ggdraw() + draw_label(glue("{pref}, {s}"), fontface = 'bold')
p <- plot_grid(title, p1234, ncol = 1, rel_heights = c(0.05, 1)) # rel_heights values control title margins
pdf("figs/MCM_illustration.pdf", width = 6, height = 6)
print(p)
dev.off()