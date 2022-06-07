#create multi-panel with cowplot
library(cowplot)

#load in figures

fig1 <- "figures/maps_jn.png"
fig2 <- "figures/timeseriesplots.png"

# create ggplot object that only features the image

fig1gg <- ggdraw() + draw_image(fig1)#, scale = 0.8)
fig2gg <- ggdraw() + draw_image(fig2)#, scale = 0.8)

mergedfig <- plot_grid(fig1gg, fig2gg, ncol = 2,
                       # A negative rel_height shrinks space between elements
                       rel_widths = c(.5,.5),
                       labels = c("A","B"),
                       label_size = 18,
                       label_fontfamily = "sans")
mergedfig

save_plot("figures/cowplot_merged_roostmapsandts.png", mergedfig)#, base_height = 11, base_width = 8.5)
