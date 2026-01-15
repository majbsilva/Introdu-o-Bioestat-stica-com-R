library(patchwork)

plot <- plot + theme(
  axis.title = element_text(size = 24),
  axis.text = element_text(size = 20, color = 'black'),
  plot.subtitle = element_text(size = 12)
)

plot1 <- plot1 + theme(
  axis.title = element_text(size = 24),
  axis.text = element_text(size = 20, color = 'black'),
  plot.subtitle = element_text(size = 12)
)

plot <- plot + labs(tag = "(A)") + theme(plot.tag = element_text(size = 20, face = 'bold'))

plot1 <- plot1 + labs(tag = "(B)") + theme(plot.tag = element_text(size = 20, face = 'bold'))

combined_figure <- plot + plot1

ggsave(
  filename = 'Módulo 5/wound_healing.png',
  plot = combined_figure,
  height = 9,
  width = 14,
  dpi = 300
)

