library(devtools)
install_github("williazo/ggplot.spaghetti")
library(ggplot.spaghetti)
library(nlme)
data("Orthodont")
Orthodont = data.frame(Orthodont, Race = rep(ifelse(rbinom(n = 27, size = 1, prob = 0.5)==0, "White", "Non-White"), each = 4))
attach(Orthodont)

#specifying just group
ortho_plot_group <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                                     alpha = 0.3, group = Sex, method = "lm")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Gender", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Gender", values = c("dashed", "solid"))
ortho_plot_group
ggsave(ortho_plot_group, file = "ortho_plot_group.jpg", dpi = 600)

#specifying just wrap
ortho_plot_wrap <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                                    alpha = 0.3, wrap = Race, method = "loess", scales = "free")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Race", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Race", values = c("dashed", "solid"))
ortho_plot_wrap
ggsave(ortho_plot_wrap, file = "ortho_plot_wrap.jpg", dpi = 600)

#specifying both group and wrap
ortho_plot <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                               alpha = 0.3, group = Sex, wrap = Race,
                               method = "glm")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Gender", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Race", values = c("dashed", "solid"))
ortho_plot
ggsave(ortho_plot, file = "ortho_plot.jpg", dpi = 600)
