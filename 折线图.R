library(ggplot2)
library(ggsci)
library(svglite)
library(Rmisc)
library(gridExtra)
library(ggthemes)
library(patchwork)


##Cell_number
df_Cell_number <- read.csv("Cell_number.CSV")

# 创建绘图
p_Cell_number <- ggplot(df_Cell_number, aes(x = Time, y = `Cell_number`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `Cell_number` - std, ymax = `Cell_number` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab("Cell number") +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.05, 0.9),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,2,4,6,8,10,12,14,17)) +# 调整 x 轴间隔 
  #facet_wrap(~ Group, ncol = 2) +
  coord_cartesian(clip = "off")
# 显示绘图
print(p_Cell_number) 
ggsave("p_Cell_number.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_Cell_number.svg", width = 5, height = 4, units = "in", dpi = 300)

##FVFM
df_FVFM <- read.csv("FVFM.CSV")

# 创建绘图
p_FVFM <- ggplot(df_FVFM, aes(x = Time, y = `FVFM`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `FVFM` - std, ymax = `FVFM` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab(expression(F[v]/F[m])) +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.05, 0.7),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,2,4,6,8,10,12,14,17)) +# 调整 x 轴间隔 
  #facet_wrap(~ Group, ncol = 2) +
  coord_cartesian(clip = "off")
# 显示绘图
print(p_FVFM) 
ggsave("p_FVFM.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_FVFM.svg", width = 5, height = 4, units = "in", dpi = 300)

##OD680
df_OD680 <- read.csv("OD680.CSV")

# 创建绘图
p_OD680 <- ggplot(df_OD680, aes(x = Time, y = `OD680`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `OD680` - std, ymax = `OD680` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab("OD680") +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.05, 0.9),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,2,4,6,8,10,12,14,17)) +# 调整 x 轴间隔 
  #facet_wrap(~ Group, ncol = 2) +
  coord_cartesian(clip = "off")
# 显示绘图
print(p_OD680) 
ggsave("p_OD680.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_OD680.svg", width = 5, height = 4, units = "in", dpi = 300)

############################################SMS
df_SMS <- read.csv("SMS.CSV")

# 创建绘图
p_SMS <- ggplot(df_SMS, aes(x = Time, y = `CtC0`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `CtC0` - std, ymax = `CtC0` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab(expression(italic(C)[t]/italic(C)[0])) +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.8, 0.9),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,2,4,6,8,10,12,14,17)) +# 调整 x 轴间隔 
  #facet_wrap(~ Group, ncol = 2) +
  coord_cartesian(clip = "off")
# 显示绘图
print(p_SMS) 
ggsave("p_SMS.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_SMS.svg", width = 5, height = 4, units = "in", dpi = 300)



############################################光解水解
df_guangjie <- read.csv("光解水解.CSV")

# 创建绘图
p_guangjie <- ggplot(df_guangjie, aes(x = Time, y = `CtC0`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `CtC0` - std, ymax = `CtC0` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab(expression(italic(C)[t]/italic(C)[0])) +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.6, 0.7),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,4,8,12,17)) +# 调整 x 轴间隔
  scale_y_continuous(breaks = c (0.2,0.4,0.6,0.8,1,1.2))
  #facet_wrap(~ Group, ncol = 2) +
  coord_cartesian(clip = "off")
# 显示绘图
print(p_guangjie) 
ggsave("p_guangjie.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_guangjie.svg", width = 5, height = 4, units = "in", dpi = 300)

############################################pH
df_pH <- read.csv("pH.CSV")

# 创建绘图
p_pH <- ggplot(df_pH, aes(x = Time, y = `pH`, colour = Group, )) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = `pH` - std, ymax = `pH` + std), 
                width = 0.2, size = 0.5) +
  scale_color_aaas() +
  xlab("Time (days)") +
  ylab("pH") +
  theme_classic() +
  theme(
    text = element_text(family = "Helvetica", size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.line = element_line(size = 0.5),
    axis.ticks = element_line(size = 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.position = c(0.2, 0.7),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    #strip.text = element_text(size = 10),  # 调整分面标题的字体大小
    #strip.background = element_rect(color = "black", size = 0.2, fill = "lightgray"),  # 自定义边框和背景
  ) +
  scale_x_continuous(breaks = c (0,4,8,12,17)) +# 调整 x 轴间隔
#facet_wrap(~ Group, ncol = 2) +
coord_cartesian(clip = "off")
# 显示绘图
print(p_pH) 
ggsave("p_pH.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_pH.svg", width = 5, height = 4, units = "in", dpi = 300)

############################# 0，17天干重
df_dry_weigh <- read.csv("Dryweigh.CSV")
df_dry_weigh$Time <- factor(df_dry_weigh$Time, levels = c("0","17"))
p_dry_weigh <- ggplot(df_dry_weigh,aes(x=Time, y=Mean, fill=Group))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6, colour = "black", size = 0.3) + 
  geom_point(aes(group = Group), 
             position = position_dodge(width = 0.8),  # 点的分布与柱子的宽度对齐
             shape = 21, size = 3, stroke = 0.5, fill = "white") +
  geom_errorbar(aes(ymin = Mean - std, ymax = Mean + std), width = 0.1, size = 0.5, position = position_dodge(width = 0.8)) + 
  xlab("Time (days)") +
  ylab("Dry weight (g/L)") +
  scale_fill_aaas() +   # 使用 Nature 配色方案
  coord_cartesian(ylim = c(0,1.2)) +  # 设置 y 轴范围
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Helvetica", size = 12),  # 使用常用的出版物字体
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.line = element_blank(),
    axis.ticks = element_line(size = 0.5, colour = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.2, 0.7),  # 将图例位置设在右下角
    legend.justification = c(0, 1),  # 设置图例的对齐方式
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
print(p_dry_weigh) 

ggsave("p_dry_weigh.pdf", width = 5, height = 4, units = "in", dpi = 300)
#导出为svj格式
ggsave("p_dry_weigh.svg", width = 5, height = 4, units = "in", dpi = 300)






##使用patchwork组合并自动标注
combined_plot <- (p_OD680|p_Cell_number|p_SMS)/(p_dry_weigh|p_pH|p_FVFM) +
  plot_annotation(tag_levels = "a")
print(combined_plot)
ggsave("combined_plot.pdf", width = 10, height = 6, units = "in", dpi = 300)
ggsave("combined_plot.svg", width = 10, height = 6, units = "in", dpi = 300)











