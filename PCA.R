rm(list = ls())
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(missMDA)
library(FactoMineR)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggExtra)

#加载数据
df <- read.csv("data.csv", header = TRUE)

# 假设 df 中有一个 Time 列，去除 Time 列后只保留数值型列进行 PCA
df_numeric <- df[, !names(df) %in% c("Time")]  # 删除 Time 列

# 如果 Group 是因子或字符型列，先排除它
df_numeric <- df_numeric[, sapply(df_numeric, is.numeric)]  # 筛选所有数值型列


#使用RDA函数执行pca分析
#scale = TRUE代表进行标准化
df_pca <- rda(df_numeric, scale = TRUE)
df_pca

# scaling 参数代表为标尺类型， 默认为2型标尺
summary(df_pca)
#可以切换为1型标尺
summary(df_pca, scaling = 1)

#提取特征值
dfc <- df_pca$CA$eig
dfc

#kasiser-Guttman准则选取排序轴
dfc[dfc>mean(dfc)]

#断棍模型
n <- length(dfc)
bsm <- data.frame(j=seq(1:n), p = 0)
bsm$p[1] <- 1/n
for (i in 2:n) {
  bsm$p[i]=bsm$p[i-1]+(1/(n+1-i))
}
bsm$p <- 100*bsm$p/n
bsm

#特征值
barplot(dfc, main = "Eigenvalues", col = "bisque", las = 2)
abline(h = mean(dfc), col = "red") # 特征根取平均值
legend("topright","Average eigenvalue", lwd = 1, col = 2, bty = "n")

#方差百分比
barplot(t(cbind(100*dfc/sum(dfc), bsm$p[n:1])), beside = TRUE,
        main = "%variance", col = c("bisque",2), las=2)
legend("topright",c("%eigenvalue","Broken stick model"),
       pch = 15, col = c("bisque",2), bty = "n")

#1型标尺默认图
biplot(df_pca, scaling= 1, main = "PCA - scaling 1")

#2型标尺默认图
biplot(df_pca, scaling= 2, main = "PCA - scaling 2")

##使用之前的代码进行绘图
#样方坐标
site <- df_pca$CA$u[,1:2]
head(site)

#环境坐标
data <- df_pca$CA$v[,1:2]
head(data)

#解释度
pca1 <- round(df_pca$CA$eig[1]/sum(df_pca$CA$eig)*100,2)
pca1
pca2 <- round(df_pca$CA$eig[2]/sum(df_pca$CA$eig)*100,2)
pca2


# 假设 'site' 和 'data' 数据框已经存在并且包含 PCA 结果
grp = as.data.frame(c(rep("A",1), rep("B",1), rep("C",1), rep("D",1), rep("E",1), rep("F",1), rep("G",1)))
colnames(grp) = "group"  # 重命名列名

p <- ggplot() +
  geom_point(data = site, aes(PC1, PC2, color = grp$group, size = 4)) +  # 缺失的闭合括号
  scale_color_manual(values = c("#CD4F39", "#CDB5CD", "#FF8C00", "#4F94CD", "#FF0066", "#EEE685", "#FFB5C5")) +  # 修正了逗号错误
  geom_segment(data = data, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), 
               linetype = 1, size = 0.6, colour = "red") +
  geom_text_repel(data = data, aes(PC1, PC2, label = row.names(data))) +  # 修正了 'lable' 为 'label'
  labs(x = "PCA1 77.47%", y = "PCA2 21.47%") +
  geom_hline(yintercept = 0, linetype = 3, size = 1) +  # 添加水平线
  geom_vline(xintercept = 0, linetype = 3, size = 1) +  # 添加垂直线
  guides(shape = guide_legend(title = NULL), color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +  # 修正了 guides 部分
  theme_bw() + 
  theme(panel.grid = element_blank())  # 去除背景网格线

# 显示图形
print(p)

ggsave("p.pdf", width = 6, height = 4, units = "in", dpi = 300)
ggsave("p.svg", width = 6, height = 4, units = "in", dpi = 300)


# 执行层次聚类
hc <- hclust(dist(df_numeric), method = "ward.D2")  # 使用欧几里得距离和Ward方法进行层次聚类
# 获取聚类结果
cluster_cut <- cutree(hc, k = 3)  # 将数据分为3类，可以根据需要调整
df_numeric$cluster <- as.factor(cluster_cut)
sit.sc1 <- scores(df_pca,display = "wa", scaling = 2)

# 可视化：使用 PCA 结果与层次聚类结果
p1 <- ggplot() +
  geom_point(data = site, aes(PC1, PC2, color = df_numeric$cluster, size = 4)) +  # 添加聚类信息
  scale_color_manual(values = c("#CD4F39", "#CDB5CD", "#FF8C00")) +  # 为不同聚类分配颜色
  geom_segment(data = data, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), 
               linetype = 1, size = 0.6, colour = "red") +
  geom_text_repel(data = data, aes(PC1, PC2, label = row.names(data))) +  
  labs(x = paste("PCA1", pca1, "%"), y = paste("PCA2", pca2, "%")) +
  geom_hline(yintercept = 0, linetype = 3, size = 1) +  
  geom_vline(xintercept = 0, linetype = 3, size = 1) +  
  guides(shape = guide_legend(title = NULL), color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +  
  theme_bw() + 
  theme(panel.grid = element_blank())  
print(p1)

ggsave("p1.pdf", width = 5, height = 4, units = "in", dpi = 300)
ggsave("p1.svg", width = 5, height = 4, units = "in", dpi = 300)
