##数据清理下次搞好，注意缺失值。
####如果不是交互，对于categorical variable要进行-0.5&0.5的sum-code


#1.工作路径设置

setwd("G:/AAA/")
getwd()

#2. 数据准备

#2.1 数据的读取
df = read.table("G:/AAA/DATA.csv", header=TRUE, sep = ",")

#2.2 数据类型转换
df$Item = as.factor(df$Item)
df$Subject= as.factor(df$Subject)
df$Predictability=as.factor(df$Predictability)
levels(df$Predictability)
df$Predictability <- factor(
  ifelse(df$Predictability == "", NA, as.character(df$Predictability)),
  levels = c("High", "Low")
)
df$WMC=as.factor(df$WMC)
df$WMC <- factor(
  ifelse(df$WMC == "", NA, as.character(df$WMC)),
  levels = c("High", "Low")
)
df$ RT=as.numeric(df$RT)
df$Proficiency=as.numeric(df$Proficiency)

#2.3调整基线
levels(df$Predictability)
levels(df$WMC)

#2.4 标准差剔除outlier
RT_mean=mean(df$RT,na.rm=TRUE)
RT_mean

RT_sd= sd(df$RT, na.rm=TRUE)

lower_bound=RT_mean-2.5*RT_sd
upper_bound=RT_mean+2.5*RT_sd

DATA2 = df[df$RT >lower_bound
           & df$RT<upper_bound,]

str(DATA2)

write.csv(DATA2,"G:/AAA/DATA2.csv",row.names=FALSE)

df2=read.table("G:/AAA/DATA2.csv", header= TRUE, sep= ",")
df2$Item = as.factor(df2$Item)
df2$Subject= as.factor(df2$Subject)
df2$Predictability=as.factor(df2$Predictability)
df2$WMC=as.factor(df2$WMC)
df2$ RT=as.numeric(df2$RT)

#2.5计算保存描述性统计量

ncol(df2)
nrow(df2)
mean(df2$RT)
str(df2$RT)
sum(is.na(df2$RT))
mean(df2$RT, na.rm = TRUE)
sd(df2$RT, na.rm=TRUE)
var(df2$RT,na.rm=TRUE)
min(df2$RT,na.rm=TRUE)
max(df2$RT,na.rm=TRUE)

#2.6 分组计算
mean_rt <- aggregate(RT ~ Predictability + WMC, data = df2, 
                     FUN = function(x) mean(x, na.rm = TRUE))
mean_rt

sd_rt <- aggregate(RT ~ Predictability + WMC, data = df2, 
                   FUN = function(x) sd(x, na.rm = TRUE))

min_rt <- aggregate(RT ~ Predictability + WMC, data = df2, 
                    FUN = function(x) min(x, na.rm = TRUE))

max_rt <- aggregate(RT ~ Predictability + WMC, data = df2, 
                    FUN = function(x) max(x, na.rm = TRUE))

#导出
library(dplyr)

summary_table <- df2 %>%
  group_by(Predictability, WMC) %>%
  summarise(
    Mean = mean(RT, na.rm = TRUE),
    SD = sd(RT, na.rm = TRUE),
    Min = min(RT, na.rm = TRUE),
    Max = max(RT, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )


write.csv(summary_table, "G:/AAA/RT_summary_statistics.csv", row.names = FALSE)

#2.7 正态性检查
shapiro.test(df2$RT)

#qq图我们来咯
qqnorm(df2$RT)
qqline(df2$RT)
hist(df2$RT,breaks=30, main = "Histogram of R" , xlab= "RT")

#2.8 减少RT的正偏态
df2$logRT=log(df2$RT)

qqnorm(df2$logRT)
qqline(df2$logRT)
hist(df2$logRT,breaks=30, main = "Histogram of R" , xlab= "RT")

#3.开始建模
install.packages("lme4")
install.packages("Matrix")

library(lme4)
library(Matrix)

fitall= lmer(logRT~Predictability*WMC*Proficiency+(1+Predictability|Subject)+(1+Predictability|Item),data=df2,REML=FALSE)
summary(fitall)
isSingular(fitall)


fit1= lmer(logRT~Predictability*WMC*Proficiency+(1+Predictability|Subject),data=df2,REML=FALSE)
summary(fit1)
isSingular(fit1)

fit2= lmer(logRT~Predictability*WMC*Proficiency+(1|Subject),data=df2,REML=FALSE)
summary(fit2)
isSingular(fit2)

fit3= lmer(logRT~Predictability*WMC*Proficiency+(1|Subject)+(1|Item),data=df2,REML=FALSE)
summary(fit3)
isSingular(fit3)

qqnorm(resid(fit3))
qqline(resid(fit3))

anova(fit2,fit3)

#4.选择fit3作为最终模型的同时，进行画图

  library(dplyr)
  library(ggplot2)
  
  # 清理数据并重命名Predictability
  df2_clean <- df2 %>%
    filter(!is.na(RT), !is.na(WMC), !is.na(Predictability)) %>%
    mutate(Predictability = recode(Predictability,
                                   "High" = "High-cloze",
                                   "Low" = "Low-cloze"))
  
  # 绘图
  p1 <- ggplot(df2_clean, aes(x = WMC, y = RT, fill = WMC)) +
    stat_boxplot(geom = "errorbar", width = 0.2) +
    geom_boxplot(size = 0.5) +
    scale_fill_manual(values = c("#db6968", "#4d97cd")) +
    facet_grid(. ~ Predictability) +
    labs(title = "RTs for the Self-paced Reading Task Between High WMC and Low WMC Groups across High and Low Cloze Trials",
         x = "WMC Group", y = "RT(ms)") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(size=10))
  
  p1

ggsave("G:/AAA/Fig_boxplot.png", plot=p1, width=8, height=4)

#5.汇报LLM的结果
library(knitr)
library(kableExtra)

results <- data.frame(
  `Fixed effects` = c(
    "Model: logRT ~ Predictability × WMC × Proficiency + (1 | Subject) + (1 | Item)",
    "Intercept",
    "Predictability (Low vs High)",
    "WMC (Low vs High)", 
    "Proficiency",
    "Predictability × WMC",
    "Predictability × Proficiency",
    "WMC × Proficiency",
    "Predictability × WMC × Proficiency"
  ),
  β = c("", "6.056", "0.186", "0.079", "-0.538", "-0.089", "-0.445", "0.042", "0.885"),
  SE = c("", "0.066", "0.072", "0.083", "0.338", "0.086", "0.355", "0.397", "0.415"),
  t = c("", "92.23", "2.57", "0.96", "-1.59", "-1.04", "-1.25", "0.11", "2.13"),
  p = c("", "< .001", "0.011", "0.339", "0.113", "0.299", "0.212", "0.915", "0.034")
)

# 关键修正：把表格赋值给变量
table_output <- results %>%
  kable(
    format = "html",
    align = "lcccc",
    caption = "Table 3. Parameter estimates and results of significance tests in mixed-effects models.",
    col.names = c("Fixed effects", "β", "SE", "t", "p")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE
  ) %>%
  row_spec(1, bold = TRUE, italic = TRUE) %>%
  row_spec(c(3, 9), bold = TRUE, background = "#f0f0f0") %>%
  footnote(
    general = "Number of observations = 172, Subjects = 8, Items = 30. Model fit: AIC = 62.57, BIC = 97.19. Model comparison with simpler model (without Item random effects): χ²(1) = 3.47, p = 0.062.",
    general_title = "Note:"
  )

# 显示表格
table_output

# 现在保存这个表格对象
save_kable(table_output, "model_results.png", 
           zoom = 2,
           vwidth = 1000,
           vheight = 600)

#6.深入理解三重交互的简单效应和简单斜率分析
install.packages("emmeans")
install.packages("lmerTest")
library(lme4)       # 拟合模型
library(lmerTest)   # 获取p值
library(emmeans)    # 简单效应/斜率分析
library(ggplot2)    # 画图
library(dplyr)      # 数据处理

fit3 = lmer(logRT ~ Predictability * WMC * Proficiency + (1|Subject) + (1|Item), 
             data = df2, REML = FALSE)
# 计算在每一个Predictability和WMC水平上，Proficiency的简单斜率
simple_slopes <- emtrends(
  fit3, 
  specs = ~ Predictability * WMC,  # 按这两个变量的所有组合进行分组
  var = "Proficiency"              # 考察Proficiency的斜率
)

# 查看结果
summary(simple_slopes, infer = c(TRUE, TRUE))
# 对简单斜率进行成对比较 - 看看哪些条件间的斜率有显著差异
slope_comparisons <- pairs(simple_slopes)
summary(slope_comparisons, infer = c(TRUE, TRUE))

# 更详细的对比：比较在相同Predictability水平下，不同WMC组的斜率差异
contrast_slopes <- contrast(simple_slopes, 
                            method = "pairwise", 
                            by = "Predictability")
summary(contrast_slopes, infer = c(TRUE, TRUE))

# 比较在相同WMC水平下，不同Predictability组的斜率差异  
contrast_slopes2 <- contrast(simple_slopes,
                             method = "pairwise",
                             by = "WMC")
summary(contrast_slopes2, infer = c(TRUE, TRUE))

#7.可视化简单斜率

slope_plot <- ggplot(as.data.frame(simple_slopes), 
                     aes(x = interaction(Predictability, WMC, sep = " / "), 
                         y = Proficiency.trend,
                         color = interaction(Predictability, WMC))) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  labs(title = "Simple Slopes of Proficiency Across Conditions",
       subtitle = "With 95% Confidence Intervals",
       x = "Condition (Predictability / WMC)",
       y = "Simple Slope of Proficiency on logRT",
       color = "Condition") +
  theme_bw(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"))

print(slope_plot)
ggsave("Table_5_Simple_Slopes.png", slope_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

#8.可视化交互作用

proficiency_seq <- seq(min(df2_clean$Proficiency), max(df2_clean$Proficiency), length.out = 50)
pred_data <- emmip(fit3, Predictability ~ Proficiency | WMC, 
                   at = list(Proficiency = proficiency_seq), 
                   plotit = FALSE)

interaction_plot <- ggplot(pred_data, aes(x = Proficiency, y = yvar, 
                                          color = Predictability)) +
  geom_line(size = 1.5) +
  facet_wrap(~WMC, labeller = as_labeller(c(High = "High WMC", Low = "Low WMC"))) +
  labs(title = "Processing Speed as a Function of Proficiency and Predictability",
       subtitle = "Separated by Working Memory Capacity Groups",
       x = "Language Proficiency", 
       y = "Predicted logRT",
       color = "Predictability") +
  theme_bw(14) +
  theme(plot.title = element_text(face = "bold", size = 16),
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "top") +
  scale_color_manual(values = c("High" = "#377EB8", "Low" = "#E41A1C"))

print(interaction_plot)
ggsave("Table_6_Interaction_Patterns.png", interaction_plot,
       width = 12, height = 6, dpi = 300, bg = "white")


