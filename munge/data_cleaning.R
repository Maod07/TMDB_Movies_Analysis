# 安装包
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("stringr")

# 加载库
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)

# 读取数据
data <- read_csv("data/TMDB Movies Dataset-2.csv")

# 查看数据的前几行
head(data)


##识别并处理缺失值
# 替换所有包含 '#' 或是空白值的值为 NA
data <- data %>%
  mutate(across(everything(), ~ifelse(grepl("#", .x) | .x == "", NA, .x)))

# 检查缺失值情况
colSums(is.na(data))

# 填充字符型列的缺失值为 "Unknown"
data <- data %>%
  mutate(
    homepage = ifelse(is.na(homepage), "Unknown", homepage),
    tagline = ifelse(is.na(tagline), "Unknown", tagline),
    overview = ifelse(is.na(overview), "Unknown", overview),
    popularity_level = ifelse(is.na(popularity_level), "Unknown", popularity_level)
  )

# 检查填充后的缺失值情况
colSums(is.na(data))


##转换数据类型
# 检查数据类型
str(data)

# 转换 release_date 为 Date 类型，并提取年份和月份
data <- data %>%
  mutate(
    release_date = as.Date(release_date, origin = "1970-01-01"),
    release_year = year(release_date),
    release_month = month(release_date)
  )

# 转换数据类型（确保所有数值型变量都以数值形式存储）
data <- data %>%
  mutate(
    popularity = as.numeric(popularity),
    budget = as.numeric(budget),
    revenue = as.numeric(revenue),
    runtime = as.numeric(runtime),
    vote_count = as.numeric(vote_count),
    vote_average = as.numeric(vote_average),
    budget_adj = as.numeric(budget_adj),
    revenue_adj = as.numeric(revenue_adj),
    profit = as.numeric(profit)
  )


##识别并处理异常值
# 使用箱线图查看电影评分的异常值
ggplot(data, aes(y = vote_average)) + 
  geom_boxplot() + 
  labs(title = "Vote Average Boxplot")

# 使用箱线图查看投票数量的异常值
ggplot(data, aes(y = vote_count)) + 
  geom_boxplot() + 
  labs(title = "Vote Count Boxplot")

# 使用箱线图查看受欢迎度的异常值
ggplot(data, aes(y = popularity)) + 
  geom_boxplot() + 
  labs(title = "Popularity Boxplot")

# 使用箱线图查看预算的异常值
ggplot(data, aes(y = budget)) + 
  geom_boxplot() + 
  labs(title = "Budget Boxplot")

# 使用箱线图查看收入的异常值
ggplot(data, aes(y = revenue)) + 
  geom_boxplot() + 
  labs(title = "Revenue Boxplot")

# 检查上映年份的合理性
summary(data$release_year)

##开始过滤
# 过滤不合理的电影评分
data <- data %>%
  filter(vote_average >= 0 & vote_average <= 10)

# 过滤异常高的投票数量
data <- data %>%
  filter(vote_count < quantile(vote_count, 0.99, na.rm = TRUE))

## 确保 release_date 是 Date 类型
data <- data %>%
  mutate(release_date = as.Date(release_date, format = "%Y-%m-%d"))

# 过滤不合理的 release_year
data <- data %>%
  filter(release_year >= 1961 & release_year <= as.numeric(format(Sys.Date(), "%Y")))

# 修正 release_date
data <- data %>%
  mutate(release_date_corrected = if_else(year(release_date) != release_year, 
                                          as.Date(paste0(release_year, "-", month(release_date), "-", day(release_date))),
                                          release_date))

# 检查修正后的数据
summary(data$release_date_corrected)
head(data %>% select(release_date, release_year, release_date_corrected))

# 过滤异常高的受欢迎度
data <- data %>%
  filter(popularity < quantile(popularity, 0.99, na.rm = TRUE))

# 过滤异常高的预算和收入
data <- data %>%
  filter(budget < quantile(budget, 0.99, na.rm = TRUE),
         revenue < quantile(revenue, 0.99, na.rm = TRUE))

# 检查类型变量的合理性和一致性
unique(data$genres)

##一部电影包含多个不同的类型标签
# 分割 genres 列，并展开为独立的行
data_expanded <- data %>%
  separate_rows(genres, sep = "\\|")

# 查看分割和展开后的数据
head(data_expanded)

# 再次查看标准化后的唯一类型标签
unique(data_expanded$genres)

# 转换字符型变量为因子型变量
data_expanded <- data_expanded %>%
  mutate(
    original_title = as.factor(original_title),
    cast = as.factor(cast),
    homepage = as.factor(homepage),
    director = as.factor(director),
    tagline = as.factor(tagline),
    keywords = as.factor(keywords),
    overview = as.factor(overview),
    genres = as.factor(genres),
    production_companies = as.factor(production_companies),
    popularity_level = as.factor(popularity_level)
  )

# 查看转换后的数据类型
str(data_expanded)

# 检查描述性统计信息
summary(data_expanded)
