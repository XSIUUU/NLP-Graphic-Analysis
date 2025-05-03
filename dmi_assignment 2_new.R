# dmi assignment 2 revised version
# this clears the environment from any leftover variables (makes a clean sheet)
rm(list=ls())
# Loading necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(readxl)
library(lubridate)
library(hms)
library(stringr)
library(tm)
options(scipen=999)

# --------------------
# 0 IMPORTING & MERGING -------------------------------------------------------
# --------------------

## Load the data
setwd("E:/onedrive/Groningen(online)/MADS/2a/digital marketing intelligence/tutorial/assignment 2")
setwd("C:/Users/XSIU/OneDrive/Groningen(online)/MADS/2a/digital marketing intelligence/tutorial/assignment 2")


# Read the Excel file
influencer_df <- read_excel("scrapping_dmi.xlsx")
object_df <- read_excel("detr_object_detection_results.xlsx")
color_df <- read_excel("color_analysis_results.xlsx")

# create df for user profile
profile <- data.frame(
  Influencer = c("realbarbarapalvin", "maralafontan", "badgalriri", "bellahadid", "alexandrasaintmleux"),
  followers = c(20800000, 1100000, 149000000, 61200000, 1600000),
  following = c(370, 1006, 1672, 989, 933),
  post = c(1997, 1534, 4992, 851, 108)
)

# --------------------
# 0.1 data cleaning -------------------------------------------------------
# --------------------
str(influencer_df)
summary(influencer_df)
str(object_df)
summary(object_df)
str(color_df)
summary(color_df)

# data type of likes (chr->num)
influencer_df$Likes <- as.numeric(gsub("[^0-9.]", "", influencer_df$Likes))
# date changing
influencer_df$`Date Time` <- dmy_hms(influencer_df$`Date Time`)
influencer_df$Date <- as_date(influencer_df$`Date Time`)   
influencer_df$Time <- as_hms(influencer_df$`Date Time`) 

# variables names changing
colnames(influencer_df) <- gsub(" ", "_", colnames(influencer_df))
colnames(color_df) <- gsub(" ", "_", colnames(color_df))
colnames(object_df) <- gsub(" ", "_", colnames(object_df))

# Rename Caption column to Caption_Text
influencer_df <- influencer_df %>%
  rename(Caption_Text = Caption) %>%
  mutate(
    Caption = case_when(
      is.na(Caption_Text) & is.na(Emoji_Use) ~ NA_character_,
      is.na(Caption_Text) ~ Emoji_Use,
      is.na(Emoji_Use) ~ Caption_Text,
      TRUE ~ paste(Caption_Text, Emoji_Use)
    )
  )

# --------------------
# 0.2 statistical analysis -------------------------------------------------------
# --------------------
str(influencer_df)
summary(influencer_df)
str(object_df)
summary(object_df)
str(color_df)
summary(color_df)


ggplot(influencer_df %>% group_by(Influencer) %>% summarize(mean_likes=mean(Likes,na.rm = TRUE)) %>% arrange(desc(mean_likes)) %>% mutate(Influencer = factor(Influencer, levels = Influencer)),aes(Influencer,mean_likes))+
  geom_col(fill="gold") +
  geom_text(aes(label = round(mean_likes,0)),  
            vjust = -0.5, size = 3, fontface = "bold") +
  labs(
    title="mean likes of each influencer",
    x="influencer",
    y="mean likes"
  )+
  theme_minimal()

ggplot(influencer_df %>% group_by(Influencer) %>% summarize(mean_comments=mean(Number_of_Comments,na.rm = TRUE)) %>% arrange(desc(mean_comments)) %>% mutate(Influencer = factor(Influencer, levels = Influencer)),aes(Influencer,mean_comments))+
  geom_col(fill="gold") +
  geom_text(aes(label = round(mean_comments,0)),  
            vjust = -0.5, size = 3, fontface = "bold") +
  labs(
    title="mean comments of each influencer",
    x="influencer",
    y="mean comments"
  )+
  theme_minimal()
# for this plot, I remove a post of bella in 2021
ggplot(influencer_df %>% filter(!is.na(Date) & Date > as_date("2023-01-01")),aes(Date,Likes,color = Influencer)) +
  geom_point(alpha=0.8,size=3)


# --------------------
# 0.3 data Merging -------------------------------------------------------
# --------------------
Merge_df <- influencer_df %>% left_join(color_df,by = c("Image_Name"="File_Name")) %>% 
  left_join(object_df,by=c("Image_Name"="File_Name")) %>% 
  left_join(profile,by="Influencer")

# -----------------------------------------------------------------------------------------------
#     Describe the descriptive statistics (mean, median) of the engagement metrics (e.g., likes, 
# 1️⃣  comments). Additionally, analyze influencer-level features such as the number of followers 
#     and followings.  
# -----------------------------------------------------------------------------------------------

descriptive_df <- influencer_df %>% group_by(Influencer) %>% 
  summarize(mean_likes=mean(Likes,na.rm = TRUE),
            median_likes = median(Likes, na.rm = TRUE),
            mean_comments=mean(Number_of_Comments,na.rm = TRUE),
            median_comments = median(Number_of_Comments, na.rm = TRUE))
descriptive_df

profile %>% arrange(desc(followers))
profile %>% arrange(desc(following))

# -----------------------------------------------------------------------------------------------
# 2️⃣  How many words does the average caption contain? What are the 10 most frequent words 
#     across all posts?  
# -----------------------------------------------------------------------------------------------

# word & emoji counts
Merge_df %>%
  group_by(Influencer) %>%
  summarize(
    max_words = max(replace_na(str_count(Caption_Text, "\\S+"), 0)),
    min_words = min(replace_na(str_count(Caption_Text, "\\S+"), 0)),
    avg_words = mean(replace_na(str_count(Caption_Text, "\\S+"), 0)),
    max_emoji = max(Emoji_Number),
    min_emoji = min(Emoji_Number),
    avg_emoji = mean(Emoji_Number)
  )

# Build corpus
corpus <-Corpus(VectorSource(Merge_df$Caption))
inspect(corpus[1:5])
# Clean text
# Define a function to remove invisible Unicode characters (e.g., VS-16 and Zero Width Joiner)
#remove_invisible <- content_transformer(function(x) gsub("[\uFE0F\u200D]", "", x))
#corpus <- tm_map(corpus, remove_invisible)
#inspect(corpus[1:5])

# 自定义标点符号移除函数，覆盖 Unicode 标点
remove_extended_punctuation <- content_transformer(function(x) {
  # 移除所有标点（ASCII + Unicode）
  x <- gsub("[[:punct:]]", "", x)  # ASCII 标点
  # 额外移除常见 Unicode 标点（按需扩展）
  x <- gsub("[‘’“”—…]", "", x, perl = TRUE)  # 直接匹配 Unicode 标点
  return(x)
})
corpus <- tm_map(corpus, remove_extended_punctuation)
inspect(corpus[1:5])


# 移除所有格（'s/’s/‘s）和缩写（'re/'ve等）
remove_possessive <- content_transformer(function(x) {
  x <- gsub("['’‘](s|re|ve|ll|d|m|t)\\b", "", x, ignore.case = TRUE)
  return(x)
})
corpus <- tm_map(corpus, remove_possessive)
inspect(corpus[1:5])

# 移除连字符、长破折号等
remove_hyphens <- content_transformer(function(x) {
  x <- gsub("[-—–]", " ", x)  # 替换为空格避免粘连
  return(x)
})
corpus <- tm_map(corpus, remove_hyphens)
inspect(corpus[1:5])

#corpus <-tm_map(corpus, tolower)
custom_tolower <- content_transformer(function(x) {
  # Use a regular expression to match all alphabetic characters and convert to lowercase
  # Non-alphabetic characters (such as Emoji) are retained as is
  gsub("([A-Z])", "\\L\\1", x, perl = TRUE)
})
corpus <- tm_map(corpus, custom_tolower)
inspect(corpus[1:5]) #处理了emoji好像

corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5])
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5])



# Function to remove empty spaces
removeEmptySpace <- function(x) gsub("^\\s+|\\s+$", "", x)  # Remove leading/trailing spaces
corpus <- tm_map(corpus, content_transformer(removeEmptySpace))
inspect(corpus[1:5])


cleanset<-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])


# Tokenization: Create quanteda dfm
cleaned_texts <- sapply(cleanset, as.character)
corpus_q <- corpus(cleaned_texts)
dfm_obj <- dfm(tokens(corpus_q))

# frequency counting
dfm_matrix <- as.matrix(dfm_obj)
word_counts <- colSums(dfm_matrix)
word_freq <- data.frame(
  feature = names(word_counts),
  frequency = word_counts,
  row.names = NULL
) %>% arrange(desc(frequency))

# Bar plot
barplot(
  height = word_freq[word_freq$frequency>10,]$frequency,
  names.arg = word_freq[word_freq$frequency>10,]$feature,
  las = 2,
  col = rainbow(20),
  main = "Top Frequent Words"
)


# -----------------------------------------------------------------------------------------------
# 3️⃣ How can you visualize the overall distribution of words in the data in a compelling way? 
# -----------------------------------------------------------------------------------------------

# Create a wordcloud
#install.packages("wordcloud2")
library(wordcloud2)

colnames(word_freq) <- c("word", "freq")  # 设置列名

wordcloud2(word_freq[which(word_freq$freq > 7),], size=0.7, shape = 'circle', rotateRatio  = 0.5, minSize  = 0.5) 
wordcloud2(word_freq[which(word_freq$freq > 10),], size=0.7, shape = 'circle', rotateRatio  = 0.5, minSize  = 1) 

# -----------------------------------------------------------------------------------------------
# 4️⃣Create textual features based on three different off-the-shelf lexicons 
# -----------------------------------------------------------------------------------------------
#install.packages("tidytext")
#install.packages("quanteda")
#install.packages("sentimentr")
#install.packages("textdata")
#install.packages("lexicon")
library(tidytext)
library(lexicon)
library(syuzhet)  # For NRC and AFINN sentiment
library(textdata) # For Bing and AFINN lexicons
library(sentimentr) # For VADER-like sentiment analysis
library(quanteda) # For LIWC-like analysis (not exact)

# Convert corpus cleanset to dataframe
text_data <- data.frame(text = sapply(cleanset, as.character), stringsAsFactors = FALSE)
text_data <- text_data %>%
  mutate(id = row_number())

####  VADER Sentiment Analysis ####
vader_sentiment <- sentiment_by(text_data$text)

###  AFINN Sentiment Analysis
afinn_lexicon <- get_sentiments("afinn")

afinn_sentiment <- text_data %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn_lexicon, by = "word") %>%
  group_by(id) %>%
  summarise(afinn_score = sum(value), .groups = "drop")

###  BING Sentiment Analysis
bing_lexicon <- get_sentiments("bing")

bing_sentiment <- text_data %>%
  unnest_tokens(word, text) %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(bing_score = positive - negative)

###  NRC Sentiment Analysis
nrc_lexicon <- get_sentiments("nrc")

nrc_sentiment <- text_data %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

### 合并所有结果
sentiment_results <- text_data %>%
  select(id, text) %>%
  left_join(afinn_sentiment, by = "id", suffix = c("", "_afinn")) %>%        # 第一个join无冲突
  left_join(bing_sentiment, by = "id", suffix = c("", "_bing")) %>%          # 保留原始列名+后缀
  left_join(nrc_sentiment, by = "id", suffix = c("", "_nrc")) %>%            # 情绪分类列添加后缀
  left_join(vader_sentiment, by = c("id" = "element_id"), suffix = c("", "_vader"))  # VADER专用

# 查看前几行
sentiment_results[is.na(sentiment_results)] <- 0
print(head(sentiment_results))

# merge data
full_df <- cbind(Merge_df, sentiment_results)

# -----------------------------------------------------------------------------------------------
# 5️⃣ Extract text features based on existing literature, such as the number of hashtags, emojis, 
#    mentions (@), and question marks per post caption.   
# -----------------------------------------------------------------------------------------------

full_df <- full_df %>%
  mutate(
    hashtag_count = if_else(is.na(Caption), 0L, str_count(Caption, pattern = "#")),
    at_count = if_else(is.na(Caption), 0L, str_count(Caption, pattern = "@")),
    questionmark_count = if_else(is.na(Caption), 0L, str_count(Caption, pattern = "\\?")),
    exclamation_count = if_else(is.na(Caption), 0L, str_count(Caption, pattern = "!"))
  )


# -----------------------------------------------------------------------------------------------
# 6️⃣ Conduct topic modeling via LDA. Determine the optimal number of topics, explore, interpret 
#    and name the topics and generate a list with the top 20 words per topic. 
# -----------------------------------------------------------------------------------------------

#install.packages("ldatuning")  # 确保包名正确
#install.packages("topicmodels")
library(ldatuning)
library(topicmodels)
library(parallel)

# **🔹 关键步骤：移除全零的文档**
dfm_obj_complete <- dfm_trim(dfm_obj, min_termfreq = 1, min_docfreq = 1)  # 确保至少出现一次
dfm_obj_complete <- dfm_subset(dfm_obj_complete, rowSums(dfm_obj_complete) > 0)  # 过滤掉全零的行

# **检查是否仍然有全零行**
if (nrow(dfm_obj_complete) == 0) {
  stop("Error: All rows are empty after preprocessing. Check data cleaning steps.")
}

# 频率矩阵
dfm_complete_matrix <- as.matrix(dfm_obj_complete)

# --------------------------
# 3. 确定最佳主题数量
# --------------------------
set.seed(77)  # 固定随机种子保证可重复性
result <- FindTopicsNumber(
  dfm_complete_matrix,
  topics = seq(from = 2, to = 8, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 1,  # 只使用一个核心，避免并行计算导致连接问题
  verbose = TRUE
)

#warnings()

# 绘制最佳主题数量评估图
FindTopicsNumber_plot(result)

#  Train the LDA Model with Optimal Topic Count
optimal_topics <- 4  # Adjust based on the previous step
lda_model <- LDA(dfm_complete_matrix, k = optimal_topics, method = "Gibbs", control = list(seed = 77))

# Extract top words per topic
topics <- tidy(lda_model, matrix = "beta")
topics
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Print the top words per topic
print(top_terms)

# -----------------------------------------------------------------------------------------------
# 7️⃣ Generate topic probabilities per post. 
# -----------------------------------------------------------------------------------------------

#  Get Topic Probabilities for Each Document
theta_matrix <- posterior(lda_model)$topics
theta_df <- as.data.frame(theta_matrix)
colnames(theta_df) <- paste0("Topic_", 1:ncol(theta_df))

# Print the topic probabilities
print(head(theta_df, 10))

#  Visualize the Results
# Barplot of top words per topic
top_terms %>%
  ggplot(aes(reorder(term, beta), beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Words in Each Topic", x = "Terms", y = "Beta Score") +
  theme_minimal()

# Boxplot of topic distribution across documents
theta_df_long <- pivot_longer(theta_df, cols = everything(), names_to = "Topic", values_to = "Probability")

ggplot(theta_df_long, aes(x = Topic, y = Probability, fill = Topic)) +
  geom_boxplot() +
  labs(title = "Topic Probabilities Across Documents", x = "Topic", y = "Probability") +
  theme_minimal()

# -----------------------------------------------------------------------------------------------
#    Extract all image-based features that are part of the provided code (e.g., brightness, 
# 8️⃣ saturation, number of objects) and analyze their impact on engagement. Use combination of 
#    3 methods discussed for extracting image features.  
# -----------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------
# 9️⃣ Analyze the impact of the text and image features that you generated on user engagement 
#    via regression models.
# -----------------------------------------------------------------------------------------------

##################
# Random Froest  #
##################
# load fastDummies package
#install.packages("fastDummies")
library(fastDummies)

# create dummy variables of Influencer
full_df_dummy <- dummy_cols(full_df, select_columns = "Influencer", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
head(full_df_dummy)
sum(is.na())

# ------------------------------
# 1. 加载必要的包
# ------------------------------
library(randomForest)  # 随机森林模型


# ------------ 数据预处理 ------------
# 仅删除 Likes 和 Number_of_Comments 列中存在 NA 的行
full_df_clean <- full_df_dummy %>%
  filter(!is.na(Likes) & !is.na(Number_of_Comments))

# 定义自变量集合（确保列名与数据一致）
features <- c(
  "Emoji_Number", "Seconds_Passed",
  "Brightness", "Saturation", "Brightness_Contrast", "Warmth",
  "Total_Detected_Objects", "Number_of_Unique_Objects", "followers", 
  "following", "post", "afinn_score", "positive", "negative", "bing_score",
  "anticipation", "trust", "joy", "positive_nrc", "anger", "disgust", 
  "fear", "negative_nrc", "sadness", "surprise", "word_count",
  "ave_sentiment", "hashtag_count", "at_count", "questionmark_count",
  "exclamation_count", "Influencer_badgalriri", "Influencer_bellahadid",
  "Influencer_maralafontan", "Influencer_realbarbarapalvin"
)

rf_data <- full_df_clean %>%
  select(Likes, Number_of_Comments,Emoji_Number,Seconds_Passed,Brightness,Saturation,
         Brightness_Contrast,Warmth,Total_Detected_Objects,Number_of_Unique_Objects,followers,
         following,post,afinn_score,positive,negative,bing_score,anticipation,trust,
         joy,positive_nrc,anger,disgust,fear,negative_nrc,sadness,surprise,word_count,
         ave_sentiment,hashtag_count,at_count,questionmark_count,exclamation_count,
         Influencer_badgalriri,Influencer_bellahadid,
         Influencer_maralafontan,Influencer_realbarbarapalvin
  )

# 确认所有特征存在于数据中
if (!all(features %in% names(full_df_clean))) {
  stop("部分特征名称不匹配，请检查列名")
}

# ------------ 构建模型 ------------
# 模型1：预测Likes
set.seed(1234) # 固定随机种子
rf_likes <- randomForest(
  x = rf_data[, features],
  y = rf_data$Likes,
  ntree = 500,       # 树的数量（根据计算资源调整）
  importance = TRUE, # 计算变量重要性
  na.action = na.omit
)

# 模型2：预测Number_of_Comments
set.seed(1234)
rf_comments <- randomForest(
  x = rf_data[, features],
  y = rf_data$Number_of_Comments,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)

# ------------ 模型评估指标计算 ------------
# 自定义评估函数
calculate_metrics <- function(model, actual, model_name) {
  predicted <- model$predicted
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  data.frame(
    Model = model_name,
    R_Squared = round(r_squared, 3),
    MSE = round(mse, 1),
    RMSE = round(rmse, 1)
  )
}

# 计算两个模型的指标
metrics_likes <- calculate_metrics(
  rf_likes, 
  actual = rf_data$Likes,
  model_name = "Likes Model"
)

metrics_comments <- calculate_metrics(
  rf_comments,
  actual = rf_data$Number_of_Comments,
  model_name = "Comments Model"
)

# 合并评估结果
performance_metrics <- bind_rows(metrics_likes, metrics_comments)

# 打印格式化结果
cat("\n=== 模型性能评估 ===\n")
print(performance_metrics)
cat("\n")

# ------------ 变量重要性可视化 ------------
# 自定义可视化函数
plot_importance <- function(model, title) {
  imp <- importance(model, type = 1) # type=1表示基于MSE的重要性
  imp_df <- data.frame(
    Variable = rownames(imp),
    Importance = imp[, "%IncMSE"]
  ) %>%
    arrange(desc(Importance)) %>%
    head(20) # 显示前20个重要变量
  
  ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = title,
      x = "Variable",
      y = "Variable Importance (%IncMSE)"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 12))
}

# 生成图形
plot_likes <- plot_importance(rf_likes, "Likes预测模型变量重要性")
plot_comments <- plot_importance(rf_comments, "评论数预测模型变量重要性")

# 显示图形
print(plot_likes)
print(plot_comments)


########################
# Stepwise Regression  #
########################
# ------------------------------
# 1. 加载必要包
# ------------------------------
#install.packages("tidyverse")
#install.packages("MASS")
#library(MASS)       # 提供stepAIC函数
library(tidyselect)

SW_data <- full_df_clean %>%
  select(Likes, Number_of_Comments,Emoji_Number,Seconds_Passed,Brightness,Saturation,
    Brightness_Contrast,Warmth,Total_Detected_Objects,Number_of_Unique_Objects,followers,
    following,post,afinn_score,positive,negative,bing_score,anticipation,trust,
    joy,positive_nrc,anger,disgust,fear,negative_nrc,sadness,surprise,word_count,
    ave_sentiment,hashtag_count,at_count,questionmark_count,exclamation_count,
    Influencer_badgalriri,Influencer_bellahadid,
    Influencer_maralafontan,Influencer_realbarbarapalvin
  )
# ------------------------------
# 3. 构建逐步回归模型（双向选择）
# ------------------------------
# 模型1：预测Likes
full_model_likes <- lm(
  Likes ~ ., 
  data = SW_data %>% select(-Number_of_Comments)
)
# 检查目标列是否存在
if (!all(c("Likes", "Number_of_Comments") %in% names(full_df_clean))) {
  stop("数据框中缺少 Likes 或 Number_of_Comments 列")
}

step_model_likes <- MASS::stepAIC(
  full_model_likes,
  direction = "both",  # 双向逐步
  trace = FALSE        # 关闭冗长输出
)

# 模型2：预测评论数
full_model_comments <- lm(
  Number_of_Comments ~ ., 
  data = SW_data %>% select(-Likes)
)

step_model_comments <- MASS::stepAIC(
  full_model_comments,
  direction = "both",
  trace = FALSE
)

# ------------------------------
# 4. 模型结果输出
# ------------------------------
# Likes模型摘要
cat("\n=== Likes stepwise model ===\n")
summary(step_model_likes)
cat("AIC:", AIC(step_model_likes), "\n")

# 评论数模型摘要
cat("\n=== comments stepwise model ===\n")
summary(step_model_comments)
cat("AIC:", AIC(step_model_comments), "\n")

# 保存最终模型公式
final_formula_likes <- formula(step_model_likes)
final_formula_comments <- formula(step_model_comments)

cat("\n Likes model:\n")
print(final_formula_likes)

cat("\n comments model:\n")
print(final_formula_comments)

# VIF check

library(car)

# 分别计算两个模型的 VIF
cat("=== VIF for Likes model ===\n")
vif_likes <- vif(step_model_likes)
print(vif_likes)

cat("\n=== VIF for Comments model ===\n")
vif_comments <- vif(step_model_comments)
print(vif_comments)

# ------------------------------
# 5. 模型评估指标
# ------------------------------
calculate_metrics <- function(model) {
  data.frame(
    R_Squared = round(summary(model)$r.squared, 3),
    Adj_R_Squared = round(summary(model)$adj.r.squared, 3),
    AIC = round(AIC(model), 1),
    BIC = round(BIC(model), 1)
  )
}

metrics_likes <- calculate_metrics(step_model_likes) %>%
  mutate(Model = "Likes Model")

metrics_comments <- calculate_metrics(step_model_comments) %>%
  mutate(Model = "Comments Model")

performance_metrics <- bind_rows(metrics_likes, metrics_comments) %>%
  select(Model, everything())

cat("\n=== 模型性能对比 ===\n")
print(performance_metrics)

#######----------------------------------------------------------------------------------
#### LM

# Likes 模型（不变）
lm_likes <- lm(
  Likes ~ Brightness + Saturation + followers + anticipation +
    trust + anger + disgust + fear + negative_nrc + at_count + 
    Influencer_maralafontan,
  data = SW_data
)

# Comments 模型（去掉 Influencer_badgalriri）
lm_comments <- lm(
  Number_of_Comments ~ Emoji_Number + Seconds_Passed + Brightness + 
    Brightness_Contrast + followers + anger + disgust + fear + 
    negative_nrc + exclamation_count,
  data = SW_data
)

# 查看摘要结果
summary(lm_likes)
summary(lm_comments)
BIC(lm_likes)
BIC(lm_comments)




# -----------------------------------------------------------------------------------------------
# 1️⃣1️⃣ Analyze the impact of the text and image features that you generated on user engagement 
#    via regression models.
# -----------------------------------------------------------------------------------------------
library(randomForest)

###################################
# likes model adjustment process
###################################
# Likes 模型（包含二次项）
lm_likes_quad <- lm(
  Likes ~ Brightness + I(Brightness^2) +
    Saturation + I(Saturation^2) +
    followers + I(followers^2) +
    anticipation + I(anticipation^2) +
    trust + I(trust^2) +
    anger + I(anger^2) +
    disgust + I(disgust^2) +
    fear + I(fear^2) +
    negative_nrc + I(negative_nrc^2) +
    at_count + I(at_count^2) +
    Influencer_maralafontan,
  data = SW_data
)
summary(lm_likes_quad)

# 根据二次项的引入对like模型调整
lm_likes_refined <- lm(
  Likes ~ Brightness + I(Brightness^2) + 
    Saturation + 
    followers + 
    anger +
    I(fear^2) +  # 保留平方项
    I(negative_nrc^2) +  # 只保留平方项
    at_count +
    Influencer_maralafontan,
  data = SW_data
)
summary(lm_likes_refined) 
BIC(lm_likes_refined) # R2提升和BIC下降,模型效力提高

# 引入交互项
lm_likes_interact <- lm(
  Likes ~ (Brightness + I(Brightness^2) + 
             Saturation + 
             followers + 
             anger +
             I(fear^2) +  # 保留平方项
             I(negative_nrc^2) +  # 只保留平方项
             at_count +
             Influencer_maralafontan)^2,
  data = SW_data
)
summary(lm_likes_interact)

# 在原数据框上创建SQ列,方便后续可视化
SW_data <- SW_data %>% mutate(
    followers_sq = followers^2,
    brightness_sq = Brightness^2,
    negative_nrc_sq = negative_nrc^2,
    fear_sq = fear^2
  )

# 根据交互项的显著性调整模型
lm_likes_final <- lm(
  Likes ~ Brightness + brightness_sq + Saturation + followers +
    anger + fear_sq + negative_nrc_sq + at_count + 
    Influencer_maralafontan + 
    Brightness:Saturation +
    Brightness:followers +
    Brightness:brightness_sq +
    brightness_sq :followers +
    Saturation:anger +
    Saturation:at_count +
    Saturation:negative_nrc_sq +
    followers:anger +
    followers:negative_nrc_sq,
  data = SW_data
)

summary(lm_likes_final)
BIC(lm_likes_final)




###################################
# comment model adjustment process
###################################

# Comments 模型（包含二次项，已去掉 Influencer_badgalriri）
lm_comments_quad <- lm(
  Number_of_Comments ~ Emoji_Number + I(Emoji_Number^2) +
    Seconds_Passed + I(Seconds_Passed^2) +
    Brightness + I(Brightness^2) +
    Brightness_Contrast + I(Brightness_Contrast^2) +
    followers + I(followers^2) +
    anger + I(anger^2) +
    disgust + I(disgust^2) +
    fear + I(fear^2) +
    negative_nrc + I(negative_nrc^2) +
    exclamation_count + I(exclamation_count^2),
  data = SW_data
)

summary(lm_comments_quad)

# 根据二次项引入,调整变量
lm_comments_refined <- lm(
  Number_of_Comments ~ 
    I(Seconds_Passed^2) +
    Brightness + I(Brightness^2) +
    I(followers^2) +
    anger +
    I(negative_nrc^2),
  data = SW_data
)

summary(lm_comments_refined)
BIC(lm_comments_refined)

# 引入交互项
lm_comments_interact <- lm(
  Number_of_Comments ~ (I(Seconds_Passed^2) +
                        Brightness + I(Brightness^2) +
                        I(followers^2) +
                        anger +
                        I(negative_nrc^2))^2,
  data = SW_data
)
summary(lm_comments_interact)

# 根据交互项的显著性调整模型
lm_comments_final <- lm(
  Number_of_Comments ~ 
    Brightness + brightness_sq +
    followers_sq +
    anger +
    negative_nrc_sq +
    Brightness:followers_sq +
    brightness_sq:followers_sq +
    followers_sq:anger +
    followers_sq:negative_nrc_sq,
  data = SW_data
)

summary(lm_comments_final)
BIC(lm_comments_final)





###############
#visuallization--likes model
###############
# 如未安装 interactions 包，请先运行：
#install.packages("interactions")

library(interactions)

# 简洁样式
theme_set(theme_minimal())




# ✅ 1. Brightness²（二次曲线）
ggplot(SW_data, aes(x = Brightness, y = Likes)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "darkblue") +
  labs(title = "Quadratic Effect: Brightness² on Likes")

# ✅ 2. followers × Brightness（交互）
interact_plot(lm_likes_final, pred = Brightness, modx = followers, 
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Brightness × Followers")

# ✅ 3. fear²（二次曲线）
ggplot(SW_data, aes(x = fear, y = Likes)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "purple") +
  labs(title = "Quadratic Effect: Fear² on Likes")

# ✅ 4. negative_nrc²
ggplot(SW_data, aes(x = negative_nrc, y = Likes)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "firebrick") +
  labs(title = "Quadratic Effect: Negative NRC² on Likes")

# ✅ 5. Brightness² × followers
interact_plot(lm_likes_final, pred = brightness_sq, modx = followers,
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Brightness² × Followers")

# ✅ 6. Saturation × anger
interact_plot(lm_likes_final, pred = Saturation, modx = anger,
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Saturation × Anger")

# ✅ 7. Saturation × at_count
interact_plot(lm_likes_final, pred = Saturation, modx = at_count,
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Saturation × @ Count")

# ✅ 8. Saturation × negative_nrc²
interact_plot(lm_likes_final, pred = Saturation, modx = negative_nrc_sq,
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Saturation × Negative NRC²")

# ✅ 9. Brightness × Saturation
interact_plot(lm_likes_final, pred = Brightness, modx = Saturation,
              plot.points = TRUE, interval = TRUE, main.title = "Interaction: Brightness × Saturation")


###############
#visuallization--comments model
###############


# ✅ 1. Brightness²（二次项）
ggplot(SW_data, aes(x = Brightness, y = Number_of_Comments)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  labs(title = "Quadratic Effect: Brightness² on Comments")

# ✅ 2. followers²（二次项）
ggplot(SW_data, aes(x = followers, y = Number_of_Comments)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "darkgreen") +
  labs(title = "Quadratic Effect: Followers² on Comments")

# ✅ 3. negative_nrc²（二次项）
ggplot(SW_data, aes(x = negative_nrc, y = Number_of_Comments)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Quadratic Effect: Negative NRC² on Comments")

# ✅ 4. followers² × Brightness（交互）
interact_plot(lm_comments_final,
              pred = Brightness,
              modx = followers_sq,
              plot.points = TRUE,
              interval = TRUE,
              main.title = "Interaction: Brightness × Followers²")

# ✅ 5. followers² × Brightness²（交互）
interact_plot(lm_comments_final,
              pred = brightness_sq,
              modx = followers_sq,
              plot.points = TRUE,
              interval = TRUE,
              main.title = "Interaction: Brightness² × Followers²")

# ✅ 6. followers² × Anger（交互）
interact_plot(lm_comments_final,
              pred = anger,
              modx = followers_sq,
              plot.points = TRUE,
              interval = TRUE,
              main.title = "Interaction: Anger × Followers²")

# ✅ 7. followers² × negative_nrc²（交互）
interact_plot(lm_comments_final,
              pred = negative_nrc_sq,
              modx = followers_sq,
              plot.points = TRUE,
              interval = TRUE,
              main.title = "Interaction: Negative NRC² × Followers²")



