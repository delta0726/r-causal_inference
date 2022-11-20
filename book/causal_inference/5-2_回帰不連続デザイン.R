#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 5章 回帰不連続デザイン
# Theme     : 5-2 回帰不連続デザイン
# Date      : 2022/11/19
# Page      : P169 - P171
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 テーブル集計
# 3 線形回帰モデルによる分析
# 4 非線形回帰モデルによる分析


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(rdd)
library(rddtools)
library(gridExtra)


# データロード
male_data <- read_csv("csv/male_data.csv")
rdd_data <- read_csv("csv/rdd_data.csv")


# 1 データ確認 -------------------------------------------------------------------

# ＜ポイント＞
# - RCTデータはVisitに依存するものの、介入基準は明確ではない
# - RDDデータは介入基準が明確にhistoryの水準によって決められている


# プロット作成
# --- RCTデータ
p1 <-
  male_data %>%
    mutate(history_log_grp = round(history_log / 0.1, 0) * 0.1) %>%
    group_by(history_log_grp, segment) %>%
    summarise(visit = mean(visit),
              N = n()) %>%
    ungroup() %>%
    filter(N > 10) %>%
    ggplot(aes(y = visit, x = history_log_grp, shape = segment, size = N)) +
    geom_point() +
    ylim(0, NA) +
    theme_bw() +
    xlab("log(history)") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(1, 2, 1, 1, "cm"))

# プロット作成
# --- RDDデータ
p2 <-
  rdd_data %>%
    group_by(history_log_grp, segment) %>%
    summarise(visit = mean(visit),
              N = n()) %>%
    ungroup() %>%
    filter(N > 10) %>%
    ggplot(aes(y = visit, x = history_log_grp, shape = segment, size = N)) +
    geom_point() +
    geom_vline(xintercept = 5.5, linetype = 2) +
    ylim(0, NA) +
    theme_bw() +
    xlab("log(history)") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(1, 2, 1, 1, "cm"))

# プロット表示
grid.arrange(p1, p2)


# 2 テーブル集計 ----------------------------------------------------

# RCTデータ
rct_data_table <-
  male_data %>%
    filter(history_log > 5, history_log < 6) %>%
    group_by(treatment) %>%
    summarise(count = n(),
              visit = mean(visit)) %>%
    ungroup()

# RDDデータ
rdd_data_table <-
  rdd_data %>%
    group_by(treatment) %>%
    summarise(count = n(),
              visit = mean(visit))

# データ確認
rct_data_table %>% print()
rdd_data_table %>% print()

# 来訪率の差
# --- rct：0.08  rdd：0.13
# --- 想定どおりRDDの方が高い
round(rct_data_table$visit[2] - rct_data_table$visit[1], 2)
round(rdd_data_table$visit[2] - rdd_data_table$visit[1], 2)


# 3 線形回帰モデルによる分析 -----------------------------------------------------

# モデルデータ
rdd_lm_data <-
  rdd_data %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# モデル構築
rdd_lm_reg <-
  lm(visit ~ treatment + history_log, data = rdd_lm_data) %>%
    tidy() %>%
    filter(term == "treatment")

# 結果確認
# --- 0.114
rdd_lm_reg %>% print()


# 4 非線形回帰モデルによる分析 -----------------------------------------------------

# ＜ポイント＞
# - {rddtools}を使って非線形回帰を利用したRDDを簡単に行うことができる


# モデルデータ作成
# --- cutpoint引数にカットオフの値を入力する
nonlinear_rdd_data <-
  rdd_data %$%
    rdd_data(y = visit,
             x = history_log,
             cutpoint = 5.5)

# データ確認
nonlinear_rdd_data %>% class()
nonlinear_rdd_data %>% glimpse()

# モデル構築
# --- order引数でべき乗の値を指定する
nonlinear_rdd_ord4 <- rdd_reg_lm(rdd_object = nonlinear_rdd_data, order = 4)

# 結果確認
nonlinear_rdd_ord4 %>% print()
nonlinear_rdd_ord4$coefficients['D']

# プロット作成
nonlinear_rdd_ord4 %>% plot()


# 4 非線形回帰モデルによる分析 -----------------------------------------------------

# プロットデータ作成
bound_list <- 2:100 / 100
result_data <- data.frame()
for (bound in bound_list) {
  out_data <-
    rdd_data %>%
      filter(between(history_log, 5.5 - bound, 5.5 + bound)) %>%
      group_by(treatment) %>%
      summarise(count = n(),
                visit_rate = mean(visit),
                sd = sd(visit)) %>%
      ungroup()

  late <- out_data$visit_rate[2] - out_data$visit_rate[1]
  N <- sum(out_data$count)
  se <- sqrt(sum(out_data$visit_rate^2)) / sqrt(N)
  result_data <- rbind(result_data, data.frame(late, bound, N, se))
}

# プロット作成
result_data %>%
  ggplot(aes(y = late, x = bound)) +
  geom_ribbon(aes(ymax = late + 1.96 * se,
                  ymin = late - 1.96 * se), fill = "grey70") +
  geom_line() +
  theme_bw()


## non-parametric RDDの実行
rdd_result <-
  rdd_data %>%
    RDestimate(formula = visit ~ history_log,
               cutpoint = 5.5)

# 結果確認
rdd_result %>% summary()

# プロット作成
rdd_result %>% plot()

## manipulat
rdd_data %>%
  pull(history_log) %>%
  DCdensity(cutpoint = 5.5, plot = FALSE)

