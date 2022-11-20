#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 4章 差分の差分法(DID)とCausal Impact
# Theme     : 4-1 コレラデータの作成
# Date      : 2022/11/19
# Page      : P137 - P147
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜概要＞
# - DIDでは介入を受けるグループ/受けないグループで前後比較を行い、その結果の差を見る


# ＜目次＞
# 0 準備
# 1 カテゴリ別の集計
# 2 データを変化ので確認


# 0 準備 ---------------------------------------------------------------------

library(tidyverse)
library(broom)
library(Ecdat)
library(miceadds)
library(CausalImpact)


# データロード
JS_df <- read_csv("csv/JS_df.csv")


# 1 カテゴリ別の集計 ---------------------------------------------------------

# ＜ポイント＞
# - DIDでは集計データに対して効果測定を行う
#   --- 効果測定の単位までデータを集計


# カテゴリ別の集計
JS_sum <-
  JS_df %>%
    group_by(company, LSV, year) %>%
    summarise(death = sum(death), .groups = "drop") %>%
    ungroup()

# 確認
JS_sum %>% print()


# 2 データを変化ので確認 ------------------------------------------------

# 企業ごとの時点比較
# --- gap: 企業ごとの時点の差
# --- gap_rate: 変化率
JS_grp_summary <-
  JS_sum %>%
    mutate(year = paste("year", year, sep = "_")) %>%
    spread(year, death) %>%
    mutate(gap = year_1854 - year_1849,
           gap_rate = year_1854 / year_1849 - 1)

# データ確認
JS_grp_summary %>% print()


# 企業ごとの時点比較
# --- gap: 企業ごとの時点の差（対数変換）
JS_grp_summary_ln <-
  JS_sum %>%
    mutate(year = paste("year", year, sep = "_"),
           death = log(death)) %>%
    spread(year, death) %>%
    mutate(gap = year_1854 - year_1849)

# データ確認
JS_grp_summary_ln %>% print()


# 2 プロットによるDIDのイメージ --------------------------------------------

# ＜ポイント＞
# - DIDのイメージをプロットで表現
#   --- (1)は非介入グループで起きた死者数の変化を示す
#   --- (2)は介入グループで介入が行われなかった際に期待される変化（平行トレンド仮定）
#   --- (3)は介入による変化
#   --- (2) + (3)を介入効果とみなす


# プロット作成
JS_sum %>%
  ggplot(aes(y = death, x = year, shape = company)) +
  geom_point(size = 2) +
  geom_line(aes(group = company), linetype = 1) +
  ylim(2000, 4250) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) +
  annotate("text", x = 1854.2, y = 2400, label = "(1)") +
  annotate("text", x = 1854.2, y = 3904 + 197*0.6, label = "(2)") +
  annotate("text", x = 1854.2, y = 3300, label = "(3)") +
  annotate("segment", # for common trend in treatment group
           x = 1849, xend = 1854,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(length = unit(.2,"cm")),
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1849, xend = 1854,
           y = 2261, yend = 2261,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for parallel trend
           x = 1849, xend = 1854,
           y = 3904, yend = 3904,
           size = 0.1,
           linetype = 2) +
  annotate("segment", # for (1)
           x = 1854, xend = 1854,
           y = 2261, yend = 2458,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (2)
           x = 1854, xend = 1854,
           y = 3904, yend = 3904 + 197,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90)) +
  annotate("segment", # for (3)
           x = 1854, xend = 1854,
           y = 3904, yend = 2547,
           arrow = arrow(ends = "both",
                         length = unit(.1,"cm"),angle = 90))


# 4 回帰分析を用いたDID ------------------------------------------------------

# ＜集計データ＞
# - LSV  : 企業のダミーフラグ
# - D1854: 時点のダミーフラグ
# - D1854:LSVで相互作用を表現（DIDの効果）


# モデル用データ
JS_model_data <-
  JS_sum %>%
    mutate(D1854 = if_else(year == 1854, 1, 0))

# 集計データによるDID
# --- 4サンプルしかないので標準誤差が定義されない
JS_did      <- lm(death ~ LSV + D1854 + D1854:LSV, data = JS_model_data) %>% tidy()
JS_did_area <- lm(death ~ LSV + area + D1854 + D1854:LSV, data = JS_model_data) %>% tidy()

# 結果確認
JS_did %>% print()
JS_did_area %>% print()

# 元データによるDID
JS_did_log      <- lm(log(death) ~ LSV + D1854 + D1854:LSV, data = JS_model_data) %>% tidy()
JS_did_area_log <- lm(log(death) ~ LSV + area + D1854 + D1854:LSV, data = JS_model_data) %>% tidy()

# 結果確認
JS_did_log %>% print()
JS_did_area_log %>% print()
