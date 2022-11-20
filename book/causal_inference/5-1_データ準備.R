#***************************************************************************************
# Title     : 効果検証入門
# Chapter   : 5章 回帰不連続デザイン
# Theme     : 5-1 データ準備
# Date      : 2022/11/19
# Page      : P169 - P171
# URL       : https://github.com/ghmagazine/cibook
#***************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 データ保存


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)


# データロード
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")


# 1 データ準備 ---------------------------------------------------------------------

# (3) ルールによるメールの配信を行ったログを作成
## データの整形とrunning variableの追加
male_data <-
  email_data %>%
    filter(segment %in% c("Mens E-Mail","No E-Mail")) %>%
    mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0),
           history_log = log(history))

## cut-off の値を指定
threshold_value <- 5.5

## ルールによる介入を再現したデータを作成
## cut-offよりrunning variableが大きければが配信されたデータのみ残す
## 逆の場合には配信されなかったデータのみ残す
## running variableを0.1単位で区切ったグループわけの変数も追加しておく
rdd_data <-
  male_data %>%
    mutate(history_log_grp = round(history_log/0.1,0)*0.1) %>%
    filter(((history_log > threshold_value) &
              (segment == "Mens E-Mail")) |
             (history_log <= threshold_value) &
             (segment == "No E-Mail"))


# 2 データ保存 ----------------------------------------------------------------

# データ保存
male_data %>% write_csv("csv/male_data.csv")
rdd_data %>% write_csv("csv/rdd_data.csv")


