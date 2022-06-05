# ***********************************************************************************************
# Library   : ivreg
# Title     : Reference
# Function  : ivreg
# Date      : 2022/06/04
# URL       : https://john-d-fox.github.io/ivreg/
# **********************************************************************************************


# ＜概要＞
# - 2段階最小二乗（2SLS）による操作変数回帰を構築する
# - これは、楽器の数がリグレッサーの数と等しい場合の直接操作変数推定と同等です。
# - M推定(2SM)およびMM推定(2SMM)に基づいたロバスト回帰推定量も提供される


# ＜フォーミュラ＞
# ivreg(
#   formula,
#   instruments,
#   data,
#   subset,
#   na.action,
#   weights,
#   offset,
#   contrasts = NULL,
#   model = TRUE,
#   y = TRUE,
#   x = FALSE,
#   method = c("OLS", "M", "MM"),
#   ...
# )


# ＜引数＞
# formula
# 回帰関係と機器の式の仕様。どちらの楽器も欠落しており、数式はy〜x1 +x2|のように3つの部分に分かれています。
# z1 + z2 + z3（推奨）または式はy〜x1 + x2であり、計測器は片側式〜z1 + z2 + z3（下位互換性のみ）です。

# data
# - モデル内の変数を含むオプションのデータフレーム。デフォルトでは、変数は式の環境から取得されます。

# subset
# - モデルのフィッティングに使用される観測値のサブセットを指定

# na.action
# - データにNAが含まれている場合に何が起こるかを示す関数
# - デフォルトはna.actionオプションで設定されます。

# weights
# フィッティングプロセスで使用されるウエイト

# offset
# フィッティング中に含まれる事前に既知のコンポーネントを指定するために使用できるオプションのオフセット。

# コントラスト
# オプションのリスト。 model.matrix.defaultのcontrasts.argを参照してください。

# method
# - ステージ1および2の回帰を適合させるために使用される方法
# - 後者の2つのロバスト回帰方法が実装されています。 MASSパッケージのrlm関数を介して。
# --- 従来の2SLS回帰（デフォルト）の場合は「OLS」、
# --- M推定の場合は「M」、
# --- MM推定の場合は「MM」。



# 実行例 -----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ivreg)
library(car)

# データロード
data("CigaretteDemand", package = "ivreg")


# モデル構築
m <- ivreg(log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome), data = CigaretteDemand)

m %>% summary()


m %>% summary(vcov = sandwich::sandwich, df = Inf)


#>
#> Call:
#> ivreg(formula = log(packs) ~ log(rprice) + log(rincome) | salestax +
#>     log(rincome), data = CigaretteDemand)
#>
#> Residuals:
#>       Min        1Q    Median        3Q       Max
#> -0.611000 -0.086072  0.009423  0.106912  0.393159
#>
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)
#> (Intercept)    9.4307     1.3584   6.943 1.24e-08 ***
#> log(rprice)   -1.1434     0.3595  -3.181  0.00266 **
#> log(rincome)   0.2145     0.2686   0.799  0.42867
#>
#> Diagnostic tests:
#>                  df1 df2 statistic  p-value
#> Weak instruments   1  45    45.158 2.65e-08 ***
#> Wu-Hausman         1  44     1.102      0.3
#> Sargan             0  NA        NA       NA
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#>
#> Residual standard error: 0.1896 on 45 degrees of freedom
#> Multiple R-Squared: 0.4189,	Adjusted R-squared: 0.3931
#> Wald test: 6.534 on 2 and 45 DF,  p-value: 0.003227
#> summary(m, vcov = sandwich::sandwich, df = Inf)
#>
#> Call:
#> ivreg(formula = log(packs) ~ log(rprice) + log(rincome) | salestax +
#>     log(rincome), data = CigaretteDemand)
#>
#> Residuals:
#>       Min        1Q    Median        3Q       Max
#> -0.611000 -0.086072  0.009423  0.106912  0.393159
#>
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)
#> (Intercept)    9.4307     1.2194   7.734 1.04e-14 ***
#> log(rprice)   -1.1434     0.3605  -3.172  0.00151 **
#> log(rincome)   0.2145     0.3018   0.711  0.47729
#>
#> Diagnostic tests:
#>                  df1 df2 statistic p-value
#> Weak instruments   1  45    47.713 1.4e-08 ***
#> Wu-Hausman         1  44     1.287   0.263
#> Sargan             0  NA        NA      NA
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#>
#> Residual standard error: 0.1896 on Inf degrees of freedom
#> Multiple R-Squared: 0.4189,	Adjusted R-squared: 0.3931
#> Wald test:     2 on NA DF,  p-value: NA
#>
## ANOVA
m2 <- update(m, . ~ . - log(rincome) | . - log(rincome))
anova(m, m2)
#> Analysis of Variance Table
#>
#> Model 1: log(packs) ~ log(rprice) + log(rincome) | salestax + log(rincome)
#> Model 2: log(packs) ~ log(rprice) | salestax
#>   Res.Df    RSS Df Sum of Sq      F Pr(>F)
#> 1     45 1.6172
#> 2     46 1.6668 -1 -0.049558 0.6379 0.4287car::Anova(m)
#> Analysis of Deviance Table (Type II tests)
#>
#> Response: log(packs)
#>              Df       F   Pr(>F)
#> log(rprice)   1 10.1161 0.002662 **
#> log(rincome)  1  0.6379 0.428667
#> Residuals    45
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## same model specified by formula with three-part right-hand side
ivreg(log(packs) ~ log(rincome) | log(rprice) | salestax, data = CigaretteDemand)
#>
#> Call:
#> ivreg(formula = log(packs) ~ log(rincome) | log(rprice) | salestax,     data = CigaretteDemand)
#>
#> Coefficients:
#>  (Intercept)   log(rprice)  log(rincome)
#>       9.4307       -1.1434        0.2145
#>
# Robust 2SLS regression
data("Kmenta", package = "ivreg")
Kmenta1 <- Kmenta
Kmenta1[20, "Q"] <- 95 # corrupted data
deq <- ivreg(Q ~ P + D | D + F + A, data=Kmenta) # demand equation, uncorrupted data
deq1 <- ivreg(Q ~ P + D | D + F + A, data=Kmenta1) # standard 2SLS, corrupted data
deq2 <- ivreg(Q ~ P + D | D + F + A, data=Kmenta1, subset=-20) # standard 2SLS, removing bad case
deq3 <- ivreg(Q ~ P + D | D + F + A, data=Kmenta1, method="MM") # 2SLS MM estimation
car::compareCoefs(deq, deq1, deq2, deq3)
