# ***********************************************************************************************
# Library   : ivreg
# Title     : Two-Stage Least-Squares Regression with Diagnostics
# Date      : 2022/06/04
# URL       : https://john-d-fox.github.io/ivreg/articles/Diagnostics-for-2SLS-Regression.html
# ***********************************************************************************************


# ＜概要＞
# - {ivreg}は回帰診断を行うための関数群が実装されている


# ＜目次＞
# 0 準備
# 1 モデル構築
# 3 伝統的な診断プロット


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ivreg)
library(modelsummary)
library(psych)
library(car)


# データロード
data("Kmenta", package = "ivreg")

# 確認
Kmenta %>% as_tibble()
Kmenta %>% pairs.panels()


# 1 モデル構築 -----------------------------------------------------------------

# モデル構築
# --- 需要モデル
# --- 供給モデル
deq <- ivreg(Q ~ P + D | D + F + A, data = Kmenta)
seq <- ivreg(Q ~ P + F + A | D + F + A, data = Kmenta)

# サマリー0
deq %>% summary()
seq %>% summary()

#
deq %>% summary()
seq %>% summary()


# 2 サマリーの内容確認 ----------------------------------------------------------

# ＜ポイント＞
# Weak instruments
#

# Wu-Hausman検定
# - 2SLS回帰に適用される内因性の検定

# Sargan検定
# - 過剰識別の検定
# - 推定する操作変数が係数よりも多い式では操作変数が係数の値に関する矛盾する情報を提供する可能性がある


deq %>% summary() %>% use_series(diagnostics)
seq %>% summary() %>% use_series(diagnostics)


# 3 診断プロット ------------------------------------------------------------------

# 診断プロットの出力
# --- 見やすいものではない
par(mfrow = c(2, 2))
deq %>% plot()


# QQプロット
par(mfrow = c(1, 2))
deq %>% car::qqPlot()
deq %>% influencePlot()




Kmenta1 <- Kmenta
Kmenta1[20, "Q"] <- 95
deq1 <- update(deq, data=Kmenta1)
compareCoefs(deq, deq1)

deq1 %>% qqPlot()
deq1 %>% influencePlot()

deq1 %>% avPlots()

deq1.20 <- update(deq1, subset = -20)
compareCoefs(deq, deq1, deq1.20)


H <- cbind(hatvalues(deq1), hatvalues(deq1, type="both"),
           hatvalues(deq1, type="maximum"))
colnames(H) <- c("stage2", "geom.mean", "maximum")
head(H)

scatterplotMatrix(H, smooth=FALSE)

cbind(dfbeta(deq1)[20, ], coef(deq1) - coef(deq1.20))

c(influence(deq1)$sigma[20], sigma(deq1.20))

crPlots(deq, smooth=list(span=1))

ceresPlots(deq, smooth=list(span=1))

library("effects")


plot(predictorEffects(deq, residuals=TRUE),
     partial.residuals=list(span=1))


deq2 <- update(deq, . ~ I((P - 85)^4/10^5) + D)
crPlots(deq2, smooth=list(span=1))

plot(predictorEffects(deq2, residuals=TRUE),
     partial.residuals=list(span=1))

plot(fitted(deq), rstudent(deq))
abline(h=0)

spreadLevelPlot(deq, smooth=list(span=1))

with(Kmenta, plot(Q, Q^-2.5))
abline(lm(Q^-2.5 ~ Q, data=Kmenta))

ncvTest(deq)

ncvTest(deq, var = ~ P + D)

summary(deq, vcov=sandwich::sandwich)

SEs <- round(cbind(sqrt(diag(sandwich::sandwich(deq))),
                   sqrt(diag(vcov(deq)))),
             4)
colnames(SEs) <- c("sandwich", "conventional")
SEs


Kmenta2 <- Kmenta[, c("D", "F", "A")]
set.seed(492365) # for reproducibility
Kmenta2 <- within(Kmenta2, {
    EQ <- 75.25 + 0.1125*D + 0.1250*F + 0.225*A
    EP <- 85.00 + 0.7500*D - 0.5000*F - 0.900*A
    d1 <- rnorm(20)
    d2 <- rnorm(20)
    v1 <- 2*d1
    v2 <- -0.5*v1 + d2
    w <- 3*(EQ - min(EQ) + 0.1)/(max(EQ) - min(EQ))
    v1 <- v1*w # inducing nonconstant variance
    Q <- EQ + v1
    P <- EP + v2
})

with(Kmenta2, plot(EQ, v1))


deq2 <- update(deq, data=Kmenta2)
summary(deq2)


spreadLevelPlot(deq2)


ncvTest(deq2)


SEs2 <- round(cbind(sqrt(diag(sandwich::sandwich(deq2))),
                   sqrt(diag(vcov(deq2)))),
             4)
colnames(SEs2) <- c("sandwich", "conventional")
SEs2


set.seed <- 869255 # for reproducibility
b.deq2 <- Boot(deq2)
summary(deq2, vcov.=vcov(b.deq2))


confint(b.deq2)



deqw <- update(deq, data=Kmenta2, weights=1/w)
summary(deqw)



ncvTest(deqw)


plot(fitted(deqw), rstudent(deqw))

outlierTest(deqw)


sqrt(vif(deq))


mcPlots(deq)


deq.mm <- update(deq1, method = "MM")
summary(deq.mm)

compareCoefs(deq, deq1, deq1.20, deq.mm)


weights(deq.mm, type = "robustness")

influencePlot(deq.mm)
