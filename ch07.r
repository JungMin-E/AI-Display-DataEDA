my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("tidyverse", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("plotly", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("DT", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("corrplot", lib = my_lib, repos = "https://cloud.r-project.org")

library(tidyverse)
library(plotly)
library(DT)
library(corrplot)

# getwd() # "/Users/ijeongmin/Desktop/R"
project_root <- normalizePath(file.path(my_lib, ".."))
cd_data_path <- file.path(project_root, "data/cd.csv")
tk_data_path <- file.path(project_root, "data/tk.csv")

cd_data <- read.csv(cd_data_path)
tk_data <- read.csv(tk_data_path)

summary(cd_data)
summary(tk_data)

install.packages("DataExplorer", lib = my_lib, repos = "https://cloud.r-project.org")
library(DataExplorer)

plot_histogram(cd_data)
plot_histogram(tk_data)

# 정규성 검정
shapiro.test(cd_data$cd)
shapiro.test(tk_data$TK)

ggplot(cd_data, aes(sample=cd)) +
    stat_qq() + 
    stat_qq_line()

ggplot(tk_data, aes(sample=TK)) +
    stat_qq() +
    stat_qq_line()

# 이상치 boxplot.stats를 이용해서 확인
outliers <- boxplot.stats(cd_data$cd)$out

Q1 <- quantile(tk_data$TK, 0.25)
Q3 <- quantile(tk_data$TK, 0.75)
IQR <- Q3 - Q1

outliers <- tk_data$TK[tk_data$TK < Q1 - 1.5 * IQR | tk_data$TK > Q3 + 1.5 * IQR]

boxplot(tk_data$TK)

install.packages("skimr", lib = my_lib, repos = "https://cloud.r-project.org")
library(skimr)

cd_data |>
  select(Tpr, exp, dev, cd) |>
  skim()

# CD vs 공정변수 시각화
ggplot(cd_data, aes(x=Tpr, y=cd, color=material)) + 
    geom_plot() +
    geom_smooth(method="lm")
    geom_hline(yinterceot=40) +
    facet_wrap(~exp, labeller=label_both) + 
    labs(title="코팅 두께와 노광량에 따른 CD 변화",
        x="코팅 두께",
        y="CD")

# Thickness 데이터 기초 통계
# Thickness vs 공정변수 시각화
ggplot(tk_data, aes(x=Tpr, y=TK, color=material)) +
    geom_plot() +
    geom_smooth(method="lm") +
    geom_hline(yintercept=2.0) +
    scale_x_countinous(breaks=seq(2.5, 3.5, 0.25),
                        labels=seq(2.5, 3.5, 0.25)) +
    facet_wraps(~dev, labeller=label_both) +
    labs(title="코팅 두께와 현상 시간에 따른 Thickness 변화",
        x="코팅 두께 (um)",
        y="CD (um)")

p1 <- cd_data |> 
    ggplot(aes(x=exp, y=cd)) +
    geom_boxplot(aes(fill=material),alpha=0.5) +
    geom_hline(yintercept=40, linetype='dashed', color='red', linewidth=1) +
    facet_wrap(Tpr ~ dev, ncol=3, labeller=label_context) + 
    ggtitle("Box Plot of CD") +
    xlab('exp[mj]') +
    ylab('CD[um]') +
    theme_bw() +
    theme(title=element_text(size=20),
        axis.title=element_text(size=20),
        axis.test.y=eleement_text(size=15),
        axis.test.x=eleement_text(size=15),
        legend.text=element_text(size=12)) +
    theme(strip.text=element_text(size=15, colour="blue"))

p1

p2 <- tk_data |> 
    ggplot(aes(x=exp, y=TK)) +
    geom_boxplot(aes(fill=material),alpha=0.5) +
    geom_hline(yintercept=2.0, linetype='dashed', color='red', linewidth=1) +
    facet_wrap(Tpr ~ dev, ncol=3, labeller=label_context) + 
    ggtitle("Box Plot of TK") +
    xlab('exp[mj]') +
    ylab('CD[um]') +
    theme_bw() +
    theme(axis.title=element_text(size=20),
        title=element_text(size=20),
        axis.test.y=eleement_text(size=15),
        axis.test.x=eleement_text(size=15),
        legend.text=element_text(size=12)) +
    theme(strip.text=element_text(size=15, colour="blue"))

p2
