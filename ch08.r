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

# 드라이 에치 공정
etch_data <- read.csv("/Users/ijeongmin/Desktop/R/AD/data/dry_etch.csv")
head(etch_data)
summary(etch_data) #기초 통계량 확인하기

# Recipe별 기술통계량
etch_data |>
    grpup_by(recipe) |>
    summarise(
        mean_cd=mean(cd),
        sd_cd=sd(cd),
        cv=sd_cd/mean_cd^100,
        min_cd=min(cd),
        max_cd=max(cd)
    )

# Recipe 별 CD 분포 시각화
# CD 산포도를 최소로 맞추는 것이 매우 중요
ggplot(etch_data, aes(x=recipe, y=cd, fill=recipe)) +
    geom_boxplot() +
    labs(title="Recipe별 CD 분포",
        x="Recipe",
        y="CD(um)") +
    theme(legend.position="none")

# Recipe별 Uniformity 계산 및 시각화
# Uniformity가 좋다 = 수치가 낮다
uniformity_data <- etch_data |>
    group_by(recipe) |>
    summarise(
        uniformity = (max(cd)-min(cd)/max(cd) + min(cd))*100
    )
    ggplot(uniformity_data, aes(x=recipe, y=uniformity, fill=recipe)) +
    geom_bar(stat="identity") + 
    labs(title="Recipe별 Uniformity",
        x="Recipe",
        y="Uniformity(%)") +
    theme(legend.position="none")

# 공정변수와 CD의 상관관계 분석
cor_matrix <- etch_data |>
    select(gas_cf4, gas_o2, pressure, time, cd) |>
    cor()

corrplot(cor_matrix, method="color",
        type="upper", order="hclust",
        addcoef.col="black",
        t1.col="black", tl.srt=45)

# x, y 위치에 따른 CD 분포 시각화
ggplot(etch_data, aes(x=x1, y=y1, color=cd)) +
    geom_point(size=3) + #산점도
    facet_wrap(~recipe) + #그래프를 나누어서 그리기 위한 용도
    scale_color_viridis_c() +
    labs(title="Recipe별 위치에 따른 CD 분포",
        x="X position",
        y="Y position") +
    theme_minimal()

# 최적 조건 도출
# 1. Uniformity가 가장 우수한 Recipe: R5
# 2. 주요 영향 인자: CF4 gas, Pressure
# 3. 최적 조건 CF4 gas: 150, O2:30, Pressure:200, Time:90


# 데이터분포 Recipe별 density 그리기
p1 <- etch_data |>
    ggplot(aes(x=cd)) + 
    geom_histogram(aes(col=recipe)) +
    facet_wrap(recipe ~., ncol=5) +
    theme(legened.position="none")

p1 
# 데이터분포 Recipe별 boxplt 그리기
p2 <- etch_data |>
    ggplot(aes(x=cd)) + 
    geom_density(aes(col=recipe)) +
    facet_wrap(recipe ~., ncol=5) +
    theme(legened.position="none")

p2 

# Etch 산포 Violin, Boxplot으로 보기
p3 <- etch_data |>
    ggplot(aes(x=recipe, y=cd)) + 
    geom_violin(aes(col=recipe), trim=FALSE) +
    geom_boxplot(width=0.2) +
    scale_fill_viridis_d() +
    theme(legened.position="none")

p3 

# 공정 변수 Etch 영향도 분석 - 산점도
p4 <- etch_data |>
    select(cd, gas_cf4, gas_o2, pressure, time) |>
    mutate(cf4_ratio=gas_cf4/(gas_cf4+gas_o2)) |> #cf4_ratio mutate로 만들기 
    select(-gas_cf4, -gas_o2) |> #gas_cf4, gas_o2값 제거하기 위해 -사용
    pivot_longer(-1, names_to="condition", values_to="value") |>
    ggplot(aes(x=value, y=cd, col=condition)) +
    geom_point() +
    facet_wrap(~condition, scales="free")

p4 

# Etch 맵(geom_raster)
p5 <- etch_data |>
    ggplot(aes(x1,y1)) +
    geom_title(aes(fill=cd)) +
    scale_fill_continuous(low="green", high="blue") +
    facet_grid(, ~recipe, labeller=label_value)

p5 
# 위에 그래프와 동일
p6 <- etch_data |>
    ggplot(aes(x1,y1)) +
    geom_raster(aes(fill=cd)) +
    scale_fill_continuous(low="green", high="blue") +
    facet_grid(, ~recipe, labeller=label_value)

p6 
