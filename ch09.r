my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("tidyverse", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("plotly", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("DT", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("corrplot", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("patchwork", lib = my_lib, repos = "https://cloud.r-project.org")


library(tidyverse)
library(plotly)
library(DT)
library(corrplot)

# 스퍼터링 공정의 원리: 진공 챔버내에서 가속된 이온을 타겟에 충돌시켜 그 충격으로 튀어나온 원자들이 기판 표면에 달라붙어 얇은 막을 형성하는 물리적 기상 증착 방식의 대표적인 기술.
# 스퍼터링 장비 구조와 특성: Planner Type, Rotary Type
# Planner Type: 마그네트론 방식으로 플라즈마 밀도 향상, 단순한 구조로 제작 용이 직사각형 또는 원형 형태의 평면 구조
# Rotary Type: 원통형 회전 타겟 구조, 내부 마그넷 고정 방식, 타겟 회전을 통한 열 분산, 연속 생산에 적합한 구조
# 공정변수와 박막 특성: 아르곤 가스의 압력은 박막의 밀도와 응력에 큰 영향을 줌 저압상태에서는 밀도가 높고 단단한 박막이 형성. 고압상태에서는 원자들이 가스와 충돌하여 에너지를 잃음 이로 인해서 박막의 밀도가 낮아지고 표면이 거칠어질 수 있다.

# 4-Point Probe 측정 원리: 4개의 탐침이 일렬로 배열된 측정 장비, 탐침 간격이 통일하고 외부 2개 탐침: 전류 인가/수집 내부 2개의 탐침: 전압 측정

# Sheet Resistance 측정방법 1.외부탐침을 통해 일정 전류 인가 2. 내부 탐침에서 전압차 측정 3. Sheet Resistance 계산

srm_data <- read.csv('/Users/ijeongmin/Desktop/R/AD/data/sputter_srm.csv')
thickness_data <- read.csv('/Users/ijeongmin/Desktop/R/AD/data/sputter_thickness.csv')

srm_data |>
    mutate(recipe=case_when(
        id == 1 ~ "R1",
        id == 2 ~ "R1",
        id == 3 ~ "R2",
        id == 4 ~ "R2",
        id == 5 ~ "R3",
        id == 6 ~ "R3",
    )) |>

summary(thickness_data)

rs_uniformity <- srm_data |>
    group_by(recipe) |>
    summarise(
        uniformity = (max(Rs) - min(Rs) / max(Rs) + min(Rs))*100
    )

thickness_uniformity <- thickness_data |>
    group_by(recipe) |>
    summarise(
        unformity = (max(thickness) - min(thickness)) / (max(thickness) + min(thickness)) * 100
    )

bind_rows(
    mutate(rs_unformity, type="Sheet Resistance"),
    mutate(thickness_unformity, type="Thickness")) |>
    ggplot(aes(x=recipe, y=unformity, fill=type)) +
    geom_bar(stat="identity", position="dodge") +
    labs(title="recipe별 uniformity 비교",x="recipe",y="Uniformity(%)")

# boxplot
srm_data |>
    ggplot(aes(x=recipe, y=Rs, fill=recipe)) +
    geom_boxplot() +
    labs(title="recipe별 Sheet 저항 분포",x="recipe",y="Rs(%)") +
    theme(legend.positon="none")

srm_data |>
    mutate(recipe=case_when(
        id == 1 ~ "R1",
        id == 2 ~ "R1",
        id == 3 ~ "R2",
        id == 4 ~ "R2",
        id == 5 ~ "R3",
        id == 6 ~ "R3",
    )) |>
    ggplot(aes(x=Rs)) +
    geom_histogram(aes(col=recipe)) +
    facet_wrap(recipe ~., ) +
    theme(legend.positon="none")

thickness_data |>
    mutate(recipe=case_when(
        id == 1 ~ "R1",
        id == 2 ~ "R1",
        id == 3 ~ "R2",
        id == 4 ~ "R2",
        id == 5 ~ "R3",
        id == 6 ~ "R3",
    )) |>
    ggplot(aes(x=thickness)) + 
    geom_histogram(aes(col=recipe)) + 
    facet_wrap(recipe ~., ) + 
    theme(legend.position="none")

# recipe별 density 그리기
p1 <- srm_data |>
    ggplot(aes(x=Rs)) + 
    geom_histogram(aes(col=recipe)) + 
    geom_vline(xintercept=0.2, linetype="dashed") +
    facet_wrap(recipe ~., ) +
    theme(legend.position="none")

p2 <- thickness_data |>
    ggplot(aes(x=thickness)) + 
    geom_histogram(aes(col=recipe)) + 
    facet_wrap(recipe ~., ) + 
    theme(legend.position="none")

p1/p2

srm_data |>
    mutate(recipe=case_when(
        id == 1 ~ "R1",
        id == 2 ~ "R1",
        id == 3 ~ "R2",
        id == 4 ~ "R2",
        id == 5 ~ "R3",
        id == 6 ~ "R3",
    )) -> srm_data

thickness_data |>
    mutate(recipe=case_when(
        id == 1 ~ "R1",
        id == 2 ~ "R1",
        id == 3 ~ "R2",
        id == 4 ~ "R2",
        id == 5 ~ "R3",
        id == 6 ~ "R3",
    )) -> thickness_data

p5 <- srm_data |>
    ggplot(aes(x=recipe, y=Rs, fill=recipe)) + 
    geom_boxplot() + 
    facet_hline(yintercept=0.2, linetype="dashed") + 
    theme(legend.position="none")

p6 <- thickness_data |> 
    ggplot(aes(x=recipe, y=Rs, fill=recipe)) + 
    geom_boxplot() +
    facet_hline(yintercept=0.2, linetype="dahsed") +
    theme(legend.position="none")

library(patchwork)
p5/p6

# Violin
p7 <- srm_data |>
    ggplot(aes(x=recipe, y=thickness, fill=recipe)) + 
    geom_violin() + 
    facet_hline(yintercept=0.2, linetype="dashed") + 
    theme(legend.position="none")

p8 <- thickness_data |> 
    ggplot(aes(x=recipe, y=thickness, fill=recipe)) + 
    geom_violin() +
    theme(legend.position="none")

p7/p8
    
p9 <- srm_data |>
    ggplot(aes(x1,y1)) + 
    geom_tile(aes(fill=Rs)) +
    scale_fill_continuous(low="green", high="blue") +       
    facet_wrap(~recipe, labeller=label_both)

p10 <- thickness_dat |>
    ggplot(aes(x1, y1, size=thickness, col=thickness)) + 
    geom_point() + 
    scale_color_continuous(low="green", high="blue") + 
    facet_wrap(~recipe, labeller=label_both) + 
    labs(x="", y="") +
    theme(legend.position="none", axis.text.x=element_blanck(), axis.text.y=element_blanck())

# 분석결과 Rs < 0.2옴을 만족시키기 위해서는 recipe 3을 사용해야함.
# Metal의 두께가 증가할 수록 면저항은 감소
