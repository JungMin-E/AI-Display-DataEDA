my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("palmerpenguins", lib = my_lib, repos = "https://cloud.r-project.org")

library(palmerpenguins)
library(tidyverse)
penguins |>
    ggplot(aes(x = flipper_length_mm, body_mass_g)) +
    geom_point(aes(color=species, shape=species), size=3, alpha=0.7) +
    scale_color_brewer(palette="Dark2") + 
    labs(title="Penguin Size, Palmer Station LTER",
    subtitle="Flipper length and body mass for Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Penguin species",
    shape = "Penguin species") + 
    #추세선
    geom_smooth(method="lm", se=F, aes(color=species))
    #그래프를 나누어서 그리기 facet_wrap()

# BoxPlot 사용하기
penguins |>
    ggplot(aes(x = flipper_length_mm, body_mass_g)) +
    geom_boxplot(aes(color=species), width=0.3, show.legend=FALSE) +
    #BoxPlot에서 점들을 좌우로 흩으려서 볼수 있게 하는 jitter 
    geom_jitter(aes(color=species), alpha=0.5, show.legend=FALSE,
                position=position_jitter(width=0.2, seed=0)) +
    scale_fill_brewer(palette="Dark2") + 
    labs(title="Penguin Size, Palmer Station LTER",
    subtitle="Flipper length and body mass for Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Penguin species",
    shape = "Penguin species") 

# 3. 히스토그램 (Histogram) -  수정: x 데이터만 필요함!
penguins |>
    ggplot(aes(x = flipper_length_mm)) + # body_mass_g 제거 완료!
    geom_histogram(aes(fill=species), alpha=0.5, position='identity') +
    scale_fill_brewer(palette="Dark2") + # 수정: color(테두리)가 아니라 fill(채우기)로 변경!
    labs(title="Penguin Size", x = "Flipper length (mm)", y = "Count")

# 막대 그래프 (Bar Chart)
df |> 
    pivot_longer(-1, names_to="space", values_to="value") |>
    mutate(space=as.numeric(space)) |>
    filter(angle %in% c(0, 10, 15)) |>
    mutate(angle=as.factor(angle)) |>
    ggplot(aes(x=space, y=value*100, fill=angle)) +
    geom_col(position="dodge") + 
    geom_text(aes(label=value*100, y=value*100+3), position=position_dodge(0.5)) + 
    labs(y="normalized value(%)", x="space 이격거리")
