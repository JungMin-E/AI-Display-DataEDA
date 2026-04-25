my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("palmerpenguins", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("patchwork", lib = my_lib, repos = "https://cloud.r-project.org")

library(palmerpenguins)
library(tidyverse)
library(patchwork)

# Violin
warpbreaks |>
    ggplot(aes(x=tension, y=breaks, fill=tension)) +
    geom_violin(trim=F) +
    geom_boxplot(width=0.07)

x <- c(rnorm(200, mean = -2,1.5),
        rnorm(200, mean = 0, sd = 1),
        rnorm(200, mean = 2, 1.5))

group <- c(rep("A", 200), rep("B", 200), rep("C", 200))
df <- data.frame(x, group)

cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

# Histogram
p1 <- df |>
    ggplot(aes(x = x,fill = group)) + 
    geom_histogram(alpha=0.7, position="identity") +
    scale_fill_manual(values=cols) + 
    theme(legend.position="top") + 
    labs(title='Histogram')

p2 <- df |>
    ggplot(aes(x = x, fill = group)) + 
    geom_density(alpha=0.7, position="identity") + 
    scale_fill_manual(values=cols) +
    labs(title="Density")

p1+p2

# 그래프 분할하기 
# facet_grid
df <- data.frame(team=c('A', 'A', 'A', 'A', 'B', 'B', 'B', 'B'),
                position=c('G', 'G', 'F', 'F', 'G', 'G', 'G', 'G'),
                points=c(8, 14, 20, 22, 25, 29, 30, 31),
                assists=c(10, 5, 5, 3, 8, 6, 9, 12))

p1 <- ggplot(df, aes(assists, points, col=position)) +
    geom_point() + 
    facet_grid(position~team)
    
# patchwork 패키지: ggplot 그래프들을 레고 블록처럼 내 맘대로 조립하게 해주는 마법의 도구
# ggsave("test.png") -> test.png폴더로 저장이 됨



# Sankey 그래프
# 흐름이나 변화를 시각화하는데 적합, 노드 간의 연결과 크기를 표현

install.packages("remotes", lib = my_lib, repos = "https://cloud.r-project.org")
library(remotes, lib.loc = my_lib)
install_github("davidsjoberg/ggsankey", lib = my_lib)
library(ggsankey, lib.loc = my_lib)

df <- mtcars |> make_long(cyl, vs, am, gear, carb) #make_long함수에 (데이터 변수 값) 넣기

df |> ggplot(aes(x=x,
                next_x = next_x,
                node = node,
                next_node = next_node,
                fill = factor(node),
                label = node)) + 

geom_sankey(flow.alpha=0.5, node.color=1) + 
geom_sankey_label(size=3.5, color = 1, fill = "white") + 
scale_fill_viridis_d() + 
theme_sankey(base_size = 16) + 
theme(legend.position = "none")

# Pair그래프
# 여러 변수 간의 관계를 한번에 표현, 산점도와 히스토그램의 조합 

install.packages("GGally", lib = my_lib, repos = "https://cloud.r-project.org")
library(GGally)

iris
ggpairs(iris, columns = 1:4, aes(color=Species, alpha=0.5),
        upper = list(continuous = "points"))

# Raincloud plot
# 특징: 박스플롯, 밀도플롯, 산점도를 결합, 데이터의 분포를 종합적으로 표현
# dev.off() # 그래프지우기

install_github("davidsjoberg/ggdlist", lib = my_lib)
library(ggdlist, lib.loc = my_lib)
install.packages("tidyquant", lib = my_lib, repos = "https://cloud.r-project.org")
library(ggdlist)
library(tidyquant)

npg |> 
    filter(cyl %in% c(4, 6, 8)) |>
    ggplot(aes(x=factor(cyl), y=hwy, fill=factor(cyl))) + #factor()범주형 변수로 바꾸는
    ggdlist::stat_halfeye(
        adjust=0.5, 
        justification = -.2,
        .width = 0,
        width=0.4,
        point_colour = NA
    ) + 
    ggdlist::stat_dots(
        side="left",
        justification = 1.1,
        binwidth = .25
    ) +
    geom_boxplot(
        width=.12,
        outlier.color = NA,
        alpha=0.5
    ) + 
    scale_fill_tq() + 
    theme_tq() + 
    labs(title="Raincloud Plot",
        subtitle="Showing the bi-modal distribution of 6 cylinder vehicle",
        x="engine size",
        y="highway fuel economy",
        fill="cylinders")

# Polar Contour Graph   
# 극좌표계를 이용한 등고선 그래프, 방향성 데이터 표현에 적합
