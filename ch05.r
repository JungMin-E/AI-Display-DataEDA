my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("gapminder", lib = my_lib, repos = "https://cloud.r-project.org")
install.packages("htmlwidgets", lib = my_lib, repos = "https://cloud.r-project.org")

library(htmlwidgets)
library(gapminder)
library(tidyverse)


# 기본 시계열 그래프
head(gapminder)

gapminder |> 
    ggplot(aes(x=year, y=lifeExp, group=country)) + 
    geom_line()

install.packages("plotly", lib = my_lib, repos = "https://cloud.r-project.org")
library(plotly)

p <- gapminder |>
    ggplot(aes(x=year, y=lifeExp, group=country,
            text = paste0("대륙: ", continent, "\n",
                            "국가: ", country))) + 
    geom_line()

ggplotly(p, tooltip = "text")

# 하이라이트 기능
gm_highlight <- highlight_key(gapminder, ~country)

life_g <- gm_highlight |>
    ggplot(aes(x=year, y=lifeExp, group=country, text = paste0("대륙: ", continent, "\n", "국가: ", country))) + 
    geom_line()
    life_gg <- ggplotly(life_g, tooltip="text")
    highlight(life_gg, on="plotly_click", selectize=TRUE, dynamic=TRUE, persistemt=TRUE)

install.packages("crosstalk", lib = my_lib, repos = "https://cloud.r-project.org")
library(crosstalk)

gapminder_cjk <- gapminder |>
    filter(country %in% c("China", "Japan", "Korea, Rep"))

gapminder_sd <- SharedData$new(gapminder_cjk,~country)

life_g_static <- gapminder_sd |>
    ggplot(aes(x=year, y=lifeExp, group=country, text = paste0("대륙: ", continent, "\n", "국가: ", country))) +
    geom_line() +
    geom_point() + 
    facet_wrap(~country)

life_g_interactive <- ggplotly(life_g, tooltip="text")

#
