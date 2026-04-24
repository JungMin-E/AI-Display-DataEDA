my_lib <- "/Users/ijeongmin/Desktop/R"
install.packages("tidyverse", lib = my_lib, repos = "https://cloud.r-project.org")

.libPaths(my_lib)
library(tidyverse)


df <- iris

# 조건에 맞는 행 추출
df |> filter(Species == 'setosa') |> head()

# 특정 열 선택
df |> select(Sepal.Length, Species) |> head()

# 특정 행 선택
df |> slice(1:5)

df

# 새로운 열 추가 df |> mutate(새열 = 계산식)
df |> mutate(ratio = Sepal.Length / Sepal.Width) |> head()

# 열 이름 변경 df |> rename(새이름  = 이전이름)
df |> rename(Sepal_Length = Sepal.Length) |> head()

# 정렬
#df |> arrange(열이름) 오름차순
#df |> arrange(desc(열이름)) 내림차순
df |> arrange(Sepal.Length) |> head()
df |> arrange(desc(Sepal.Length)) |> head()

df |>
    group_by(Species) |>
    summarize(
        평균 = mean(Sepal.Length),
        합계 = sum(Sepal.width),
        개수 = n()
    )

iris |> 
    pivot_longer(
        cols = c(Sepal.Length: Petal.Width),
        names_to = "measure",
        values_to = "value"
    )

head(iris)

library(tidyverse)
df <- read.csv("./data/display_data.csv")
head(df)

