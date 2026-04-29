my_lib <- "/Users/ijeongmin/Desktop/R/AD"
.libPaths(c(my_lib, .libPaths()))
install.packages("gt", lib = my_lib, repos = "https://cloud.r-project.org")

library(gt)
library(tidyverse)

# GT 패키지 
i_df |> tibble(
    name = names(islands),
    size = islands
)

final_table <- i_df |>
    arrange(-size) |> #desc = - 내림차순
    slice(1:10) |> #1:10까지 행을 선택
    gt() |>
# 제목 붙이는 방법
    tab_header(
        title = md("Large Landmasses of the world"), #md: markdown 테이블의 제목 진하게
        subtitle = md("The top ten largest are presented")
    ) |>
# 주석과 출처 추가하기 tab_source_note, tab_footnote
    tab_source_note(
        source_note = "Source: The world Almanac"
    ) |> 
    tab_footnote(
        footnote = "The largest by area.",
        locations = cells_body(
            columns = size,
            rows = size - max(size)
        )
    )

# 표 저장하기
final_table <- df |> 
    arrange(-size) |> #desc = - 내림차순
    slice(1:10) |> #1:10까지 행을 선택
    gt() |>
# 제목 붙이는 방법
    tab_header(
        title = md("Large Landmasses of the world"), #md: markdown 테이블의 제목 진하게
        subtitle = md("The top ten largest are p
