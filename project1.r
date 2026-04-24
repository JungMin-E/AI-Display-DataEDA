my_lib <- "/Users/ijeongmin/Desktop/R"
install.packages("tidyverse", lib = my_lib, repos = "https://cloud.r-project.org")

.libPaths(my_lib)
library(tidyverse)

file_path <- "/Users/ijeongmin/Desktop/project_1_하/defect.csv"
defect_data <- read.csv(file_path)

head(defect_data)

# 결측치
colSums(is.na(defect_data))

# 월별 defect_a 평균과 defect_c의 표준편차
defect_data |>
    group_by(Month) |>
        summarise(
            avg_defect_a = round(mean(defect_a, na.rm=T), 1),
            sd_defect_c = round(sd(defect_c, na.rm=T), 1)
        )

# 명점불량과 암점불량을 더해서 픽셀불량(pixel_defect)이라는 새로운 열을 만들고 이때 픽셀불량이 가장 많이 나온 날은 몇월, 몇일, 몇개가 나왔는지 표시
defect_data |>
    mutate(pixel_defect = defect_c + defect_d) |>
    arrange(desc(pixel_defect)) |>
    select(Month, Day, pixel_defect) |>
head(1)