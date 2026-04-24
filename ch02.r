# iris #R에 존재하는 기본 데이터

# install.packages("palmerpenguins") 데이터 설치 

# my_lib <- "/Users/ijeongmin/Desktop/R/AD"
# install.packages("gapminder", lib = my_lib, repos = "https://cloud.r-project.org")
# library(gapminder, lib.loc = my_lib)

# # CSV 파일 다루기
# # getwd() : 내 디렉토리가 어디있는지 확인

# # df <- read.csv("")
# # head(df)

# # Excel 파일 처리

# library(readxl) # readxl라이브러리 파일 설치

# data <- read_excel("파일명.xlsx")

# data <- read_excel("file.xlsx",
#                     sheet = "Sheet1",
#                     range = "A1:D100",
#                     col_names = TRUE,
#                     na = c("NA", ""),
#                     skip = 2)

# # 엑셀 파일 정보확인
# excel_sheets("파일명.xlsx")
# sheet_names <- excel_sheets("파일명.xlsx")

# library(writexl)

# write_xlsx(data, "파일명.xlsx")

# # 여러 데이터프레임을 하나의 엑셀 파일의 여러 시트로 저장
# list_of_dfs <- list(
#     "sheet1" = data1,
#     "sheet2" = data2,
#     "sheet3" = data3
# )

# write_xlsx(list_of_dfs, "multiple_sheets.xlsx")

# write_xlsx(data,
#             path = "output.xlsx",
#             col_names = TRUE,
#             format_headers = TRUE)

# library(openxlsx) # openxlsx 패키지를 통해서 불러오거나 쓸 수있음

# # 읽기
# data <- read.xlsx("파일명.xlsx",
#                     sheet = 1,
#                     startRow = 1,
#                     colNames = TRUE)

# # 쓰기
# write.xlsx(data, "파일명.xlsx",
#             sheetName = "Sheet1",
#             rowNames = FALSE,
#             colNames = TRUE)


getwd()

# 1. 패키지를 저장할 내 폴더 경로 설정
my_lib <- "/Users/ijeongmin/Desktop/R/AD"

# 2. R에게 "앞으로 패키지 찾을 때 바탕화면 폴더도 기본으로 인식해!" 라고 선언 (가장 중요 🌟)
.libPaths(c(my_lib, .libPaths())) 

# 3. 빈틈없이 설치하기: dependencies = TRUE 옵션을 넣으면
# tidyverse 작동에 필요한 ggplot2 같은 '딸린 식구들'까지 강제로 싹 다 설치해 줍니다.
install.packages("tidyverse", dependencies = TRUE, repos = "https://cloud.r-project.org")

# 4. 이제 lib.loc을 굳이 안 써도 R이 알아서 바탕화면 폴더까지 뒤져서 불러옵니다.
library(knitr)
library(tidyverse)
head(mtcars)

# mtcars 데이터 기본 통계
mtcars |> 
    summarise(
        avg_mpg = mean(mpg),
        avg_wt = mean(wt),
        avg_hp = mean(hp),
        n_cars = n()
    ) |>
    kable(digits = 2,
            col.names = c("평균 연비",
                          "평균 무게",
                          "평균 마력",
                          "차량 수"))

# mutate로 새로운 "자동", "수동" 새로운 열 만들기!
# 변속기 유형별 분석
mtcars |>
    mutate(am = factor(am,
            labels = c("자동", "수동"))) |>
    group_by(am) |>
    summarise(
        count = n(), #자동, 수동의 개수
        avg_mpg = mean(mpg),
        avg_hp = mean(hp)
    ) |>
    kable(digits = 1,
          col.names = c("변속기",
                        "수량",
                        "평균연비",
                        "평균마력"))


# 디스플레이 데이터를 불러와서 평균 밝기, 평균 색재현율, 평균 수명, 평균 
display_data <- read.csv('/Users/ijeongmin/Desktop/R/display_data.csv')

display_data |>
    summarise(
        avg_brightness = mean(Panel_Brightness),
        avg_gamut = mean(Color_Gamut),
        avg_lifetime = mean(Lifetime),
        avg_contrast = mean(Contrast_Ratio)
    ) |>
    kable(
        digits = 2,
        col.names = c("평균 밝기",
                      "평균 색재현율",
                      "평균 수명",
                      "평균 명암비"))

# 패널 밝기 최대치 대비 80% 이상의 데이터를 선정해서 밝기, 색재현율, 명암비, 등급을 표시하시오
display_data <- read.csv('/Users/ijeongmin/Desktop/R/display_data.csv')

display_data |>
    filter(Panel_Brightness >= max(Panel_Brightness, na.rm=T)*0.8) |>
    arrange(desc(Panel_Brightness)) |>
    select(Panel_Brightness, Color_Gamut,
        Contrast_Ratio, Panel_Grade) |>
    head(10) |>
    kable(digits = 2,
            col.names = c("밝기",
                            "색재현율",
                            "명암비",
                            "등급"

            ))

# 프로세스 데이터를 불러와서 생산라인에 따라 생산되는 Panel_Type별 데이터 분석
my_lib <- "/Users/ijeongmin/Desktop/R/AD"
install.packages("readxl", lib = my_lib, repos = "https://cloud.r-project.org")
library(readxl)

process_data <- read_excel('/data/process.xlsx')

process_data |>
    group_by(Panel_Type, Production_Line) |>
    summarise(
        avg_brightness = mean(Panel_Brightness, na.rm=T),
        avg_response = mean(Response_Time, na.rm=T),
        avg_power = mean(Power_Consumption, na,rm =T),
        yield = sum(QC_Status == 'Pass', na.rm=T) / n() = 100
    ) |>
    arrange(desc(yield)) |>

# 데이터 저장
library(writexl)

data <- process_data |>
    group_by(Production_Quarter, Panel_Type) |>
    summarise(
        생산량 = n(),
        수율 = round(sum(QC_Status == 'Pass', na.rm=T) / n() * 100, 1)
    ) |>
    kable()