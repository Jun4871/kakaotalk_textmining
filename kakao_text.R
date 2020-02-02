library(tidyverse)
library(lubridate)

kko <- readLines("text_data.txt")

raw_df <- kko

# comma가 있는 것만 찾기
comma_index <- raw_df %>% 
  str_detect(",")

# comma가 있는 것만 재정렬
raw_df <- raw_df[comma_index]


## 날짜 데이터 

# 첫번째 comma 위치 찾는 함수
index_data <- regexpr(",", raw_df)

# comma 별 글자선택
kakao_date <- str_sub(raw_df,1,index_data - 1)


## user_name 테스트 선정

nchar_max <- max(nchar(raw_df))

# raw_df[which(nchar(raw_df) == 269)]
kakao_text <- str_sub(raw_df, index_data + 2, nchar_max)


# : 있는것
text_colon_index <- grepl(":",kakao_text)

kakao_colon_text <- kakao_text[text_colon_index]


# 첫번재 콜론 위치찾는 함수
first_colon_index <- regexpr(":", kakao_colon_text)

# user_name 찾기
kakao_user_name <- str_sub(kakao_colon_text, 1, first_colon_index-2)


# message 찾기
kakao_message <- str_sub(kakao_colon_text, first_colon_index+2, nchar_max)




# : 없는것 
text_not_colon_index <- !text_colon_index

kakao_not_colon_text <- kakao_text[text_not_colon_index]




# User_name_index 데이터프레임 만들기
kakao_colon_df <- data.frame("Username"= kakao_user_name, "Message" = kakao_message, "Index" = which(text_colon_index))
kakao_not_colon_df <- data.frame("Username"= "Unknown","Message" = kakao_not_colon_text, "Index" = which(text_not_colon_index))

# index 정렬
kakao_user_message <- rbind(kakao_colon_df,kakao_not_colon_df) %>% 
  arrange(Index) 

# 날짜 포함 정렬
kakao_df <- cbind("Date" = kakao_date, kakao_user_message)


# 날짜데이터를 위한 전처리
kakao_df$Date <- kakao_df$Date %>% 
  str_replace_all("\\.","-") %>% 
  str_remove_all(" ") %>% 
  str_replace_all("-오전"," ")

# 오후 Index만 따로 추출
pm_Index <- kakao_df$Date %>% 
  str_detect("-오후") %>% 
  which()

# 오후 데이터 전처리
kakao_df$Date <- kakao_df$Date %>% 
  str_replace_all("-오후"," ")

# 날짜 데이터 변형
kakao_df$Date <- kakao_df$Date %>% 
  ymd_hm()


# 오후 데이터 맞추기 (12시간을 더한다)
hour(kakao_df$Date[pm_Index]) <- hour(kakao_df$Date[pm_Index]) + 12

# NA제거
kakao_df <- kakao_df %>% na.omit()


# User name 바꿔주기
levels(kakao_df$Username)[1:3] <- paste0("User",c(1:length(levels(kakao_df$Username)[1:3])))
