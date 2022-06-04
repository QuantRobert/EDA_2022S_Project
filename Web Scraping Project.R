

# (Web Scraping Project) 2022. 4. 23.
# Topic: Korean Rating Agencies' Rating Action 

#..............................................................................
# 0. In Advance
# install.packages("httr")  ## for HTTP Request & Response
# install.packages("RSelenium") ## for HTTP Request & Response
# install.packages("urltools") ## for HTTP Request & Response
# install.packages("rvest") ## for HTML or XML Type Data Extraction 
# install.packages("jsonlite") ## for JSON Type Data Extraction
# install.packages("stringr") ## for Data Handling
# install.packages("dplyr") ## for Data Handling
# install.packages("tidyverse") ## for Data Handling

library(httr)
library(RSelenium)
library(urltools)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidyverse)

# Generating Rating Action Conversion Table 
Conversion <- data.frame(scaler = c(20, 19, 18, 18, 17, 16, 15, 15, 14, 13, 12, 12, 11, 10, 9, 9, 8, 7, 6, 6, 5, 4, 4, 3, 3, 2, 2, 1, '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
                   Rating = c('AAA', 'AA+', 'AA0', 'AA', 'AA-', 'A+', 'A0', 'A', 'A-', 'BBB+', 'BBB0', 'BBB', 'BBB-', 'BB+', 'BB0', 'BB', 'BB-', 'B+', 'B0', 'B', 'B-', 'CCC0', 'CCC', 'CC0', 'CC', 'C0', 'C', 'D', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
                   scaleo = c(0.90, 0.75, 0.75, 0.50, 0.50, 0.50, 0.25, 0.25, 0.10, '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
                   Outlook = c('상향검토', 'Positive', '긍정적', 'Stable', '안정적', '유동적', 'Negative', '부정적', '하향검토', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
                   Review = c('AA+↑', 'AA+↓', 'AA0↑', 'AA0↓', 'AA↑', 'AA↓', 'AA-↑', 'AA-↓', 'A+↑', 'A+↓', 'A0↑', 'A0↓', 'A↑', 'A↓', 'A-↑', 'A-↓', 'BBB+↑', 'BBB+↓', 'BBB0↑', 'BBB0↓', 'BBB↑', 'BBB↓', 'BBB-↑', 'BBB-↓', 'BB+↑', 'BB+↓', 'BB0↑', 'BB0↓', 'BB↑', 'BB↓', 'BB-↑', 'BB-↓', 'B+↑', 'B+↓', 'B0↑', 'B0↓', 'B↑', 'B↓', 'B-↑', 'B-↓', 'CCC0↑', 'CCC0↓', 'CCC↑', 'CCC↓', 'CC0↑', 'CC0↓', 'CC↑', 'CC↓', 'C0↑', 'C0↓', 'C↑', 'C↓'),
                   scalev = c(19.9, 19.1, 18.9, 18.1, 18.9, 18.1, 17.9, 17.1, 16.9, 16.1, 15.9, 15.1, 15.9, 15.1, 14.9, 14.1, 13.9, 13.1, 12.9, 12.1, 12.9, 12.1, 11.9, 11.1, 10.9, 10.1, 9.9, 9.1, 9.9, 9.1, 8.9, 8.1, 7.9, 7.1, 6.9, 6.1, 6.9, 6.1, 5.9, 5.1, 4.9, 4.1, 4.9, 4.1, 3.9, 3.1, 3.9,3.1, 2.9, 2.1, 2.9, 2.1))


#..............................................................................
# 1. NICE Rating
# 1.1. HTTP Request
res_nice <- GET(  ## res = abb(response)
  url = 'https://www.nicerating.com/disclosure/dayRatingNews.do'
)

# # Regular Format Instance - Recommended though the result would be same (this format can generate loop phrase easily)
# res <- GET(
#   url = 'https://section.blog.naver.com',
#   path = '/BlogHome.naver', ## Get rid of question mark('?')
#   query = list('directoryNo' = '0', ## separate letter and number
#                'currentPage' = '1', ## separate letter and number
#                'groupId' = '0')
# ) ## Original Address: 'https://section.blog.naver.com/BlogHome.naver?directoryNo=0&currentPage=1&groupId=0'

print(x = res_nice) ## Summary of HTML Response
## 'Response Status'; 2XX (Data Transfer Success), 3XX (Redirection), 4XX (Client Request Error), 5XX (Server Error)
status_code(x = res_nice)
content(x = res_nice) # to view full content, content(x = res_nice, as = 'text', encoding = 'UTF-8')
# View(res_nice)

#..............................................................................
# 1.2. Recall HTML Parsing Tree and Find Element (F12)

Sys.setlocale(category = 'LC_ALL', locale = 'C') # for Windows User

tbl_nice <- res_nice %>%
  read_html() %>%  # read_html(, encoding = 'UTF-8')
  html_node(css = '#dBody > section > div:nth-child(6)') %>% # Xpath(XML) vs. CSS(HTML) ... CSS is majority 
  html_table(fill = TRUE) # there would be a problem if the table has been merged

Sys.setlocale(category = 'LC_ALL', locale = 'Korean') # for Windows User

#..............................................................................
# 1.3. Data Handling

library(dplyr)
library(aqp)
class(tbl_nice)

colnames(x = tbl_nice) <- c('Entity Name', 'Round', 'Bond Type', 'Rating Type', 'Previous Rating', 'Previous Outlook', 'Present Rating', 'Present Outlook', 'Determination', 'Confirmation', 'Amount', 'Summary', 'Financials', 'Report')

tbl_nice2 <- select(tbl_nice, 'Entity Name', 'Bond Type', 'Previous Rating', 'Previous Outlook', 'Present Rating', 'Present Outlook', 'Determination', 'Confirmation')

tbl_nice2[,1:1] <- tbl_nice2$'Entity Name' %>%  # Insert the operation into the specific column [, n:n]
  str_remove_all(pattern = '\\(주\\)')  # because round bracket '()' is not a regular expression, I should put double-backslash (\\) before the round bracket, like apostrophe(') in Excel













# tbl_nice3 <- arrange(tbl_nice2, 'Bond Type' = 'SB')
# tbl_nice3 <- slice(tbl_nice2, 1:100)

Sys.setlocale(category = 'LC_ALL', locale = 'Korean') # for Windows User
tbl_nice2 <- as.numeric(tbl_nice2, trimws = TRUE) # no applicable method for 'slice' applied to an object of class "c('matrix', 'array', 'character')"
class(tbl_nice2)
tbl_nice3 <- slice(tbl_nice2, 1:100)


tbl_nice2 <- as.data.frame(tbl_nice2)



# rm(list=ls())

#..............................................................................
# 1.4. Gathering Time-Series Data
# (TBD) Time-Series Data Update in MECE 


#..............................................................................
# 2. KIS Rating


#..............................................................................
# 3. Korea Rating



#..............................................................................
# for Reference

# D <- readxl::read_xlsx("sample.xlsx")
# 
# D1 <- D %>% 
#   mutate(date=as.Date(date)) %>% 
#   mutate(score=case_when(
#     grade=="AA+" ~ 19,
#     grade=="AA" ~ 18,
#     grade=="A+" ~ 16,
#     TRUE ~ 0
#   )) %>% 
#   mutate(score=case_when(
#     view=="긍정적" ~ score+0.9,
#     view=="안정적" ~ score+0.8,
#     view=="부정적" ~ score+0.7,
#   ))
# 
# D2 <- D1 %>% 
#   arrange(firm, desc(date)) %>% 
#   group_by(firm) %>% 
#   slice(1:2)
# D2
# 
# D3 <- D2 %>% 
#   arrange(firm, score) %>% 
#   slice(1)
# D3


