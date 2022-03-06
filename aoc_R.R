# SIMP56 ASSIGNMENT - ANALYZING TWITTER DATA
# 2022-03-08

#load libraries
library(dplyr)
library(rtweet)
library(readxl)
library(writexl)
library(quanteda)
library(quanteda.textplots)
library(peRspective)

#load tokens
# usethis::edit_r_environ()
Sys.setenv(perspective_api_key = "AIzaSyBHWLyxTyojrFpilEILSYjb6FoKZR-XXXXX")

# Search API
aoc_df <- search_tweets(
  "to:aoc",
  n=5000,
  include_rts = FALSE,
  lang = "en",
  retryonratelimit=T,
  verbose = T)

# Excel file was created for replication purposes, can be acquired from research 
# team on demand: 'df_aoc1.xlsx'

# Dplyr filter relevant variables
df <- aoc_df %>% 
  select(status_id, text) %>% 
  distinct(status_id, .keep_all = T) %>% 
  distinct(text, .keep_all = T)

# Google PeRspective API, keyword 'TOXICITY'
toxicity_aoc <- df %>%  #TOXICITY
  prsp_stream(
    text = text,
    languages = "en",
    text_id = status_id,
    score_model = ("TOXICITY"),
    verbose = T
  )

# Rename to correct column
toxicity_aoc <- toxicity_aoc %>% 
  rename(status_id = text_id)

# Rejoin dfs with toxicity score
full_df <- inner_join(aoc_df, toxicity_aoc, by = "status_id")

# Save full df to excel
# write_xlsx(full_df,"C:/Users/vilhe/OneDrive/Desktop/R _feb/aoc_toxic_top_1000.xlsx")

# Arranging highest to lowest toxicity score, extract top 400 toxic tweets
df_400 <- full_df %>% 
  arrange(TOXICITY, sort = T) %>% 
  head(400)

# Random sample of 200 tweets
set.seed(10) # random sampling, in order not to get the same sample
sample_200 = df_400 %>% 
  distinct() %>% 
  sample_n(200)

# Quanteda analysis for wordcloud
corp = corpus(sample_200, text_field = 'text')
dtm = dfm(corp, stem = T, remove = stopwords('en'), remove_punct = T)
# 99,8 % contains the word 0 

# Trim it down, remove all terms that occur < 10 times
dtm = dfm_trim(dtm, min_termfreq = 10)

# Worcloud, 30 most frequent words
textplot_wordcloud(dtm, max_words = 30, max_size = 12)

# save final sample to excel file
write_xlsx(sample_200,"C:/Users/vilhe/OneDrive/Desktop/R _feb/aoc_final_sample_200.xlsx")
