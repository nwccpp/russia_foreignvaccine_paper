###########################################################
#                                                         #
#                                                         #
#      Supplementary Code for Article:                    #
#      Social Mediated News about Foreign COVID-19        #
#      Vaccination and Vaccine Hesitancy in Russia        #
#                                                         #
#      Authors: Erik Nisbet, Olga Kamenchuk &             #
#               Ayse Lokmanoglu                           #
#      Code Author: Ayse Lokmanoglu                       #  
###########################################################

#########################################################
######### Step 1 Retrieval of articles from NewsWhip API
#########################################################
#########################################################

# load libraries
library(httr)
library(jsonlite)
library(devtools)
library(urltools)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(wesanderson)
library(ggthemes)
library(scales)
#source_gist(4205477)

#######################################
# script to get popular articles from #
# newswhip database using API         #
# first we call the API to get the    #
# articles we are interested in       #
#######################################

##########################

#######################
# variables to change #
#######################

# below is the variable that needs to be changed if
# we want to change the number of articles that will be
# returned by the query
# I believe the limit is 5000

num_articles <- 5000

# date range YYYY-MM-DD
days<-as.character(as.Date(as.Date("2021-05-01"):as.Date("2021-12-31"), origin="1970-01-01"))

###########################################
# calling the newship API to get articles #
###########################################

# API key: 
# setup API key and set the URL of the endpoint

# create a variable that has the API key in it
#api_key <- #####

# create a variable that tells the API where to go
# this is from the NewsWhip help file
api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)

# make a function to get articles matching a given query
# the function below creates a query based on the parameters:
# api_key: our API key
# limit: the number of results we want to get back
# start_time: the starting time for when articles are published
# end_time: the ending time for when articles are published

# included in the query are two options:
# sort_by fb_tw_and_li: this orders our results to be those that have the highest engagement on Facebook, Twitter, and Linked In
# find_related false: this says that we do not want it to find other related stories to those that are included in the query and to include them

get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language: ru AND country_code: ru AND ((пфайзер) OR (модерна) OR (мРНК) OR (moderna) OR (pfizer) OR (mrna) OR (астразенека) OR (astrazeneca) OR (oxford vaccine) OR (оксфорд вакцина))\"], 
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}

######the loop for date range

mylist <- list()
for (i in days) {
  print("now running days:")
  print (i)
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i))+1,  "00:00:00 EST", sep=" "))) * 1000 - 1
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time)
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01"))
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  data_temp<- data_temp %>% select(delta_time, 
                                   recent_fb_counts, 
                                   recent_tw_counts, 
                                   predicted_interactions, 
                                   predicted_timestamp, 
                                   uuid, 
                                   publication_timestamp, 
                                   link, 
                                   headline, 
                                   excerpt, 
                                   keywords, 
                                   image_link, 
                                   has_video, 
                                   nw_score, 
                                   max_nw_score, 
                                   fb_data.total_engagement_count, 
                                   fb_data.likes, 
                                   fb_data.comments, 
                                   fb_data.shares, 
                                   fb_data.total_count_delta, 
                                   fb_data.delta_period, 
                                   fb_data.delta_period_unit, 
                                   tw_data.tw_count, 
                                   tw_data.total_count_delta, 
                                   tw_data.delta_period, 
                                   tw_data.delta_period_unit, 
                                   li_data.li_count, 
                                   li_data.total_count_delta, 
                                   li_data.delta_period, 
                                   li_data.delta_period_unit, 
                                   pi_data.pi_count, 
                                   pi_data.delta_period, 
                                   pi_data.delta_period_unit, 
                                   source.publisher, 
                                   source.domain, 
                                   source.link, 
                                   source.country, 
                                   source.country_code, 
                                   source.language, 
                                   date_time, 
                                   date)
  mylist[[i]] <- data_temp
}

##add month and year name, or if you are pulling for a full year just put year
data_Russia_Vaccine_21 <- do.call("rbind",mylist)%>%data.frame()
###save as excel
starting_date <- '2021-05-01'
total_num_articles<-nrow(data_Russia_Vaccine_21)
openxlsx::write.xlsx(data_Russia_Vaccine_21, file = paste("CSVandExcel/COVID_NewsWhip_RU_Vaccine_starting_", gsub("-", "_", starting_date), "_N_", total_num_articles, ".xlsx", sep=""), rowNames=F)
##save as RDA
save(data_Russia_Vaccine_21, file = paste("RDA/COVID_NewsWhip_RU_Vaccine_starting_", gsub("-", "_", starting_date), "_N_", total_num_articles, ".Rda", sep=""))

#########################################################
######### Step 2 Descriptives of Retrieved Content
#########################################################
#########################################################
##just domains
test<- data_Russia_Vaccine_21 %>% count(source.domain)
write.csv(test, "CSVandEXCEL/Russia_Vaccine_Domain_List.csv")

##domains and engagement
temp<- data_Russia_Vaccine_21 %>%
  select(
    source.domain,
    fb_data.total_engagement_count, 
    tw_data.tw_count,
    li_data.li_count,
    date) %>%
  mutate(forcount = 1) %>%
  group_by(source.domain) %>%
  summarise(FB_eng = sum(fb_data.total_engagement_count),
            TW_eng = sum(tw_data.tw_count),
            LI_eng = sum(li_data.li_count),
            articlecount = sum(forcount))
write.csv(temp, "CSVandEXCEL/Russia_Vaccine_Domain_Engagement_List.csv")


#######clean domain list
Russia_Vaccine_Domain_Engagement_List_edited_en <- readxl::read_excel("CSVandExcel/Russia_Vaccine_Domain_Engagement_List_edited_en.xlsx")

variables<- Russia_Vaccine_Domain_Engagement_List_edited_en %>%
  filter(delete==1) %>%
  select(source.domain)

##make it into a list
variables<-variables$source.domain

data_filtered <- data_Russia_Vaccine_21 %>%
  dplyr:: filter(!source.domain %in% variables)

openxlsx::write.xlsx(data_filtered, file = "CSVandExcel/Russia_Vaccine_Scrape.xlsx", rowNames=F)
##save as RDA
save(data_filtered, file = "RDA/Russia_Vaccine_Scrape.Rda")

##############################################
####Visualization 1###########################
##############################################

##########Graph engagement and daily article volume
data_filtered_long<-data_filtered %>%
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, tw_data.tw_count),
         forcount = 1) %>%
  dplyr::select(date, forcount, engagement) %>%
  pivot_longer(!date,
               names_to = "Variable",
               values_to = "Volume",
               values_drop_na = TRUE) %>%
  group_by(date, Variable) %>%
  summarize(sum = sum(Volume)) %>%
  filter(!date=='2022-01-01') %>%
  ungroup()

labels_volume <- c(
  forcount = "Article Volume",
  engagement = "Engagement Volume")

ggplot(data_filtered_long, aes(x=date, y=sum)) + 
  geom_col(aes(color = Variable)) +
  scale_color_manual(values = wesanderson::wes_palette('Moonrise2')) +
  scale_x_date(date_breaks="1 month", date_labels = "%b-%Y") +
  scale_y_continuous(label=comma, breaks=pretty_breaks())+
  facet_wrap(~Variable, 
             labeller = labeller(
               Variable = labels_volume),
             nrow=2,
             scales = "free_y") +
  labs(x=" ", 
       y=" ")+
  theme_wsj(color="white") +
  theme(axis.text.x=element_text(size=10, angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=10, family="sans"),
        legend.position="none", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())
ggsave("Visuals/Figure1_Article_Engagement_Volume_061322.tiff", width=8.5, height=5, dpi=300)

#########################################################
######### Step 3 Python Scraping
#########################################################
#########################################################
###submit to python for scraping
### refer to Python Script for scraping 
#########################################################
######### Step 4 Text Analysis
#########################################################
#########################################################

###download python scrapes
dfmaster2 <- readr::read_csv("CSVandExcel/my_scraped_articles_master_FV.csv")
dfmaster <- readr::read_csv("CSVandExcel/my_scraped_articles_russian_master_v1.csv")
dfnas <- readr::read_csv("CSVandExcel/my_scraped_articles.csv")
df_master<-dfmaster %>%
  filter(!text=='N/A' | !text=='NA') %>%
  select(-...1)

########clean and save cleaned
##rename
temp<-df_master %>%
  mutate(index = row_number())
temp$originaltext<-temp$text

###rerun the cleaning
temp$text<-tolower(temp$text)
temp$text<-gsub("(f|ht)tp\\S+\\s*"," ",temp$text)
temp$text<-gsub("(://)\\S+\\s*"," ",temp$text)
temp$text<-gsub("(/html/)\\S+\\s*"," ",temp$text)
temp$text<-gsub("[\r\n]"," ",temp$text)
temp$text<-removeNumbers(temp$text)
temp$text<-removePunctuation(temp$text)

save(temp, file = "RDA/Russia_Text_Clean_051622.Rda")
# beepr::beep()

###############Check top words###################
# ###run topwords to see if more cleaning is needed
# stopwords<-c(stopwords("ru"),
#              LETTERS,
#              letters,
#              "минута", 
#              "к")
# stopwords<-data.frame(stopwords)
# names(stopwords)[1]<-"word"
# 
# temp_top_word<- temp %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stopwords) %>%
#   count(word, sort=TRUE)
# 
# openxlsx::write.xlsx(temp_top_word, file="CSVandExcel/Top_Word.xlsx")

####Start LDA steps
##1. tokenize the folder
toks <- tokens(temp$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(c(stopwords("ru"),
                  LETTERS,
                  letters, 
                  "минута", 
                  "к",
                  "internetgrouprianru",
                  "рфпи",
                  "рф",
                  "миа",
                  "это", 
                  "могут",
                  "январь", 
                  "февраль", 
                  "март", 
                  "апрель", 
                  "май", 
                  "июнь", 
                  "июль", 
                  "август", 
                  "сентябрь", 
                  "октябрь", 
                  "ноябрь", 
                  "декабрь", 
                  "год" , 
                  "день", 
                  "час", 
                  "часы", 
                  "минуты", 
                  "минуты", 
                  "секунды", 
                  "секунды",
                  "риа", 
                  "рф",
                  "дней",
                  "месяцев",
                  "месяц",
                  "одной",
                  "двух",
                  "Александр",
                  "aлександр",
                  "миа",
                  "internetgrouprianru",
                  "поскольку",
                  "владимир",
                  "сергей",
                  "na",
                  "рфпи",
                  "г",
                  "true",
                  "неделю",
                  "неделе",
                  "михаил",
                  "марта",
                  "м",
                  "алексей",
                  "андрей",
                  "оно",
                  "дмитрий",
                  "sp",
                  "х",
                  "понедельник вторник среда Четверг Пятница Суббота воскресенье",
                  "иван",
                  "александрович",
                  "одобрение",
                  "ольга",
                  "гг",
                  "й",
                  "рбк",
                  "го",
                  "николай",
                  "е",
                  "отметить",
                  "виктор",
                  "роберт",
                  "фрг",
                  "майкл",
                  "р",
                  "денис",
                  "юлия",
                  "евгений",
                  "н",
                  "сергеевич",
                  "iii",
                  "энтони",
                  "екатерина",
                  "xix",
                  "jones",
                  "викторович",
                  "дмитриев",
                  "jj",
                  "люк",
                  "ек",
                  "лазебный",
                  "юрий",
                  "василий",
                  "xx",
                  "uraru",
                  "еберезовская",
                  "наскерзаде",
                  "олег",
                  "наталья",
                  "ф",
                  "степана",
                  "дмитрия",
                  "ул",
                  "де",
                  "ртс",
                  "co")) %>%
  tokens_select(min_nchar = 2)

####2 make it into a DFM
dfm_counts<- dfm(toks) 
rm(toks)
####to match the lines after LDA##########################
docnames(dfm_counts)<-temp$index
sparsity(dfm_counts)

###trim########
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
##save it for future
save(dfm_counts2, file="RDA/dfm_counts_ru_nw_051622.Rda")

rm(list = ls())
#######################################
##########find k#########################
###########################################
###add new packages needed
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(quanteda) #for text analysis
library(topicmodels) #running topic modeling
library(ldatuning) #choosing K
library(tidytext)
library(stopwords) #for cleaning up stopwords
library(tm) #for cleaning the corpus

# convert to LDA-ready object
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)

myalpha=0.1

## Running the searchK command
Sys.time()
result1 <- FindTopicsNumber(
  dtm_lda,
  topics = seq(10,200,by=10), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 8),
  verbose = TRUE
)
Sys.time()
save(result1, file="NewswhipRU/NW_RU_FindK_Result1_051022.Rda")
FindTopicsNumber_plot(result1)
ggsave("Visuals/FigureS1_1_FindKPlot_061322.tiff", width=8.5, height=5, dpi=300)


###try a larger number of k
sys.time()
result2 <- FindTopicsNumber(
  dtm_lda,
  topics = seq(150,350,by=10), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(alpha=myalpha, seed = 8),
  verbose = TRUE
)
Sys.time()
save(result2, file="NewswhipRU/NW_RU_FindK_Result2_051022.Rda")
FindTopicsNumber_plot(result2)
rm(list = ls())





##load combined scraped data will be labeled dfm)counts2
# load("NewswhipRU/dfm_counts_ru_nw_051622.Rda")
# load("NewswhipRU/Russia_Text_Clean_051622.Rda")

###convert dfm to lda
dtm_lda <- convert(dfm_counts2, to = "topicmodels")

###k 60 and k 80 alpha 0.1
####k60#####
####LDA
Sys.time()
ldak60 <- LDA(dtm_lda,
              60,
              method = "Gibbs",
              control = list(alpha=0.1,seed=125231)) 
save(ldak60, file="NewswhipRU/NW_RU_k60_lda_051622.Rda")
Sys.time()

##top words
LDAfit<-ldak60
# textcol<-which( colnames(temp)=="text" )
datacolnum=44
topterms<-data.frame(terms(LDAfit, 25))
extract_topic_xls<-function (eachLDA) {
  LDAfit<-eachLDA}
mybeta<-data.frame(LDAfit@beta)
colnames(mybeta)<-LDAfit@terms
mybeta<-t(mybeta)
colnames(mybeta)<-seq(1:ncol(mybeta))
mybeta=exp(mybeta)
nwords=50
topwords <- mybeta[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- mybeta[order(-mybeta[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec} 
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
# openxlsx::write.xlsx(topwords, "NewswhipRU/Russia_Vaccine_k60_topwords.xlsx")

#### apply FREX formula below
myw=0.3
word_beta_sums<-rowSums(mybeta)
my_beta_for_frex<-mybeta
for (m in 1:ncol(my_beta_for_frex)) {
  for (n in 1:nrow(my_beta_for_frex)) {
    my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
  }
  print (m)
}
nwords=100
topwords <- my_beta_for_frex[1:nwords,]
for (i in 1:LDAfit@k) {
  tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
  tempframe <- tempframe[1:nwords,]
  tempvec<-as.vector(rownames(tempframe))
  topwords[,i]<-tempvec
}
rownames(topwords)<-c(1:nwords)
topwords<-data.frame(topwords)
openxlsx::write.xlsx(topwords, file="NewswhipRU/Russia_Vaccine_k60_topwords_051622.xlsx")

##toptexs
######first find the posts that were dropped#####
data1<-temp
data1$index<-as.character(data1$index)
deleted_lda_texts<-(setdiff(data1$index, as.integer(LDAfit@documents)))
'%!in%' <- function(x,y)!('%in%'(x,y))
data1<-data1[data1$index %!in% deleted_lda_texts,]

####now run top texts with the removed posts####
metadf<-data1
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=100
toptexts <- mybeta[1:ntext,]
for (i in 1:LDAfit@k) {
  print(i)
  tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
  tempframe <- tempframe[1:ntext,]
  tempvec<-as.vector(tempframe[,1])
  toptexts[,i]<-tempvec
}
rownames(toptexts)<-c(1:ntext)
toptexts<-data.frame(toptexts)
openxlsx::write.xlsx(toptexts, file="NewswhipRU/Russia_Vaccine_k60_toptexts_051622.xlsx")

meta_theta_df2<-cbind(metadf,LDAfit@gamma)
first<-which(colnames(meta_theta_df2)=="1")
last<-ncol(meta_theta_df2)
colnames(meta_theta_df2)[first:last] <- paste("X", colnames(meta_theta_df2[,c(first:last)]), sep = "_")
save(meta_theta_df2, file="NewswhipRU/Russia_Vaccine_Master_MetaThetaDF_k60_051622.Rda")
save(meta_theta_df2, file="RDA/Russia_Vaccine_Master_MetaThetaDF_k60_051622.Rda")

rm(list = ls())

# ###########################reload all
# library(dplyr)
# library(tidyverse)
# library(lubridate)
# library(ggplot2)
# library(quanteda) #for text analysis
# library(topicmodels) #running topic modeling
# library(ldatuning) #choosing K
# library(tidytext)
# library(stopwords) #for cleaning up stopwords
# library(tm) #for cleaning the corpus
# 
# ##load combined scraped data will be labeled dfm)counts2
# load("NewswhipRU/dfm_counts_ru_nw_051622.Rda")
# load("NewswhipRU/Russia_Text_Clean_051622.Rda")
# 
# ###convert dfm to lda
# dtm_lda <- convert(dfm_counts2, to = "topicmodels")
# 
# ####k80#####
# ####LDA
# Sys.time()
# ldak80 <- LDA(dtm_lda,
#            80,
#            method = "Gibbs",
#            control = list(alpha=0.1,seed=125231)) 
# save(ldak80, file="NewswhipRU/NW_RU_k80_lda_051622.Rda")
# Sys.time()
# 
# ##top words
# LDAfit<-ldak80
# datacolnum=44
# topterms<-data.frame(terms(LDAfit, 25))
# extract_topic_xls<-function (eachLDA) {
#   LDAfit<-eachLDA}
# mybeta<-data.frame(LDAfit@beta)
# colnames(mybeta)<-LDAfit@terms
# mybeta<-t(mybeta)
# colnames(mybeta)<-seq(1:ncol(mybeta))
# mybeta=exp(mybeta)
# nwords=50
# topwords <- mybeta[1:nwords,]
# for (i in 1:LDAfit@k) {
#   tempframe <- mybeta[order(-mybeta[,i]),]
#   tempframe <- tempframe[1:nwords,]
#   tempvec<-as.vector(rownames(tempframe))
#   topwords[,i]<-tempvec} 
# 
# rownames(topwords)<-c(1:nwords)
# topwords<-data.frame(topwords)
# 
# #### apply FREX formula below
# myw=0.3
# word_beta_sums<-rowSums(mybeta)
# my_beta_for_frex<-mybeta
# for (m in 1:ncol(my_beta_for_frex)) {
#   for (n in 1:nrow(my_beta_for_frex)) {
#     my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
#   }
#   print (m)
# }
# nwords=100
# topwords <- my_beta_for_frex[1:nwords,]
# for (i in 1:LDAfit@k) {
#   tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
#   tempframe <- tempframe[1:nwords,]
#   tempvec<-as.vector(rownames(tempframe))
#   topwords[,i]<-tempvec
# }
# rownames(topwords)<-c(1:nwords)
# topwords<-data.frame(topwords)
# openxlsx::write.xlsx(topwords, file="NewswhipRU/Russia_Vaccine_k80_topwords_051622.xlsx")
# 
# ##toptexs
# ######first find the posts that were dropped#####
# data1<-temp
# data1$index<-as.character(data1$index)
# deleted_lda_texts<-(setdiff(data1$index, as.integer(LDAfit@documents)))
# '%!in%' <- function(x,y)!('%in%'(x,y))
# data1<-data1[data1$index %!in% deleted_lda_texts,]
# 
# ####now run top texts with the removed posts####
# metadf<-data1
# meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
# ntext=250
# toptexts <- mybeta[1:ntext,]
# for (i in 1:LDAfit@k) {
#   print(i)
#   tempframe <- meta_theta_df[order(-as.numeric(meta_theta_df[,i+1])),]
#   tempframe <- tempframe[1:ntext,]
#   tempvec<-as.vector(tempframe[,1])
#   toptexts[,i]<-tempvec
# }
# rownames(toptexts)<-c(1:ntext)
# toptexts<-data.frame(toptexts)
# openxlsx::write.xlsx(toptexts, file="NewswhipRU/Russia_Vaccine_k80_toptexts_051622.xlsx")
# 
# meta_theta_df2<-cbind(metadf,LDAfit@gamma)
# first<-which(colnames(meta_theta_df2)=="1")
# last<-ncol(meta_theta_df2)
# colnames(meta_theta_df2)[first:last] <- paste("X", colnames(meta_theta_df2[,c(first:last)]), sep = "_")
# 
# save(meta_theta_df2, file="NewswhipRU/Russia_Vaccine_Master_MetaThetaDF_k80_051622.Rda")

#########################################################
######### Step 5 ANTMN (Walter and Ophir, 2019)
#########################################################
#########################################################

########################################################
########################################################
###############ANTMN for russia Vaccines
# Code citation:
# ANTMN V2
# Author: Dror Walter
# Creating Topic Model Networks (ANTMN Method): Supplementary code
# To Cite:
# Walter D. & Ophir Y. (2019) News Frame Analysis: an Inductive Mixed Method Computational Approach. Communication Methods and Measures. http://dx.doi.org/10.1080/19312458.2019.1639145
# Online published (July 2019)
################################
########################################################
########################################################

##### load k60 meta_theta_df and lda
load("~/NewsWhip/Newswhip/Russia/domain_nw/RDA/Russia_Vaccine_Master_MetaThetaDF_k60_051622.Rda")
load("~/NewsWhip/Newswhip/Russia/domain_nw/RDA/NW_RU_k60_lda_051622.Rda")

#############################################################
##############ANTMN##############################
##########################################################
# Assign Topic Names read from excel and get first column as list
topic_names<-readxl::read_excel("CSVandExcel/russia_vaccines_k60_topic_labels.xlsx")
topic_names<-topic_names$Topic_Names
deleted_topics=(grep("DELETE:",topic_names))
# Calculate topic size (for topics-as-nodes)
topicsize<-colMeans(meta_theta_df2[,48:107])

# ANTMN function
network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),bbone=FALSE) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # claculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # Backbone
  if (bbone==TRUE) {
    print("Backboning")
    
    nnodesBASE<-length(V(newg))
    for (bbonelvl in rev(seq(0,1,by=0.05))) {
      #print (bbonelvl)
      nnodes<-length(V(backbone_filter(newg,alpha=bbonelvl)))
      if(nnodes>=nnodesBASE) {
        bbonelvl=bbonelvl
        #  print ("great")
      }
      else{break}
      oldbbone<-bbonelvl
    }
    
    newg<-backbone_filter(newg,alpha=oldbbone)
    
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write.graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}


mynewnet<-network_from_LDA(LDAobject=ldak60,
                           topic_names=topic_names,
                           topic_size=topicsize,
                           deleted_topics=(grep("DELETE:",topic_names)),
                           save_filename="russia_foreign_vac_nojunk_k60",
                           bbone=TRUE)
save(mynewnet, file="RDA/Russia_FV_Antmn.Rda")
meta_theta_df_comm<-meta_theta_df2
rm(meta_theta_df2)
rm(ldak60)

# Calculating cumulative Theme loadings from Spinglass
# Blue Com 1
# Green Com 2
# Purple Com 3
# Orange Com 4
which(colnames(meta_theta_df_comm)=="X_1") #### -1 for rowsums

meta_theta_df_comm$BlueCom1<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$spinglass == 1)]))+47]) 
meta_theta_df_comm$OrangeCom2<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$spinglass == 2)]))+47]) 
meta_theta_df_comm$GreenCom3<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$spinglass == 3)]))+47]) 
meta_theta_df_comm$PurpleCom4<-rowSums(meta_theta_df_comm[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$spinglass == 4)]))+47]) 

save(meta_theta_df_comm, file="RDA/NW_Covid_K60_Meta_DF_Comm_SpinGlass_060822.Rda")

#################ANTMN complete#####################

#########################################################
######### Step 6 Sentiment Analysis
#########################################################
#########################################################

######################################################
#######run sentiment on each topic for k60###############
############Sentiment Analysis##########################
#######################################################
###create NRC for Russian
##load NRC_All from website
library(htmlTable)
library(tidyr)

####first get the sentiment dictionary
nrcmain<-get_sentiments("nrc")
names(NRC_All)[1]<-"word"
nrc<-inner_join(NRC_All, nrcmain)
nrcRU<-dplyr::select(nrc, 'Russian (ru)', sentiment)
names(nrcRU)[1]<-"word"
save(nrcRU,file="RDA/NRC_Russian.Rda")

###run sentiment analysis with communities
###load the packages needed
load("NewswhipRU/NW_Covid_K60_Meta_DF_Comm_SpinGlass_060822.Rda")
load("NewswhipRU/NRC_Russian.Rda")

#change name
data_master<-meta_theta_df_comm
rm(meta_theta_df_comm)

##create a stopword dataframe
stopwords<-c(stopwords("ru"),
             LETTERS,
             letters, 
             "минута", 
             "к",
             "internetgrouprianru",
             "рфпи",
             "рф",
             "миа",
             "это", 
             "могут",
             "январь", 
             "февраль", 
             "март", 
             "апрель", 
             "май", 
             "июнь", 
             "июль", 
             "август", 
             "сентябрь", 
             "октябрь", 
             "ноябрь", 
             "декабрь", 
             "год" , 
             "день", 
             "час", 
             "часы", 
             "минуты", 
             "минуты", 
             "секунды", 
             "секунды",
             "риа", 
             "рф",
             "дней",
             "месяцев",
             "месяц",
             "одной",
             "двух",
             "Александр",
             "aлександр",
             "миа",
             "internetgrouprianru",
             "поскольку",
             "владимир",
             "сергей",
             "na",
             "рфпи",
             "г",
             "true",
             "неделю",
             "неделе",
             "михаил",
             "марта",
             "м",
             "алексей",
             "андрей",
             "оно",
             "дмитрий",
             "sp",
             "х",
             "понедельник вторник среда Четверг Пятница Суббота воскресенье",
             "иван",
             "александрович",
             "одобрение",
             "ольга",
             "гг",
             "й",
             "рбк",
             "го",
             "николай",
             "е",
             "отметить",
             "виктор",
             "роберт",
             "фрг",
             "майкл",
             "р",
             "денис",
             "юлия",
             "евгений",
             "н",
             "сергеевич",
             "iii",
             "энтони",
             "екатерина",
             "xix",
             "jones",
             "викторович",
             "дмитриев",
             "jj",
             "люк",
             "ек",
             "лазебный",
             "юрий",
             "василий",
             "xx",
             "uraru",
             "еберезовская",
             "наскерзаде",
             "олег",
             "наталья",
             "ф",
             "степана",
             "дмитрия",
             "ул",
             "де",
             "ртс",
             "co")
stopwords<-data.frame(stopwords)
names(stopwords)[1]<-"word"

#####antijoin by stopwords join by sentiment and counts
temp_test<-data_master %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords) %>%
  inner_join(nrcRU) %>%
  group_by(index) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0) 
temp_test3<-inner_join(data_master, temp_test, by="index")

##create a net sentiment column
temp_test3<-temp_test3 %>% mutate(sentiment=positive-negative)

rm(data_master)
rm(temp_test)
rm(stopwords)
rm(nrcRU)
gc()

####start joining with topics
###anger
anger_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(anger_k60) <- paste("anger", colnames(anger_k60), sep = "_")

for (i in 1:ncol(anger_k60)) {
  anger_k60[ , i] <- anger_k60[ , i] * temp_test3$anger
}
temp_test3<-cbind(temp_test3, anger_k60)
tempanger<-cbind(temp_test3[,"date"],anger_k60)
colnames(tempanger)[1]<-"date"
angerbyday<-aggregate(x=tempanger[,2:ncol(tempanger)],by=list(tempanger$date), FUN="mean", na.rm=TRUE)
rm(tempanger)

#####anticipation
anticipation_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(anticipation_k60) <- paste("anticipation", colnames(anticipation_k60), sep = "_")

for (i in 1:ncol(anticipation_k60)) {
  anticipation_k60[ , i] <- anticipation_k60[ , i] * temp_test3$anticipation
}

temp_test3<-cbind(temp_test3, anticipation_k60)
tempanticipation<-cbind(temp_test3[,"date"],anticipation_k60)
colnames(tempanticipation)[1]<-"date"
anticipationbyday<-aggregate(x=tempanticipation[,2:ncol(tempanticipation)],by=list(tempanticipation$date), FUN="mean", na.rm=TRUE)
rm(tempanticipation)

###disgust
disgust_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(disgust_k60) <- paste("disgust", colnames(disgust_k60), sep = "_")

for (i in 1:ncol(disgust_k60)) {
  disgust_k60[ , i] <- disgust_k60[ , i] * temp_test3$disgust
}
temp_test3<-cbind(temp_test3, disgust_k60)
tempdisgust<-cbind(temp_test3[,"date"],disgust_k60)
colnames(tempdisgust)[1]<-"date"
disgustbyday<-aggregate(x=tempdisgust[,2:ncol(tempdisgust)],by=list(tempdisgust$date), FUN="mean", na.rm=TRUE)
rm(tempdisgust)

###fear
fear_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(fear_k60) <- paste("fear", colnames(fear_k60), sep = "_")

for (i in 1:ncol(fear_k60)) {
  fear_k60[ , i] <- fear_k60[ , i] * temp_test3$fear
}
temp_test3<-cbind(temp_test3, fear_k60)
tempfear<-cbind(temp_test3[,"date"],fear_k60)
colnames(tempfear)[1]<-"date"
fearbyday<-aggregate(x=tempfear[,2:ncol(tempfear)],by=list(tempfear$date), FUN="mean", na.rm=TRUE)
rm(tempfear)

####joy
joy_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(joy_k60) <- paste("joy", colnames(joy_k60), sep = "_")

for (i in 1:ncol(joy_k60)) {
  joy_k60[ , i] <- joy_k60[ , i] * temp_test3$joy
}
temp_test3<-cbind(temp_test3, joy_k60)
tempjoy<-cbind(temp_test3[,"date"],joy_k60)
colnames(tempjoy)[1]<-"date"
joybyday<-aggregate(x=tempjoy[,2:ncol(tempjoy)],by=list(tempjoy$date), FUN="mean", na.rm=TRUE)
rm(tempjoy)

#####negative
negative_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(negative_k60) <- paste("neg", colnames(negative_k60), sep = "_")

for (i in 1:ncol(negative_k60)) {
  negative_k60[ , i] <- negative_k60[ , i] * temp_test3$negative
}

temp_test3<-cbind(temp_test3, negative_k60)
tempneg<-cbind(temp_test3[,"date"],negative_k60)
colnames(tempneg)[1]<-"date"
negbyday<-aggregate(x=tempneg[,2:ncol(tempneg)],by=list(tempneg$date), FUN="mean", na.rm=TRUE)
rm(tempneg)

###positive
positive_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(positive_k60) <- paste("pos", colnames(positive_k60), sep = "_")

for (i in 1:ncol(positive_k60)) {
  positive_k60[ , i] <- positive_k60[ , i] * temp_test3$positive
}
temp_test3<-cbind(temp_test3, positive_k60)
temppositive<-cbind(temp_test3[,"date"],positive_k60)
colnames(temppositive)[1]<-"date"
positivebyday<-aggregate(x=temppositive[,2:ncol(temppositive)],by=list(temppositive$date), FUN="mean", na.rm=TRUE)
rm(temppositive)

#####sadness
sadness_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(sadness_k60) <- paste("sadness", colnames(sadness_k60), sep = "_")

for (i in 1:ncol(sadness_k60)) {
  sadness_k60[ , i] <- sadness_k60[ , i] * temp_test3$sadness
}

temp_test3<-cbind(temp_test3, sadness_k60)
tempsadness<-cbind(temp_test3[,"date"],sadness_k60)
colnames(tempsadness)[1]<-"date"
sadnessbyday<-aggregate(x=tempsadness[,2:ncol(tempsadness)],by=list(tempsadness$date), FUN="mean", na.rm=TRUE)
rm(tempsadness)

#####surprise
surprise_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(surprise_k60) <- paste("surprise", colnames(surprise_k60), sep = "_")

for (i in 1:ncol(surprise_k60)) {
  surprise_k60[ , i] <- surprise_k60[ , i] * temp_test3$surprise
}

temp_test3<-cbind(temp_test3, surprise_k60)
tempsurprise<-cbind(temp_test3[,"date"],surprise_k60)
colnames(tempsurprise)[1]<-"date"
surprisebyday<-aggregate(x=tempsurprise[,2:ncol(tempsurprise)],by=list(tempsurprise$date), FUN="mean", na.rm=TRUE)
rm(tempsurprise)

###trust
trust_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(trust_k60) <- paste("trust", colnames(trust_k60), sep = "_")

for (i in 1:ncol(trust_k60)) {
  trust_k60[ , i] <- trust_k60[ , i] * temp_test3$trust
}
temp_test3<-cbind(temp_test3, trust_k60)
temptrust<-cbind(temp_test3[,"date"],trust_k60)
colnames(temptrust)[1]<-"date"
trustbyday<-aggregate(x=temptrust[,2:ncol(temptrust)],by=list(temptrust$date), FUN="mean", na.rm=TRUE)
rm(temptrust)

#####sentiment
sent_k60<-select(temp_test3, X_1:PurpleCom4)
colnames(sent_k60) <- paste("sentiment", colnames(sent_k60), sep = "_")

for (i in 1:ncol(sent_k60)) {
  sent_k60[ , i] <- sent_k60[ , i] * temp_test3$sentiment
}

temp_test3<-cbind(temp_test3, sent_k60)
tempsent<-cbind(temp_test3[,"date"],sent_k60)
colnames(tempsent)[1]<-"date"
sentbyday<-aggregate(x=tempsent[,2:ncol(tempsent)],by=list(tempsent$date), FUN="mean", na.rm=TRUE)
rm(tempsent)

###save the master file 
save(temp_test3,file="NewswhipRU/Russia_k60_each_topic_gamma_sentiment_060922.Rda")

###join all the day files and save a master file
temp1<-inner_join(angerbyday, anticipationbyday, by="Group.1")
temp1<-inner_join(temp1, disgustbyday, by="Group.1")
temp1<-inner_join(temp1, fearbyday, by="Group.1")
temp1<-inner_join(temp1, joybyday, by="Group.1")
temp1<-inner_join(temp1, negbyday, by="Group.1")
temp1<-inner_join(temp1, positivebyday, by="Group.1")
temp1<-inner_join(temp1, sadnessbyday, by="Group.1")
temp1<-inner_join(temp1, surprisebyday, by="Group.1")
temp1<-inner_join(temp1, trustbyday, by="Group.1")
temp1<-inner_join(temp1, sentbyday, by="Group.1")
colnames(temp1)[1]<-"date"
save(temp1,file="NewswhipRU/Russia_k60_sentiment_byday_060922.Rda")

#########################################################
######### Step 7 DV and IVS
#########################################################
#########################################################

####Create DV and IVS
##load Russia_k60_each_topic_gamma_sentiment for ANTMN and Latest Date
load("~/NewsWhip/Newswhip/Russia/domain_nw/RDA/Russia_k60_each_topic_gamma_sentiment_060922.Rda")
temp <-temp_test3 %>% 
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, tw_data.tw_count)) %>%
  dplyr::select(date, index, 
                uuid, 
                engagement,
                BlueCom1, OrangeCom2, GreenCom3, PurpleCom4,
                neg_BlueCom1, neg_OrangeCom2, neg_GreenCom3, neg_PurpleCom4,
                pos_BlueCom1, pos_OrangeCom2, pos_GreenCom3, pos_PurpleCom4,
                trust_BlueCom1, trust_OrangeCom2, trust_GreenCom3, trust_PurpleCom4,
                sentiment_BlueCom1, sentiment_OrangeCom2, sentiment_GreenCom3, sentiment_PurpleCom4)

temp_by_day<-aggregate(x=temp[,5:ncol(temp)],by=list(temp$date),FUN="mean")
colnames(temp_by_day)[1]<-"date"  
save(temp_by_day,file="RDA/Russia_IVs_ANTMN_NoEngamament_060922.Rda")
rm(temp_test3)
gc()

###for enaggement create a temp folder
temp_engagement<-temp
temp_engagement %>%
  count(engagement)
for (i in 5:ncol(temp_engagement)) {
  temp_engagement[ , i] <- temp_engagement[ , i] * temp_engagement$engagement
}

###create engagement by day
engagement_by_day<-aggregate(x=temp_engagement[,5:ncol(temp_engagement)],by=list(temp_engagement$date),FUN="mean")
colnames(engagement_by_day)[1]<-"date"  
save(engagement_by_day,file="RDA/Russia_IVs_ANTMN_engagement_060922.Rda")

###for robustness no zero n+1
temp_engagement_v2<-temp %>%
  rowwise() %>%
  mutate(engagement_V2=engagement+1) %>% 
  relocate(engagement_V2, .after = engagement)

###see if it worked
temp_engagement_v2 %>%
  count(engagement_V2)
##run the loop to multily topics
for (i in 6:ncol(temp_engagement_v2)) {
  temp_engagement_v2[ , i] <- temp_engagement_v2[ , i] * temp_engagement_v2$engagement_V2
}

###engagment by day without zeros change start colum number
engagement_by_day_v2<-aggregate(x=temp_engagement_v2[,6:ncol(temp_engagement_v2)],by=list(temp_engagement_v2$date),FUN="mean")
colnames(engagement_by_day_v2)[1]<-"date"  
save(engagement_by_day_v2,file="RDA/Russia_IVs_engagement_robustness_060922.Rda")


####Load DV's
UMDIndicatorsRussia_vaccinated_appointment_or_accept <- readr::read_csv("DATA/UMDIndicatorsRussia_vaccinated_appointment_or_accept.csv")
#UMDIndicatorsRussia_pct_vu_v2$date<- mdy(UMDIndicatorsRussia_pct_vu_v2$date)
tempDV_1<-UMDIndicatorsRussia_vaccinated_appointment_or_accept  %>%
  dplyr::select(pct_vaccinated_appointment_or_accept, date) %>%
  rename('pct_vu' = 'pct_vaccinated_appointment_or_accept')
summary(tempDV_1$pct_vu)

UMDIndicatorsRussia_concernedsideeffects_052022 <- readr::read_csv("DATA/UMDIndicatorsRussia_concernedsideeffects_052022.csv")
tempDV_2<-UMDIndicatorsRussia_concernedsideeffects_052022  %>%
  dplyr::select(pct_concerned_sideeffects, date) %>%
  rename('pct_cse' = 'pct_concerned_sideeffects')
summary(tempDV_2$pct_cse)

temp_DV_IV<-left_join(temp_by_day, tempDV_1, by="date")
temp_DV_IV<-left_join(temp_DV_IV, tempDV_2, by="date")
temp_DV_IV<-temp_DV_IV %>%
  filter(date>'2021-05-19')
save(temp_DV_IV,file="RDA/Russia_NoEng_DV_IVs_060922.Rda")

eng_DV_IV<-left_join(engagement_by_day, tempDV_1, by="date")
eng_DV_IV<-left_join(eng_DV_IV, tempDV_2, by="date")
eng_DV_IV<-eng_DV_IV %>%
  filter(date>'2021-05-19')
save(eng_DV_IV,file="RDA/Russia_Eng_DV_IVs_060922.Rda")

#3rd dataset
eng_v2_DV_IV<-left_join(engagement_by_day_v2, tempDV_1, by="date")
eng_v2_DV_IV<-left_join(eng_v2_DV_IV, tempDV_2, by="date")
eng_v2_DV_IV<-eng_v2_DV_IV %>%
  filter(date>'2021-05-19')
save(eng_v2_DV_IV,file="RDA/Russia_Eng_DV_IVs_robustness_060922.Rda")


vtable::sumtable(temp_DV_IV)
vtable::sumtable(eng_DV_IV)
vtable::sumtable(eng_v2_DV_IV)

########################################################
########    Visualization 2             ################
########################################################

###do a scatter temp_test
fb<- temp_test3 %>%
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, tw_data.tw_count)) %>%
  dplyr::select(date, engagement) 
##change date to date
fb$date<-ymd(fb$date)

##first do a scatter plot with engagement
ggplot(fb, aes(x=date, y=engagement)) +
  geom_point() +
  scale_x_date(date_breaks="1 month", date_labels = "%b-%Y") +
  scale_y_continuous(label=comma) +
  labs(x=" ", 
       y="Facebook&Twitter\nTotal Engagement Count")+
  theme_wsj(color="white") +
  theme(plot.title = element_text(family="sans", size = 10),
        axis.text.x=element_text(size=10, angle = 45, vjust=0.65, family="sans"),
        axis.text.y=element_text(family="sans", size = 10),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=10, family="sans"),
        legend.position="none", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())
ggsave("visuals/Engagement_Scatter_052022.tiff", width=8.5, height=5, dpi=300)

#########################################################
######### Step 8 Time Series Analysis
#########################################################
#########################################################

###########################################################
###########################################################
###############Time Series for Significant Results#########
###########################################################
###########################################################
library(forecast)
library(tseries)
library(dynlm)
library(vars)
library(jtools)
library(stargazer)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(ggthemes)
library(wesanderson)
library(ggrepel)
library(ggpubr)
library(lubridate)
library(scales)
dev.off()


####Load data latest date##
load("~/NewsWhip/Newswhip/Russia/domain_nw/RDA/Russia_Eng_DV_IVs_060922.Rda")

###create descriptive statistics
eng_DV_IV %>% dplyr::select(-date) %>%
  vtable::sumtable(summ=c('notNA(x)', 'mean(x)', 'sd(x)', 'var(x)', 'min(x)','max(x)'),
                   out='csv', 
                   file="CSVandExcel/Russia_ANTMN_DV_IV_Desc_Engagement_060922.csv")


###run descriptive graphs
variables_long<-eng_DV_IV %>% 
  dplyr::select(date,
                GreenCom3,
                neg_GreenCom3,
                pos_GreenCom3,
                pct_vu) %>%
  mutate(date = ymd(date)) %>%
  rename('Green Community(#3) Cluster Prominence' = 'GreenCom3',
         'Green Community(#3) Negative Sentiment' = 'neg_GreenCom3',
         'Green Community(#3) Positive Sentiment' = 'pos_GreenCom3',
         'Vaccine Acceptance' = 'pct_vu') %>%
  pivot_longer(!date, names_to = "variable", values_to = "score")

##facetwrap
ggplot(variables_long, aes(x=date, y=score)) + 
  geom_line(aes(color=variable)) +
  #geom_smooth(stat = "smooth",
  #method = "loess",
  #se = FALSE) +
  facet_wrap(~variable, 
             nrow = 2,
             ncol = 2,
             scales="free_y") +
  scale_x_date(date_breaks="15 days", date_labels = "%d-%b-%Y") +
  labs(title = " ",
       x=" ", 
       y=" ")+
  theme_wsj(color="white") +
  theme(plot.title = element_text(family="sans", size = 10),
        axis.text.x=element_text(angle = 45, vjust = 0.5, size=8, family="sans"),
        axis.text.y=element_text(family="sans", size=8),
        axis.title.x=element_text(vjust=-0.25, size=8, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=8, family="sans"),
        legend.position="none", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())
ggsave("visuals/Figure3_Sig_Variables_Time_061522.tiff", width=8.5, height=5, dpi=300)



######################################################
#############Model 3 GreenCom3 Pct_VU##############
#####################################################
#######Model 3.1 
dat_comm3.1<-eng_DV_IV %>% 
  dplyr::select(GreenCom3, pct_vu) %>%
  drop_na()
dat_comm3.1 <- ts(dat_comm3.1)
plot(dat_comm3.1)

#Test for stationarity
Acf(dat_comm3.1[,"GreenCom3"])
Acf(dat_comm3.1[,"pct_vu"])
adf.test(dat_comm3.1[,"GreenCom3"], k=3)
adf.test(dat_comm3.1[,"pct_vu"], k=3)
kpss.test(dat_comm3.1[,"GreenCom3"])
kpss.test(dat_comm3.1[,"pct_vu"])
coint3.1 <- dynlm(GreenCom3~pct_vu, data=dat_comm3.1)
ehat3.1 <- resid(coint3.1)
adf.test(ehat3.1, k=3)

#VAR Comm3.1
fitvarcomm3.1 <- VAR(dat_comm3.1, p=3, type="both")
summary(fitvarcomm3.1) # comm3.1 pct_vu
causality(fitvarcomm3.1, cause = "GreenCom3")
#causality(fitvarcomm3.1, cause = "")
irf_comm3.1_pct_vu <- irf(fitvarcomm3.1, impulse = "GreenCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.1_pct_vu)
plot(fevd(fitvarcomm3.1))

####model 3.2 neg_GreenCom3 and pct_vu
dat_comm3.2<-eng_DV_IV %>% 
  dplyr::select(neg_GreenCom3, pct_vu) %>%
  drop_na()
dat_comm3.2 <- ts(dat_comm3.2)
plot(dat_comm3.2)

#Test for stationarity
Acf(dat_comm3.2[,"neg_GreenCom3"])
Acf(dat_comm3.2[,"pct_vu"])
adf.test(dat_comm3.2[,"neg_GreenCom3"], k=3)
adf.test(dat_comm3.2[,"pct_vu"], k=3)
kpss.test(dat_comm3.2[,"neg_GreenCom3"])
kpss.test(dat_comm3.2[,"pct_vu"])
coint3.2 <- dynlm(neg_GreenCom3~pct_vu, data=dat_comm3.2)
ehat3.2 <- resid(coint3.2)
adf.test(ehat3.2, k=3)

#VAR Comm3.2
fitvarcomm3.2 <- VAR(dat_comm3.2, p=3, type="both")
summary(fitvarcomm3.2) # comm3.2 pct_vu
causality(fitvarcomm3.2, cause = "neg_GreenCom3")
#causality(fitvarcomm3.2, cause = "")
irf_comm3.2_pct_vu <- irf(fitvarcomm3.2, impulse = "neg_GreenCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.2_pct_vu)
plot(fevd(fitvarcomm3.2))

####model 3.3 pos_GreenCom3 and pct_vu
dat_comm3.3<-eng_DV_IV %>% 
  dplyr::select(pos_GreenCom3, pct_vu) %>%
  drop_na()
dat_comm3.3 <- ts(dat_comm3.3)
plot(dat_comm3.3)

#Test for stationarity
Acf(dat_comm3.3[,"pos_GreenCom3"])
Acf(dat_comm3.3[,"pct_vu"])
adf.test(dat_comm3.3[,"pos_GreenCom3"], k=3)
adf.test(dat_comm3.3[,"pct_vu"], k=3)
kpss.test(dat_comm3.3[,"pos_GreenCom3"])
kpss.test(dat_comm3.3[,"pct_vu"])
coint3.3 <- dynlm(pos_GreenCom3~pct_vu, data=dat_comm3.3)
ehat3.3 <- resid(coint3.3)
adf.test(ehat3.3, k=3)

#VAR Comm3.3
fitvarcomm3.3 <- VAR(dat_comm3.3, p=3, type="both")
summary(fitvarcomm3.3) # comm3.3 pct_vu
causality(fitvarcomm3.3, cause = "pos_GreenCom3")
#causality(fitvarcomm3.3, cause = "")
irf_comm3.3_pct_vu <- irf(fitvarcomm3.3, impulse = "pos_GreenCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.3_pct_vu)
plot(fevd(fitvarcomm3.3))

##############################################
###### Model significant results################
varcomm3.1G <- fitvarcomm3.1[["varresult"]]$pct_vu
varcomm3.2G <- fitvarcomm3.2[["varresult"]]$pct_vu
varcomm3.3G <- fitvarcomm3.3[["varresult"]]$pct_vu

##model 
##use SjPlot
library(sjPlot)
tab_model(varcomm3.1G, varcomm3.2G, varcomm3.3G,
          show.se = TRUE,
          collapse.ci = TRUE,
          digits = 5)

#######################################################
##############  Visualization 3        ################
#######################################################


#### Graph the IRF functions
#create function to retrieve IRF model info
getIRFPlotData <- function(impulse, response, list) {
  cbind.data.frame(Comm = 0:(nrow(list[[1]][[1]])-1),
                   Lower = list[[2]][names(list[[2]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   irf = list[[1]][names(list[[1]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Upper = list[[3]][names(list[[3]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Impulse = impulse,
                   Response = response, stringsAsFactors = FALSE)
}

##get the ones with significance
cluster <- getIRFPlotData("GreenCom3", "pct_vu", irf_comm3.1_pct_vu)
negative <- getIRFPlotData("neg_GreenCom3", "pct_vu", irf_comm3.2_pct_vu)
positive <- getIRFPlotData("pos_GreenCom3", "pct_vu", irf_comm3.3_pct_vu)


plot_all <- rbind(cluster, negative, positive)
names(plot_all)
nrow(plot_all)
plot_all <- plot_all %>% mutate(group = paste0(Impulse, Response))
plot_all<-as.data.frame(plot_all)
plot_all$Response<-recode(plot_all$Response, pct_vu = "Vaccination Acceptance")
plot_all_cluster <- plot_all[1:11,]
plot_all_negative <- plot_all[12:22,]
plot_all_positive <- plot_all[23:33,]
plot_all_long <- gather(plot_all, "key", "n", 2:4) %>% mutate(group = paste0(Impulse, Response))
names(plot_all_long)

##set colors
wesanderson<-wes_palette("BottleRocket1")

group_names <- list(
  'GreenCom3pct_vu' = "Panel 1: Green Community(#3) Cluster Prominence",
  'neg_GreenCom3pct_vu' = "Panel 2: Green Community(#3) Negative Sentiment",
  'pos_GreenCom3pct_vu' = "Panel 3: Green Community(#3) Positive Sentiment")
group_labeller <- function(variable,value){
  return(group_names[value])
}

ggplot(plot_all, aes(x = Comm, y = irf)) +
  geom_line(aes(x = Comm, y = irf), color = "red2", size = 1.5) + 
  geom_line(aes(x = Comm, y = Upper) , linetype = "dashed")+ 
  geom_line(aes(x = Comm, y = Lower), linetype = "dashed")+ 
  geom_hline(aes(yintercept=0), 
             linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) + 
  facet_wrap(.~group, 
             nrow = 2,
             labeller=group_labeller,
             scales = "free_x") +
  labs(x = "time(days)",
       y= "Response to Vaccination Acceptance %") + 
  #ggtitle("Panel 2: Impulse Response Function of Trust") +
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="sans"),
        title=element_text(size=8,family="sans"),
        axis.text.x=element_text(angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="sans"),
        legend.position="none", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="sans")) 
ggsave("visuals/Figure5_IRF_061322.tiff", width=8.5, height=5, dpi=300)


####pull top articles in GreenCom

# ###pull top articles for each use temp_engagement
temp_eng<-merge(x = temp_engagement, y = temp_test3[ , c("index", "link", "headline", "neg_GreenCom3", "negative", "pos_GreenCom3", "positive", "GreenCom3", "text")], by = "index", all.x=TRUE)

df_greencom3 <-temp_eng %>%
  select(date, 
         index, 
         uuid,
         link, 
         headline, 
         engagement,
         GreenCom3) %>%
  slice_max(GreenCom3, n = 20)
openxlsx::write.xlsx(df_greencom3, "CSVandExcel/Russia_Top_Green_Com3_Engagement.xlsx")

##top negative
df_neg_greencom3 <-temp_eng %>%
  select(date, 
         index,
         uuid, 
         link, 
         headline, 
         engagement,
         neg_GreenCom3) %>%
  slice_max(neg_GreenCom3, n = 20)
openxlsx::write.xlsx(df_neg_greencom3, "CSVandExcel/Russia_Top_Neg_Green_Com3_Engagement.xlsx")

##top positive
df_pos_greencom3 <-temp_eng %>%
  select(date, 
         index,
         uuid, 
         link, 
         headline, 
         engagement,
         pos_GreenCom3) %>%
  slice_max(pos_GreenCom3, n = 20)
openxlsx::write.xlsx(df_pos_greencom3, "CSVandExcel/Russia_Top_Pos_Green_Com3_Engagement.xlsx")



