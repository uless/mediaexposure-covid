library(tidyverse)
library(stm) # version v1.3.6
library(ggplot2)
library(tm)
library(xlsx)
require(NLP)
library(purrrlyr)
library(lmtest)
library(tseries)
library(vars)
library(magick)
library(dplyr)
library(data.table)
#library(nlme)
#library(lme4)
library(haven)
library(stringr)
library(tidytext)
library(tidyr)
library(lavaan)


##### 01 News Articles #####
articles <- read.csv("USarticles_Jan2020toJul2021_Full.csv",
                     fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                     row.names = NULL, 
                     stringsAsFactors = FALSE)%>%
  select(-X)


###### Read files & Processing ######
#Note: we missed 3 months of data in the previous version!!
#In reality, we have 41,235 news articles from these news sources

articles_2 <- read.csv("US_Articles_2019-2021.csv",
                     fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                     row.names = NULL, 
                     stringsAsFactors = FALSE)%>%
  select(-X)

colnames(articles_2)
articles_2$PD_new <- as.Date(articles_2$PD_new)
hist(articles_2$PD_new,breaks = "weeks")
articles_2$PD_new <- as.Date(articles_2$PD, format = "%d-%m-%Y")
articles_2 <- articles_2 %>% select(SN,TD,PD_new)


articles_3 <- read.csv("Articles Dec-Oct USonly.csv",
                       fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                       row.names = NULL, 
                       stringsAsFactors = FALSE)%>%
  select(-X)

articles_3$PD <- gsub(' January ', '-01-', articles_3$PD)
articles_3$PD <- gsub(' February ', '-02-', articles_3$PD)
articles_3$PD <- gsub(' March ', '-03-', articles_3$PD)
articles_3$PD <- gsub(' April ', '-04-', articles_3$PD)
articles_3$PD <- gsub(' May ', '-05-', articles_3$PD)
articles_3$PD <- gsub(' June ', '-06-', articles_3$PD)
articles_3$PD <- gsub(' July ', '-07-', articles_3$PD)
articles_3$PD <- gsub(' August ', '-08-', articles_3$PD)
articles_3$PD <- gsub(' September ', '-09-', articles_3$PD)
articles_3$PD <- gsub(' October ', '-10-', articles_3$PD)
articles_3$PD <- gsub(' November ', '-11-', articles_3$PD)
articles_3$PD <- gsub(' December ', '-12-', articles_3$PD)
articles_3$PD_new <- as.Date(articles_3$PD, format = "%d-%m-%Y")
articles_3 <-articles_3%>%select(SN,TD,HD,PD_new)

hist(articles_3$PD_new,breaks = "days")


articles_sub <- readxl::read_xlsx("round2sub_fulltext.xlsx")%>%select(-`...1`)
articles_sub$PD <- gsub(' January ', '-01-', articles_sub$PD)
articles_sub$PD <- gsub(' February ', '-02-', articles_sub$PD)
articles_sub$PD <- gsub(' March ', '-03-', articles_sub$PD)
articles_sub$PD <- gsub(' April ', '-04-', articles_sub$PD)
articles_sub$PD <- gsub(' May ', '-05-', articles_sub$PD)
articles_sub$PD <- gsub(' June ', '-06-', articles_sub$PD)
articles_sub$PD <- gsub(' July ', '-07-', articles_sub$PD)
articles_sub$PD <- gsub(' August ', '-08-', articles_sub$PD)
articles_sub$PD <- gsub(' September ', '-09-', articles_sub$PD)
articles_sub$PD <- gsub(' October ', '-10-', articles_sub$PD)
articles_sub$PD <- gsub(' November ', '-11-', articles_sub$PD)
articles_sub$PD <- gsub(' December ', '-12-', articles_sub$PD)
articles_sub$PD_new <- as.Date(articles_sub$PD, format = "%d-%m-%Y")
articles_sub <-articles_sub%>%select(SN,TD,HD,PD_new)

articles <- rbind(articles_3,articles_sub)
hist(articles$PD_new,breaks = "weeks")


articles_4 <- read.csv("round3_fulltext.csv",
                       fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                       row.names = NULL, 
                       stringsAsFactors = FALSE)%>%
  select(-X)

articles_4$PD <- gsub(' January ', '-01-', articles_4$PD)
articles_4$PD <- gsub(' February ', '-02-', articles_4$PD)
articles_4$PD <- gsub(' March ', '-03-', articles_4$PD)
articles_4$PD <- gsub(' April ', '-04-', articles_4$PD)
articles_4$PD <- gsub(' May ', '-05-', articles_4$PD)
articles_4$PD <- gsub(' June ', '-06-', articles_4$PD)
articles_4$PD <- gsub(' July ', '-07-', articles_4$PD)
articles_4$PD <- gsub(' August ', '-08-', articles_4$PD)
articles_4$PD <- gsub(' September ', '-09-', articles_4$PD)
articles_4$PD <- gsub(' October ', '-10-', articles_4$PD)
articles_4$PD <- gsub(' November ', '-11-', articles_4$PD)
articles_4$PD <- gsub(' December ', '-12-', articles_4$PD)
articles_4$PD_new <- as.Date(articles_4$PD, format = "%d-%m-%Y")
articles_4 <-articles_4%>%select(SN,TD,HD,PD_new)

hist(articles_4$PD_new,breaks = "weeks")

articles <- rbind(articles,articles_4)
articles <- distinct(articles, PD_new,SN,HD,.keep_all= TRUE)
hist(articles$PD_new,breaks = "months")

articles_sub <- NULL
articles_2 <- NULL
articles_3 <- NULL
articles_4 <- NULL

write.csv(articles,"USarticles_Jan2020toJul2021_Full.csv")
write.xlsx(articles, "USarticles_Jan2020toJul2021_Full.xlsx")

#Now, the over-written file is the correct one with the real full data
#Next time, just neglect the pre-processes.

articles_full <- read.csv("USarticles_Jan2020toJul2021_Full.csv",
                          fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                          row.names = NULL, 
                          stringsAsFactors = FALSE)%>%
  select(-X)

articles_perday <- dplyr::count(articles_full,PD_new)



###### Filtering protective keywords only ######
mask_patterns <- "(mask)|(face cover*)|(mask wear)|(facemask)"
vaccinate_patterns <- "(vaccin*)"
wash_patterns <- "(wash hand*)|(hand wash*)|(hand sani*)"
quarantine_patterns <- "(quarantin*)|(stay at home)|(stay-at-home)|(isolat*)|(social distanc*)|(social-distanc*)"
avoidall_patterns <- "(avoid travel)|(avoid public)|(avoid crowd*)|(avoid gather*)|(avoid people)|(avoid others)|(social distanc*)|(social-distanc*)"

protective_patterns <- paste(mask_patterns,avoidall_patterns,
                             vaccinate_patterns,wash_patterns,quarantine_patterns,
                             sep = "|", collapse = NULL)

articles <- iconv(articles, 'UTF-8', 'ASCII')
articles$TD <- as.character(articles$TD)
articles$TD_lower <- tolower(articles$TD)

articles$TD[1]
tolower(articles$TD[1])
iconv(articles$TD[1], 'UTF-8', 'ASCII')

protective <- articles[grepl(pattern = protective_patterns, articles$TD_lower),]
protective <- protective %>% select(PD_new,SN,HD,TD,Day)
protective_perday <- dplyr::count(protective,PD_new)

###### Filtering II: by topic models ######
protective$Day <- as.numeric(protective$PD_new)
processed <- textProcessor(protective$TD, metadata = protective)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

ksearch <- searchK(out$documents, out$vocab, 
                   K = c(10:50), seed = 1000,
                   prevalence =~ SN + s(Day), 
                   data = out$meta)
plot(ksearch)
#K=47 should be the optimal option, because it has among the highest heldout likelihood,
#Have among the smallest residuals, high in lower bound
#and most importantly, have good (appropriate) semantic coherence

processed <- textProcessor(protective$TD, metadata = protective)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


Fit_A <- stm(out$documents,out$vocab, 
             K=47, prevalence = ~ SN + s(Day),
             max.em.its = 500,
             seed = 1000,
             data=out$meta, 
             init.type = "Spectral")

summary(Fit_A)
labelTopics(Fit_A,n = 10)


likeinfer <-as.data.frame(Fit_A$theta)
colnames(likeinfer)<-c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", 
                       "Topic6", "Topic7", "Topic8", "Topic9", "Topic10",
                       "Topic11", "Topic12", "Topic13", "Topic14", "Topic15",
                       "Topic16", "Topic17", "Topic18", "Topic19", "Topic20",
                       "Topic21", "Topic22", "Topic23", "Topic24", "Topic25",
                       "Topic26", "Topic27", "Topic28", "Topic29", "Topic30",
                       "Topic31", "Topic32", "Topic33", "Topic34", "Topic35",
                       "Topic36", "Topic37", "Topic38", "Topic39", "Topic40",
                       "Topic41", "Topic42", "Topic43", "Topic44", "Topic45",
                       "Topic46", "Topic47")
likeinfer<-as.data.frame(likeinfer)
row_handler <- function(row.data){  
  index <- which(row.data == max(row.data)) 
  out <- names(row.data[index])
  return(out)}

inferred<-likeinfer %>%
  by_row(..f = row_handler, .collate = "rows", .to = "InferTopic")
df_infer<-bind_cols(out$meta, inferred$InferTopic)
names(df_infer)[7] <- "Topic"

findexamples <- function(model,text,number, topic){
  findThoughts(model,texts = as.character(text), n = number, topics = topic)$docs[[1]]
}

findexamples(Fit_A,out$meta$TD,1,12)

for( i in 13:47)
{
  print(paste0("Find responses with topic: ", i))
  print(findexamples(Fit_A,out$meta$TD,1,i))
}

#Going through filter here in Excel file
#Decided to keep some of the topics that is highly associated with
#health, pandemic update, policy update, etc.,
#Also included coverage of schooling/working during the pandemic
#Some ambivalent contents such as global politics or entertainment are removed


df_infer <- filter(df_infer,Topic == 'Topic5' |Topic == 'Topic6' |Topic == 'Topic8' |
                     Topic == 'Topic9' |Topic == 'Topic10' |Topic == 'Topic11' |
                     Topic == 'Topic12' |Topic == 'Topic14' |Topic == 'Topic16' |
                     Topic == 'Topic17' |Topic == 'Topic21' |Topic == 'Topic22' |
                     Topic == 'Topic25' |Topic == 'Topic27' |Topic == 'Topic30' |
                     Topic == 'Topic32' |Topic == 'Topic34' |Topic == 'Topic35' |
                     Topic == 'Topic37' |Topic == 'Topic38' |Topic == 'Topic41' |Topic == 'Topic45')

#Got 10,528 articles after this filter

write.csv(df_infer,"articles_filtered_topic.csv")

##### 02 Twitter Data, already cleaned and saved in previous practices#####
tweet_perday <- read.csv("tweet_perday.csv",
                         fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                         row.names = NULL, 
                         stringsAsFactors = FALSE)%>%
  select(-X)

mask_twi_perday <- read.csv("mask_twi_perday.csv",
                            fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                            row.names = NULL, 
                            stringsAsFactors = FALSE)%>%
  select(-X)

avoidall_twi_perday <- read.csv("avoidall_twi_perday.csv",
                                fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                                row.names = NULL, 
                                stringsAsFactors = FALSE)%>%
  select(-X)

quarantine_twi_perday <- read.csv("quarantine_twi_perday.csv",
                               fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                               row.names = NULL, 
                               stringsAsFactors = FALSE)%>%
  select(-X)

vaccinate_twi_perday <- read.csv("vaccinate_twi_perday.csv",
                                 fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                                 row.names = NULL, 
                                 stringsAsFactors = FALSE)%>%
  select(-X)

wash_twi_perday <- read.csv("wash_twi_perday.csv",
                            fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                            row.names = NULL, 
                            stringsAsFactors = FALSE)%>%
  select(-X)

###### Read filtered newspaper entries ######
articles <- read.csv("articles_filtered_topic.csv",
                     fill=TRUE, header=TRUE, sep=",", 
                     row.names = NULL, 
                     stringsAsFactors = FALSE)%>%
  select(-X)

#articles$TD_lower <- tolower(articles$TD)
protective_perday <- dplyr::count(articles,PD_new)
mask <- articles[grepl(pattern = mask_patterns, articles$TD_lower),]
mask_perday <- dplyr::count(mask,PD_new)
mask <- NULL
#write.csv(mask_perday,"mask_perday.csv")

avoidall <- articles[grepl(pattern = avoidall_patterns, articles$TD_lower),]
avoidall_perday <- dplyr::count(avoidall,PD_new)
avoidall <-NULL
#write.csv(avoidall_perday,"avoidall_perday.csv")

quarantine <- articles[grepl(pattern = quarantine_patterns, articles$TD_lower),]
quarantine_perday <- dplyr::count(quarantine,PD_new)
quarantine <-NULL
#write.csv(quarantine_perday,"quarantine_perday.csv")

vaccinate <- articles[grepl(pattern = vaccinate_patterns, articles$TD_lower),]
vaccinate_perday <- dplyr::count(vaccinate,PD_new)
vaccinate <-NULL
#write.csv(vaccinate_perday,"vaccinate_perday.csv")

wash <- articles[grepl(pattern = wash_patterns, articles$TD_lower),]
wash_perday <- dplyr::count(wash,PD_new)
wash <-NULL

###### Causality between news and tweet [Prep] ######
#reading & cleaning
mask_perday$PD_new <- as.Date(mask_perday$PD_new)
mask_twi_perday <- mask_twi_perday %>% rename("PD_new" = "Tweet.Date..UTC.") %>%
  rename("n_twi" = "n")
mask_twi_perday$PD_new <- as.Date(mask_twi_perday$PD_new)
media_mask_perday <- full_join(mask_twi_perday,mask_perday)
media_mask_perday$PD_new <- as.Date(media_mask_perday$PD_new)

wash_twi_perday$Tweet.Date..UTC. <- as.Date(wash_twi_perday$Tweet.Date..UTC.)
wash_perday$PD_new <- as.Date(wash_perday$PD_new)
wash_twi_perday <- wash_twi_perday %>% rename("PD_new" = "Tweet.Date..UTC.") %>%
  rename("n_twi" = "n")
media_wash_perday <- full_join(wash_twi_perday,wash_perday)
media_wash_perday$PD_new <- as.Date(media_wash_perday$PD_new)

avoidall_twi_perday$Tweet.Date..UTC. <- as.Date(avoidall_twi_perday$Tweet.Date..UTC.)
avoidall_perday$PD_new <- as.Date(avoidall_perday$PD_new)
avoidall_twi_perday <- avoidall_twi_perday %>% rename("PD_new" = "Tweet.Date..UTC.") %>%
  rename("n_twi" = "n")
media_avoidall_perday <- full_join(avoidall_twi_perday,avoidall_perday)
media_avoidall_perday$PD_new <- as.Date(media_avoidall_perday$PD_new)

quarantine_twi_perday$Tweet.Date..UTC. <- as.Date(quarantine_twi_perday$Tweet.Date..UTC.)
quarantine_perday$PD_new <- as.Date(quarantine_perday$PD_new)
quarantine_twi_perday <- quarantine_twi_perday %>% rename("PD_new" = "Tweet.Date..UTC.") %>%
  rename("n_twi" = "n")
media_quarantine_perday <- full_join(quarantine_twi_perday,quarantine_perday)
media_quarantine_perday$PD_new <- as.Date(media_quarantine_perday$PD_new)

vaccinate_twi_perday$Tweet.Date..UTC. <- as.Date(vaccinate_twi_perday$Tweet.Date..UTC.)
vaccinate_perday$PD_new <- as.Date(vaccinate_perday$PD_new)
vaccinate_twi_perday <- vaccinate_twi_perday %>% rename("PD_new" = "Tweet.Date..UTC.") %>%
  rename("n_twi" = "n")
media_vaccinate_perday <- full_join(vaccinate_twi_perday,vaccinate_perday)
media_vaccinate_perday$PD_new <- as.Date(media_vaccinate_perday$PD_new)


#plotting
media_mask_perday_std <- media_mask_perday %>% 
  mutate(across(where(is.numeric), scale))
media_wash_perday_std <- media_wash_perday %>% 
  mutate(across(where(is.numeric), scale))
media_avoidall_perday_std <- media_avoidall_perday %>% 
  mutate(across(where(is.numeric), scale))
media_quarantine_perday_std <- media_quarantine_perday %>% 
  mutate(across(where(is.numeric), scale))
media_vaccinate_perday_std <- media_vaccinate_perday %>% 
  mutate(across(where(is.numeric), scale))



plots <- image_graph(height = 250) 
ggplot(media_mask_perday_std,aes(x=PD_new))+
  geom_line(aes(y=n_twi),color="darkred")+
  geom_line(aes(y=n),color="steelblue",linetype="twodash")+
  labs(x = "", y = "Coverage on Masks \n (standardized)")+
  theme_classic()
ggplot(media_wash_perday_std,aes(x=PD_new))+
  geom_line(aes(y=n_twi),color="darkred")+
  geom_line(aes(y=n),color="steelblue",linetype="twodash")+
  labs(x = "", y = "Coverage on Washing Hands \n (standardized)")+
  theme_classic()
ggplot(media_avoidall_perday_std,aes(x=PD_new))+
  geom_line(aes(y=n_twi),color="darkred")+
  geom_line(aes(y=n),color="steelblue",linetype="twodash")+
  labs(x = "", y = "Coverage on Avoiding Public\n (standardized)")+
  theme_classic()
ggplot(media_quarantine_perday_std,aes(x=PD_new))+
  geom_line(aes(y=n_twi),color="darkred")+
  geom_line(aes(y=n),color="steelblue",linetype="twodash")+
  labs(x = "", y = "Coverage on Quanrantine \n (standardized)")+
  theme_classic()
ggplot(media_vaccinate_perday_std,aes(x=PD_new))+
  geom_line(aes(y=n_twi),color="darkred")+
  geom_line(aes(y=n),color="steelblue",linetype="twodash")+
  labs(x = "Publish Date", y = "Coverage on Vaccination \n (standardized)")+
  theme_classic()
dev.off()
im6 <- image_append(plots, stack = T)
image_write(im6, 'standardized coverage plots.png')


###### Causality between news and tweet [Model] ######
#Since the data is not non stationary, we will not need to use cointegration test
#Marginal significance quarantine from news to twitter, significance from  twitter to news
grangertest(media_mask_perday$n,media_mask_perday$n_twi, order = 1, na.action = na.omit)
grangertest(media_mask_perday$n,media_mask_perday$n_twi, order = 2, na.action = na.omit)
grangertest(media_mask_perday$n,media_mask_perday$n_twi, order = 3, na.action = na.omit)
grangertest(media_mask_perday$n,media_mask_perday$n_twi, order = 4, na.action = na.omit)
grangertest(media_mask_perday$n,media_mask_perday$n_twi, order = 5, na.action = na.omit)

grangertest(media_mask_perday$n_twi,media_mask_perday$n, order = 1, na.action = na.omit)
grangertest(media_mask_perday$n_twi,media_mask_perday$n, order = 2, na.action = na.omit)
grangertest(media_mask_perday$n_twi,media_mask_perday$n, order = 3, na.action = na.omit)
grangertest(media_mask_perday$n_twi,media_mask_perday$n, order = 4, na.action = na.omit)
grangertest(media_mask_perday$n_twi,media_mask_perday$n, order = 5, na.action = na.omit)

grangertest(media_avoidall_perday$n,media_avoidall_perday$n_twi, order = 1, na.action = na.omit)
grangertest(media_avoidall_perday$n,media_avoidall_perday$n_twi, order = 2, na.action = na.omit)
grangertest(media_avoidall_perday$n,media_avoidall_perday$n_twi, order = 3, na.action = na.omit)
grangertest(media_avoidall_perday$n,media_avoidall_perday$n_twi, order = 4, na.action = na.omit)
grangertest(media_avoidall_perday$n,media_avoidall_perday$n_twi, order = 5, na.action = na.omit)

grangertest(media_avoidall_perday$n_twi,media_avoidall_perday$n, order = 1, na.action = na.omit)
grangertest(media_avoidall_perday$n_twi,media_avoidall_perday$n, order = 2, na.action = na.omit)
grangertest(media_avoidall_perday$n_twi,media_avoidall_perday$n, order = 3, na.action = na.omit)
grangertest(media_avoidall_perday$n_twi,media_avoidall_perday$n, order = 4, na.action = na.omit)
grangertest(media_avoidall_perday$n_twi,media_avoidall_perday$n, order = 5, na.action = na.omit)


grangertest(media_quarantine_perday$n,media_quarantine_perday$n_twi, order = 1, na.action = na.omit)
grangertest(media_quarantine_perday$n,media_quarantine_perday$n_twi, order = 2, na.action = na.omit)
grangertest(media_quarantine_perday$n,media_quarantine_perday$n_twi, order = 3, na.action = na.omit)
grangertest(media_quarantine_perday$n,media_quarantine_perday$n_twi, order = 4, na.action = na.omit)
grangertest(media_quarantine_perday$n,media_quarantine_perday$n_twi, order = 5, na.action = na.omit)


grangertest(media_quarantine_perday$n_twi,media_quarantine_perday$n, order = 1, na.action = na.omit)
grangertest(media_quarantine_perday$n_twi,media_quarantine_perday$n, order = 2, na.action = na.omit)
grangertest(media_quarantine_perday$n_twi,media_quarantine_perday$n, order = 3, na.action = na.omit)
grangertest(media_quarantine_perday$n_twi,media_quarantine_perday$n, order = 4, na.action = na.omit)
grangertest(media_quarantine_perday$n_twi,media_quarantine_perday$n, order = 5, na.action = na.omit)


grangertest(media_wash_perday$n,media_wash_perday$n_twi, order = 1, na.action = na.omit)
grangertest(media_wash_perday$n,media_wash_perday$n_twi, order = 2, na.action = na.omit)
grangertest(media_wash_perday$n,media_wash_perday$n_twi, order = 3, na.action = na.omit)
grangertest(media_wash_perday$n,media_wash_perday$n_twi, order = 4, na.action = na.omit)
grangertest(media_wash_perday$n,media_wash_perday$n_twi, order = 5, na.action = na.omit)

grangertest(media_wash_perday$n_twi,media_wash_perday$n, order = 1, na.action = na.omit)
grangertest(media_wash_perday$n_twi,media_wash_perday$n, order = 2, na.action = na.omit)
grangertest(media_wash_perday$n_twi,media_wash_perday$n, order = 3, na.action = na.omit)
grangertest(media_wash_perday$n_twi,media_wash_perday$n, order = 4, na.action = na.omit)
grangertest(media_wash_perday$n_twi,media_wash_perday$n, order = 5, na.action = na.omit)


grangertest(media_vaccinate_perday$n,media_vaccinate_perday$n_twi, order = 1, na.action = na.omit)
grangertest(media_vaccinate_perday$n,media_vaccinate_perday$n_twi, order = 2, na.action = na.omit)
grangertest(media_vaccinate_perday$n,media_vaccinate_perday$n_twi, order = 3, na.action = na.omit)
grangertest(media_vaccinate_perday$n,media_vaccinate_perday$n_twi, order = 4, na.action = na.omit)
grangertest(media_vaccinate_perday$n,media_vaccinate_perday$n_twi, order = 5, na.action = na.omit)


grangertest(media_vaccinate_perday$n_twi,media_vaccinate_perday$n, order = 1, na.action = na.omit)
grangertest(media_vaccinate_perday$n_twi,media_vaccinate_perday$n, order = 2, na.action = na.omit)
grangertest(media_vaccinate_perday$n_twi,media_vaccinate_perday$n, order = 3, na.action = na.omit)
grangertest(media_vaccinate_perday$n_twi,media_vaccinate_perday$n, order = 4, na.action = na.omit)
grangertest(media_vaccinate_perday$n_twi,media_vaccinate_perday$n, order = 5, na.action = na.omit)


##### 03 Survey Data #####
###### Read files & Processing ######
survey <- read_sav("Corona_W1-W6_Working_BubbleTrimmed.sav")
colnames(survey)

individual <- survey %>%
  dplyr::select(PROLIFIC_PID,c(S1_Age:S1_Political_Orientation),
         S1_National_Newspapers,
         c(S1_Wash_Risks_low_household:S1_Vaccinate_Household_action),
         S2_National_Newspapers,
         c(S2_Wash_Risks_low_household:S2_Vaccinate_Household_action),
         S3_National_Newspapers,
         c(S3_Wash_Risks_low_household:S3_Vaccinate_Household_action),
         S4_National_Newspapers,
         c(S4_Wash_Risks_low_household:S4_Vaccinate_Household_action),
         S5_National_Newspapers,
         c(S5_Wash_Risks_low_household:S5_Vaccinate_Household_action),
         S6_National_Newspapers,
         c(S6_Wash_Risks_low_household:S6_Vaccinate_Household_action)
  )%>%
  rename(X.U.FEFF.PROLIFIC_PID = PROLIFIC_PID)%>%
  mutate(S1_media = S1_National_Newspapers)%>%
  mutate(S2_media = S2_National_Newspapers)%>%
  mutate(S3_media = S3_National_Newspapers)%>%
  mutate(S4_media = S4_National_Newspapers)%>%
  mutate(S5_media = S5_National_Newspapers)%>%
  mutate(S6_media = S6_National_Newspapers)

trendy <- read.csv("initial_reg_trend.csv")%>%
  dplyr::select(X.U.FEFF.PROLIFIC_PID,       #ID of respondents
         S1_Date,c(S2_Date:S6_Date),  #Date of response
         S1_Monthstart,S1_Weekstart,c(S2_Monthstart:S6_Weekstart),
         c(monthslope1:weekslope6))   #Trend of daily increment in confirmed cases

trendy <- trendy %>% right_join(individual,by="X.U.FEFF.PROLIFIC_PID")

###### Vaccinate ######
vaccinate <- trendy %>%
  rowwise()%>%
  mutate(S1_articles_m = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_articles_w = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Weekstart]))) %>%
  mutate(S1_protect_m = ifelse(is.na(S1_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S1_Date & vaccinate_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_protect_w = ifelse(is.na(S1_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S1_Date & vaccinate_perday$PD_new > S1_Weekstart])))%>%
  mutate(S2_articles_m = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_articles_w = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Weekstart]))) %>%
  mutate(S2_protect_m = ifelse(is.na(S2_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S2_Date & vaccinate_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_protect_w = ifelse(is.na(S2_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S2_Date & vaccinate_perday$PD_new > S2_Weekstart])))%>%
  mutate(S3_articles_m = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_articles_w = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Weekstart]))) %>%
  mutate(S3_protect_m = ifelse(is.na(S3_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S3_Date & vaccinate_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_protect_w = ifelse(is.na(S3_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S3_Date & vaccinate_perday$PD_new > S3_Weekstart])))%>%
  mutate(S4_articles_m = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_articles_w = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Weekstart]))) %>%
  mutate(S4_protect_m = ifelse(is.na(S4_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S4_Date & vaccinate_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_protect_w = ifelse(is.na(S4_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S4_Date & vaccinate_perday$PD_new > S4_Weekstart])))%>%
  mutate(S5_articles_m = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_articles_w = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Weekstart]))) %>%
  mutate(S5_protect_m = ifelse(is.na(S5_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S5_Date & vaccinate_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_protect_w = ifelse(is.na(S5_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S5_Date & vaccinate_perday$PD_new > S5_Weekstart])))%>%
  mutate(S6_articles_m = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_articles_w = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Weekstart]))) %>%
  mutate(S6_protect_m = ifelse(is.na(S6_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S6_Date & vaccinate_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_protect_w = ifelse(is.na(S6_Date), as.integer(NA),sum(vaccinate_perday$n[vaccinate_perday$PD_new < S6_Date & vaccinate_perday$PD_new > S6_Weekstart])))

vaccinate <- vaccinate %>%
  mutate(S1_pct_m = round(S1_protect_m/S1_articles_m,2))%>%
  mutate(S1_pct_w = round(S1_protect_w/S1_articles_w,2))%>%
  mutate(S2_pct_m = round(S2_protect_m/S2_articles_m,2))%>%
  mutate(S2_pct_w = round(S2_protect_w/S2_articles_w,2))%>%
  mutate(S3_pct_m = round(S3_protect_m/S3_articles_m,2))%>%
  mutate(S3_pct_w = round(S3_protect_w/S3_articles_w,2))%>%
  mutate(S4_pct_m = round(S4_protect_m/S4_articles_m,2))%>%
  mutate(S4_pct_w = round(S4_protect_w/S4_articles_w,2))%>%
  mutate(S5_pct_m = round(S5_protect_m/S5_articles_m,2))%>%
  mutate(S5_pct_w = round(S5_protect_w/S5_articles_w,2))%>%
  mutate(S6_pct_m = round(S6_protect_m/S6_articles_m,2))%>%
  mutate(S6_pct_w = round(S6_protect_w/S6_articles_w,2))

mlmreg <- vaccinate

vaccinate_household <- data.frame(
  ID = mlmreg$X.U.FEFF.PROLIFIC_PID,
  age = mlmreg$S1_Age,gender=mlmreg$S1_Gender,
  education = mlmreg$S1_Education,income = mlmreg$S1_Income,
  ethnicity = mlmreg$S1_Ethnicity,party = mlmreg$S1_Political_Party,
  political = mlmreg$S1_Political_Orientation,
  S1_news_consumption = mlmreg$S1_media,S2_news_consumption = mlmreg$S2_media,
  S3_news_consumption = mlmreg$S3_media,S4_news_consumption = mlmreg$S4_media,
  S5_news_consumption = mlmreg$S5_media,S6_news_consumption = mlmreg$S6_media, 
  S1_monthslope = mlmreg$monthslope1,S1_weekslope = mlmreg$weekslope1,
  S1_monthcoverage = mlmreg$S1_pct_m,S1_weekcoverage = mlmreg$S1_pct_w,
  S1_benefit_house = mlmreg$S1_Vaccinate_Risks_low_household,S1_resources = mlmreg$S1_Vacinate_Resources,
  S1_benefit_community = mlmreg$S1_Vaccinate_Risks_low_community,
  S2_monthslope = mlmreg$monthslope2,S2_weekslope = mlmreg$weekslope2,
  S2_monthcoverage = mlmreg$S2_pct_m,S2_weekcoverage = mlmreg$S2_pct_w,
  S2_benefit_house = mlmreg$S2_Vaccinate_Risks_low_household,S2_resources = mlmreg$S2_Vacinate_Resources,
  S2_benefit_community = mlmreg$S2_Vaccinate_Risks_low_community,
  S3_monthslope = mlmreg$monthslope3,S3_weekslope = mlmreg$weekslope3,
  S3_monthcoverage = mlmreg$S3_pct_m,S3_weekcoverage = mlmreg$S3_pct_w,
  S3_benefit_house = mlmreg$S3_Vaccinate_Risks_low_household,S3_resources = mlmreg$S3_Vacinate_Resources,
  S3_benefit_community = mlmreg$S3_Vaccinate_Risks_low_community,
  S4_monthslope = mlmreg$monthslope4,S4_weekslope = mlmreg$weekslope4,
  S4_monthcoverage = mlmreg$S4_pct_m,S4_weekcoverage = mlmreg$S4_pct_w,
  S4_benefit_house = mlmreg$S4_Vaccinate_Risks_low_household,S4_resources = mlmreg$S4_Vacinate_Resources,
  S4_benefit_community = mlmreg$S4_Vaccinate_Risks_low_community,
  S5_monthslope = mlmreg$monthslope5,S5_weekslope = mlmreg$weekslope5,
  S5_monthcoverage = mlmreg$S5_pct_m,S5_weekcoverage = mlmreg$S5_pct_w,
  S5_benefit_house = mlmreg$S5_Vaccinate_Risks_low_household,S5_resources = mlmreg$S5_Vacinate_Resources,
  S5_benefit_community = mlmreg$S5_Vaccinate_Risks_low_community,
  S6_monthslope = mlmreg$monthslope6,S6_weekslope = mlmreg$weekslope6,
  S6_monthcoverage = mlmreg$S6_pct_m,S6_weekcoverage = mlmreg$S6_pct_w,
  S6_benefit_house = mlmreg$S6_Vaccinate_Risks_low_household,S6_resources = mlmreg$S6_Vacinate_Resources,
  S6_benefit_community = mlmreg$S6_Vaccinate_Risks_low_community,
  S1_vaccinate_behavior = mlmreg$S1_Vaccinate_Household_action,
  S2_vaccinate_behavior = mlmreg$S2_Vaccinate_Household_action, 
  S3_vaccinate_behavior = mlmreg$S3_Vaccinate_Household_action,
  S4_vaccinate_behavior = mlmreg$S4_Vaccinate_Household_action,
  S5_vaccinate_behavior = mlmreg$S5_Vaccinate_Household_action,
  S6_vaccinate_behavior = mlmreg$S6_Vaccinate_Household_action)

#newsconsumption
news_consumption <-  vaccinate_household%>%
  dplyr::select(ID,c(S1_news_consumption:S6_news_consumption))%>%
  gather(wave,news_consumption,-ID)
news_consumption$wave <- gsub("[^0-9.-]", "", news_consumption$wave)

#monthslope
monthslope <- vaccinate_household%>%
  dplyr::select(ID,c(S1_monthslope,S2_monthslope,S3_monthslope,
              S4_monthslope,S5_monthslope,S6_monthslope))%>%
  gather(wave,monthslope,-ID)
monthslope$wave <- gsub("[^0-9.-]", "", monthslope$wave)

#weekslope
weekslope <- vaccinate_household%>%
  dplyr::select(ID,c(S1_weekslope,S2_weekslope,S3_weekslope,
              S4_weekslope,S5_weekslope,S6_weekslope))%>%
  gather(wave,weekslope,-ID)
weekslope$wave <- gsub("[^0-9.-]", "", weekslope$wave)

#monthcoverage
monthcoverage <- vaccinate_household%>%
  dplyr::select(ID,c(S1_monthcoverage,S2_monthcoverage,S3_monthcoverage,
              S4_monthcoverage,S5_monthcoverage,S6_monthcoverage))%>%
  gather(wave,monthcoverage,-ID)
monthcoverage$wave <- gsub("[^0-9.-]", "", monthcoverage$wave)

#weekcoverage
weekcoverage <- vaccinate_household%>%
  dplyr::select(ID,c(S1_weekcoverage,S2_weekcoverage,S3_weekcoverage,
              S4_weekcoverage,S5_weekcoverage,S6_weekcoverage))%>%
  gather(wave,weekcoverage,-ID)
weekcoverage$wave <- gsub("[^0-9.-]", "", weekcoverage$wave)

#benefit_house
benefit_house <- vaccinate_household%>%
  dplyr::select(ID,c(S1_benefit_house,S2_benefit_house,S3_benefit_house,
              S4_benefit_house,S5_benefit_house,S6_benefit_house))%>%
  gather(wave,benefit_house,-ID)
benefit_house$wave <- gsub("[^0-9.-]", "", benefit_house$wave)

#resources
resources <- vaccinate_household%>%
  dplyr::select(ID,c(S1_resources,S2_resources,S3_resources,
              S4_resources,S5_resources,S6_resources))%>%
  gather(wave,resources,-ID)
resources$wave <- gsub("[^0-9.-]", "", resources$wave)

#benefit_community
benefit_community <- vaccinate_household%>%
  dplyr::select(ID,c(S1_benefit_community,S2_benefit_community,S3_benefit_community,
              S4_benefit_community,S5_benefit_community,S6_benefit_community))%>%
  gather(wave,benefit_community,-ID)
benefit_community$wave <- gsub("[^0-9.-]", "", benefit_community$wave)

#vaccinate_behavior
vaccinate_behavior <- vaccinate_household%>%
  dplyr::select(ID,c(S1_vaccinate_behavior,S2_vaccinate_behavior,S3_vaccinate_behavior,
              S4_vaccinate_behavior,S5_vaccinate_behavior,S6_vaccinate_behavior))%>%
  gather(wave,vaccinate_behavior,-ID)
vaccinate_behavior$wave <- gsub("[^0-9.-]", "", vaccinate_behavior$wave)


mlmprep <- 
  left_join(news_consumption, monthslope, by = c("ID", "wave")) %>%
  left_join(weekslope, by = c("ID", "wave")) %>%
  left_join(monthcoverage, by = c("ID", "wave")) %>%
  left_join(weekcoverage,by = c("ID", "wave")) %>%
  left_join(benefit_house, by = c("ID", "wave")) %>%
  left_join(benefit_community, by = c("ID", "wave")) %>%
  left_join(resources, by = c("ID", "wave")) %>%
  left_join(vaccinate_behavior, by = c("ID", "wave"))%>%
  left_join(dplyr::select(vaccinate_household,c(ID:political)), by = "ID")

mlmprep$wave <- as.character(mlmprep$wave)
mlmprep$gender <- as.character(mlmprep$gender)
mlmprep$education <- as.character(mlmprep$education)
mlmprep$income <- as.character(mlmprep$income)
mlmprep$ethnicity <- as.character(mlmprep$ethnicity)

mlmprep$news_exposure_w = mlmprep$news_consumption*mlmprep$weekcoverage
mlmprep$news_exposure_m = mlmprep$news_consumption*mlmprep$monthcoverage

mlmprep <- mlmprep %>% 
  mutate(across(where(is.numeric), scale))

model <- '
    level:1
        vaccinate_behavior ~ wave + resources + benefit_house + benefit_community +  monthslope*weekslope
        resources ~ news_exposure_w*news_exposure_m
        benefit_house ~ news_exposure_w*news_exposure_m
        benefit_community ~ news_exposure_w*news_exposure_m
    level:2
        vaccinate_behavior ~ age + gender + education + income + ethnicity + party + political
'

model <- '
    level:1
        vaccinate_behavior ~ a*wave + b*resources + c*benefit_house + d*benefit_community + e*weekslope + f*news_exposure_w
        resources ~ x*news_exposure_w
        benefit_house ~ y*news_exposure_w
        benefit_community ~ z*news_exposure_w
    level:2
        resources ~ age + gender + education + income + ethnicity + party + political
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
    #indirect and total effects
        bx = b*x
        cy = c*y
        dz = d*z
        total = bx + cy + dz + f
'

model <- '
    level:1
        vaccinate_behavior ~ wave + resources + benefit_house + benefit_community + weekslope
        benefit_house ~ news_exposure_w
        benefit_community ~ news_exposure_w
    level:2
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
'

vaccinate_fit <- sem(model = model, data = mlmprep, 
                     cluster = "ID",
                     verbose = TRUE, optim.method = "em", em.iter.max = 5000,
                     em.fx.tol = 1e-04, em.dx.tol = 1e-04)

summary(vaccinate_fit,fit.measures = TRUE)

###### Wash hands ######

wash <- trendy %>%
  rowwise()%>%
  mutate(S1_articles_m = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_articles_w = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Weekstart]))) %>%
  mutate(S1_protect_m = ifelse(is.na(S1_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S1_Date & wash_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_protect_w = ifelse(is.na(S1_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S1_Date & wash_perday$PD_new > S1_Weekstart])))%>%
  mutate(S2_articles_m = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_articles_w = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Weekstart]))) %>%
  mutate(S2_protect_m = ifelse(is.na(S2_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S2_Date & wash_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_protect_w = ifelse(is.na(S2_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S2_Date & wash_perday$PD_new > S2_Weekstart])))%>%
  mutate(S3_articles_m = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_articles_w = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Weekstart]))) %>%
  mutate(S3_protect_m = ifelse(is.na(S3_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S3_Date & wash_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_protect_w = ifelse(is.na(S3_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S3_Date & wash_perday$PD_new > S3_Weekstart])))%>%
  mutate(S4_articles_m = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_articles_w = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Weekstart]))) %>%
  mutate(S4_protect_m = ifelse(is.na(S4_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S4_Date & wash_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_protect_w = ifelse(is.na(S4_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S4_Date & wash_perday$PD_new > S4_Weekstart])))%>%
  mutate(S5_articles_m = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_articles_w = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Weekstart]))) %>%
  mutate(S5_protect_m = ifelse(is.na(S5_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S5_Date & wash_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_protect_w = ifelse(is.na(S5_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S5_Date & wash_perday$PD_new > S5_Weekstart])))%>%
  mutate(S6_articles_m = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_articles_w = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Weekstart]))) %>%
  mutate(S6_protect_m = ifelse(is.na(S6_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S6_Date & wash_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_protect_w = ifelse(is.na(S6_Date), as.integer(NA),sum(wash_perday$n[wash_perday$PD_new < S6_Date & wash_perday$PD_new > S6_Weekstart])))

wash <- wash %>%
  mutate(S1_pct_m = round(S1_protect_m/S1_articles_m,2))%>%
  mutate(S1_pct_w = round(S1_protect_w/S1_articles_w,2))%>%
  mutate(S2_pct_m = round(S2_protect_m/S2_articles_m,2))%>%
  mutate(S2_pct_w = round(S2_protect_w/S2_articles_w,2))%>%
  mutate(S3_pct_m = round(S3_protect_m/S3_articles_m,2))%>%
  mutate(S3_pct_w = round(S3_protect_w/S3_articles_w,2))%>%
  mutate(S4_pct_m = round(S4_protect_m/S4_articles_m,2))%>%
  mutate(S4_pct_w = round(S4_protect_w/S4_articles_w,2))%>%
  mutate(S5_pct_m = round(S5_protect_m/S5_articles_m,2))%>%
  mutate(S5_pct_w = round(S5_protect_w/S5_articles_w,2))%>%
  mutate(S6_pct_m = round(S6_protect_m/S6_articles_m,2))%>%
  mutate(S6_pct_w = round(S6_protect_w/S6_articles_w,2))

mlmreg <- wash

wash_household <- data.frame(
  ID = mlmreg$X.U.FEFF.PROLIFIC_PID,
  age = mlmreg$S1_Age,gender=mlmreg$S1_Gender,
  education = mlmreg$S1_Education,income = mlmreg$S1_Income,
  ethnicity = mlmreg$S1_Ethnicity,party = mlmreg$S1_Political_Party,
  political = mlmreg$S1_Political_Orientation,
  S1_news_consumption = mlmreg$S1_media,S2_news_consumption = mlmreg$S2_media,
  S3_news_consumption = mlmreg$S3_media,S4_news_consumption = mlmreg$S4_media,
  S5_news_consumption = mlmreg$S5_media,S6_news_consumption = mlmreg$S6_media, 
  S1_monthslope = mlmreg$monthslope1,S1_weekslope = mlmreg$weekslope1,
  S1_monthcoverage = mlmreg$S1_pct_m,S1_weekcoverage = mlmreg$S1_pct_w,
  S1_benefit_house = mlmreg$S1_Wash_Risks_low_household,S1_resources = mlmreg$S1_Wash_Resources,
  S1_benefit_community = mlmreg$S1_Wash_Risks_low_community,
  S2_monthslope = mlmreg$monthslope2,S2_weekslope = mlmreg$weekslope2,
  S2_monthcoverage = mlmreg$S2_pct_m,S2_weekcoverage = mlmreg$S2_pct_w,
  S2_benefit_house = mlmreg$S2_Wash_Risks_low_household,S2_resources = mlmreg$S2_Wash_Resources,
  S2_benefit_community = mlmreg$S2_Wash_Risks_low_community,
  S3_monthslope = mlmreg$monthslope3,S3_weekslope = mlmreg$weekslope3,
  S3_monthcoverage = mlmreg$S3_pct_m,S3_weekcoverage = mlmreg$S3_pct_w,
  S3_benefit_house = mlmreg$S3_Wash_Risks_low_household,S3_resources = mlmreg$S3_Wash_Resources,
  S3_benefit_community = mlmreg$S3_Wash_Risks_low_community,
  S4_monthslope = mlmreg$monthslope4,S4_weekslope = mlmreg$weekslope4,
  S4_monthcoverage = mlmreg$S4_pct_m,S4_weekcoverage = mlmreg$S4_pct_w,
  S4_benefit_house = mlmreg$S4_Wash_Risks_low_household,S4_resources = mlmreg$S4_Wash_Resources,
  S4_benefit_community = mlmreg$S4_Wash_Risks_low_community,
  S5_monthslope = mlmreg$monthslope5,S5_weekslope = mlmreg$weekslope5,
  S5_monthcoverage = mlmreg$S5_pct_m,S5_weekcoverage = mlmreg$S5_pct_w,
  S5_benefit_house = mlmreg$S5_Wash_Risks_low_household,S5_resources = mlmreg$S5_Wash_Resources,
  S5_benefit_community = mlmreg$S5_Wash_Risks_low_community,
  S6_monthslope = mlmreg$monthslope6,S6_weekslope = mlmreg$weekslope6,
  S6_monthcoverage = mlmreg$S6_pct_m,S6_weekcoverage = mlmreg$S6_pct_w,
  S6_benefit_house = mlmreg$S6_Wash_Risks_low_household,S6_resources = mlmreg$S6_Wash_Resources,
  S6_benefit_community = mlmreg$S6_Wash_Risks_low_community,
  S1_wash_behavior = mlmreg$S1_Wash_Household_action,
  S2_wash_behavior = mlmreg$S2_Wash_Household_action, 
  S3_wash_behavior = mlmreg$S3_Wash_Household_action,
  S4_wash_behavior = mlmreg$S4_Wash_Household_action,
  S5_wash_behavior = mlmreg$S5_Wash_Household_action,
  S6_wash_behavior = mlmreg$S6_Wash_Household_action)

#newsconsumption
news_consumption <-  wash_household%>%
  dplyr::select(ID,c(S1_news_consumption:S6_news_consumption))%>%
  gather(wave,news_consumption,-ID)
news_consumption$wave <- gsub("[^0-9.-]", "", news_consumption$wave)

#monthslope
monthslope <- wash_household%>%
  dplyr::select(ID,c(S1_monthslope,S2_monthslope,S3_monthslope,
              S4_monthslope,S5_monthslope,S6_monthslope))%>%
  gather(wave,monthslope,-ID)
monthslope$wave <- gsub("[^0-9.-]", "", monthslope$wave)

#weekslope
weekslope <- wash_household%>%
  dplyr::select(ID,c(S1_weekslope,S2_weekslope,S3_weekslope,
              S4_weekslope,S5_weekslope,S6_weekslope))%>%
  gather(wave,weekslope,-ID)
weekslope$wave <- gsub("[^0-9.-]", "", weekslope$wave)

#monthcoverage
monthcoverage <- wash_household%>%
  dplyr::select(ID,c(S1_monthcoverage,S2_monthcoverage,S3_monthcoverage,
              S4_monthcoverage,S5_monthcoverage,S6_monthcoverage))%>%
  gather(wave,monthcoverage,-ID)
monthcoverage$wave <- gsub("[^0-9.-]", "", monthcoverage$wave)

#weekcoverage
weekcoverage <- wash_household%>%
  dplyr::select(ID,c(S1_weekcoverage,S2_weekcoverage,S3_weekcoverage,
              S4_weekcoverage,S5_weekcoverage,S6_weekcoverage))%>%
  gather(wave,weekcoverage,-ID)
weekcoverage$wave <- gsub("[^0-9.-]", "", weekcoverage$wave)

#benefit_house
benefit_house <- wash_household%>%
  dplyr::select(ID,c(S1_benefit_house,S2_benefit_house,S3_benefit_house,
              S4_benefit_house,S5_benefit_house,S6_benefit_house))%>%
  gather(wave,benefit_house,-ID)
benefit_house$wave <- gsub("[^0-9.-]", "", benefit_house$wave)

#resources
resources <- wash_household%>%
  dplyr::select(ID,c(S1_resources,S2_resources,S3_resources,
              S4_resources,S5_resources,S6_resources))%>%
  gather(wave,resources,-ID)
resources$wave <- gsub("[^0-9.-]", "", resources$wave)

#benefit_community
benefit_community <- wash_household%>%
  dplyr::select(ID,c(S1_benefit_community,S2_benefit_community,S3_benefit_community,
              S4_benefit_community,S5_benefit_community,S6_benefit_community))%>%
  gather(wave,benefit_community,-ID)
benefit_community$wave <- gsub("[^0-9.-]", "", benefit_community$wave)

#wash_behavior
wash_behavior <- wash_household%>%
  dplyr::select(ID,c(S1_wash_behavior,S2_wash_behavior,S3_wash_behavior,
              S4_wash_behavior,S5_wash_behavior,S6_wash_behavior))%>%
  gather(wave,wash_behavior,-ID)
wash_behavior$wave <- gsub("[^0-9.-]", "", wash_behavior$wave)


mlmprep <- 
  left_join(news_consumption, monthslope, by = c("ID", "wave")) %>%
  left_join(weekslope, by = c("ID", "wave")) %>%
  left_join(monthcoverage, by = c("ID", "wave")) %>%
  left_join(weekcoverage,by = c("ID", "wave")) %>%
  left_join(benefit_house, by = c("ID", "wave")) %>%
  left_join(benefit_community, by = c("ID", "wave")) %>%
  left_join(resources, by = c("ID", "wave")) %>%
  left_join(wash_behavior, by = c("ID", "wave"))%>%
  left_join(dplyr::select(wash_household,c(ID:political)), by = "ID")

mlmprep$wave <- as.character(mlmprep$wave)
mlmprep$gender <- as.character(mlmprep$gender)
mlmprep$education <- as.character(mlmprep$education)
mlmprep$income <- as.character(mlmprep$income)
mlmprep$ethnicity <- as.character(mlmprep$ethnicity)

mlmprep$news_exposure_w = mlmprep$news_consumption*mlmprep$weekcoverage
mlmprep$news_exposure_m = mlmprep$news_consumption*mlmprep$monthcoverage

mlmprep <- mlmprep %>% 
  mutate(across(where(is.numeric), scale))

model <- '
    level:1
        wash_behavior ~ wave + resources + benefit_house + benefit_community + monthslope*weekslope
        resources ~ news_exposure_w*news_exposure_m
        benefit_house ~ news_exposure_w*news_exposure_m
        benefit_community ~ news_exposure_w*news_exposure_m
    level:2
        wash_behavior ~ age + gender + education + income + ethnicity + party + political
'

model <- '
    level:1
        wash_behavior ~ wave + resources + benefit_house + benefit_community + weekslope
        benefit_house ~ news_exposure_w
        benefit_community ~ news_exposure_w
    level:2
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
'


wash_fit <- sem(model = model, data = mlmprep, 
                     cluster = "ID",
                     verbose = TRUE, optim.method = "em", em.iter.max = 20000,
                     em.fx.tol = 1e-02, em.dx.tol = 1e-04)

summary(wash_fit,fit.measures = TRUE)

###### Mask ######

mask <- trendy %>%
  rowwise()%>%
  mutate(S1_articles_m = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_articles_w = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Weekstart]))) %>%
  mutate(S1_protect_m = ifelse(is.na(S1_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S1_Date & mask_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_protect_w = ifelse(is.na(S1_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S1_Date & mask_perday$PD_new > S1_Weekstart])))%>%
  mutate(S2_articles_m = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_articles_w = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Weekstart]))) %>%
  mutate(S2_protect_m = ifelse(is.na(S2_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S2_Date & mask_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_protect_w = ifelse(is.na(S2_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S2_Date & mask_perday$PD_new > S2_Weekstart])))%>%
  mutate(S3_articles_m = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_articles_w = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Weekstart]))) %>%
  mutate(S3_protect_m = ifelse(is.na(S3_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S3_Date & mask_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_protect_w = ifelse(is.na(S3_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S3_Date & mask_perday$PD_new > S3_Weekstart])))%>%
  mutate(S4_articles_m = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_articles_w = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Weekstart]))) %>%
  mutate(S4_protect_m = ifelse(is.na(S4_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S4_Date & mask_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_protect_w = ifelse(is.na(S4_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S4_Date & mask_perday$PD_new > S4_Weekstart])))%>%
  mutate(S5_articles_m = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_articles_w = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Weekstart]))) %>%
  mutate(S5_protect_m = ifelse(is.na(S5_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S5_Date & mask_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_protect_w = ifelse(is.na(S5_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S5_Date & mask_perday$PD_new > S5_Weekstart])))%>%
  mutate(S6_articles_m = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_articles_w = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Weekstart]))) %>%
  mutate(S6_protect_m = ifelse(is.na(S6_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S6_Date & mask_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_protect_w = ifelse(is.na(S6_Date), as.integer(NA),sum(mask_perday$n[mask_perday$PD_new < S6_Date & mask_perday$PD_new > S6_Weekstart])))

mask <- mask %>%
  mutate(S1_pct_m = round(S1_protect_m/S1_articles_m,2))%>%
  mutate(S1_pct_w = round(S1_protect_w/S1_articles_w,2))%>%
  mutate(S2_pct_m = round(S2_protect_m/S2_articles_m,2))%>%
  mutate(S2_pct_w = round(S2_protect_w/S2_articles_w,2))%>%
  mutate(S3_pct_m = round(S3_protect_m/S3_articles_m,2))%>%
  mutate(S3_pct_w = round(S3_protect_w/S3_articles_w,2))%>%
  mutate(S4_pct_m = round(S4_protect_m/S4_articles_m,2))%>%
  mutate(S4_pct_w = round(S4_protect_w/S4_articles_w,2))%>%
  mutate(S5_pct_m = round(S5_protect_m/S5_articles_m,2))%>%
  mutate(S5_pct_w = round(S5_protect_w/S5_articles_w,2))%>%
  mutate(S6_pct_m = round(S6_protect_m/S6_articles_m,2))%>%
  mutate(S6_pct_w = round(S6_protect_w/S6_articles_w,2))

mlmreg <- mask

mask_household <- data.frame(
  ID = mlmreg$X.U.FEFF.PROLIFIC_PID,
  age = mlmreg$S1_Age,gender=mlmreg$S1_Gender,
  education = mlmreg$S1_Education,income = mlmreg$S1_Income,
  ethnicity = mlmreg$S1_Ethnicity,party = mlmreg$S1_Political_Party,
  political = mlmreg$S1_Political_Orientation,
  S1_news_consumption = mlmreg$S1_media,S2_news_consumption = mlmreg$S2_media,
  S3_news_consumption = mlmreg$S3_media,S4_news_consumption = mlmreg$S4_media,
  S5_news_consumption = mlmreg$S5_media,S6_news_consumption = mlmreg$S6_media, 
  S1_monthslope = mlmreg$monthslope1,S1_weekslope = mlmreg$weekslope1,
  S1_monthcoverage = mlmreg$S1_pct_m,S1_weekcoverage = mlmreg$S1_pct_w,
  S1_benefit_house = mlmreg$S1_Mask_Risks_low_household,S1_resources = mlmreg$S1_Mask_Resources,
  S1_benefit_community = mlmreg$S1_Mask_Risks_low_community,
  S2_monthslope = mlmreg$monthslope2,S2_weekslope = mlmreg$weekslope2,
  S2_monthcoverage = mlmreg$S2_pct_m,S2_weekcoverage = mlmreg$S2_pct_w,
  S2_benefit_house = mlmreg$S2_Mask_Risks_low_household,S2_resources = mlmreg$S2_Mask_Resources,
  S2_benefit_community = mlmreg$S2_Mask_Risks_low_community,
  S3_monthslope = mlmreg$monthslope3,S3_weekslope = mlmreg$weekslope3,
  S3_monthcoverage = mlmreg$S3_pct_m,S3_weekcoverage = mlmreg$S3_pct_w,
  S3_benefit_house = mlmreg$S3_Mask_Risks_low_household,S3_resources = mlmreg$S3_Mask_Resources,
  S3_benefit_community = mlmreg$S3_Mask_Risks_low_community,
  S4_monthslope = mlmreg$monthslope4,S4_weekslope = mlmreg$weekslope4,
  S4_monthcoverage = mlmreg$S4_pct_m,S4_weekcoverage = mlmreg$S4_pct_w,
  S4_benefit_house = mlmreg$S4_Mask_Risks_low_household,S4_resources = mlmreg$S4_Mask_Resources,
  S4_benefit_community = mlmreg$S4_Mask_Risks_low_community,
  S5_monthslope = mlmreg$monthslope5,S5_weekslope = mlmreg$weekslope5,
  S5_monthcoverage = mlmreg$S5_pct_m,S5_weekcoverage = mlmreg$S5_pct_w,
  S5_benefit_house = mlmreg$S5_Mask_Risks_low_household,S5_resources = mlmreg$S5_Mask_Resources,
  S5_benefit_community = mlmreg$S5_Mask_Risks_low_community,
  S6_monthslope = mlmreg$monthslope6,S6_weekslope = mlmreg$weekslope6,
  S6_monthcoverage = mlmreg$S6_pct_m,S6_weekcoverage = mlmreg$S6_pct_w,
  S6_benefit_house = mlmreg$S6_Mask_Risks_low_household,S6_resources = mlmreg$S6_Mask_Resources,
  S6_benefit_community = mlmreg$S6_Mask_Risks_low_community,
  S1_mask_behavior = mlmreg$S1_Masks_Household_action,
  S2_mask_behavior = mlmreg$S2_Masks_Household_action, 
  S3_mask_behavior = mlmreg$S3_Masks_Household_action,
  S4_mask_behavior = mlmreg$S4_Mask_Household_action,
  S5_mask_behavior = mlmreg$S5_Mask_Household_action,
  S6_mask_behavior = mlmreg$S6_Mask_Household_action)

#newsconsumption
news_consumption <-  mask_household%>%
  dplyr::select(ID,c(S1_news_consumption:S6_news_consumption))%>%
  gather(wave,news_consumption,-ID)
news_consumption$wave <- gsub("[^0-9.-]", "", news_consumption$wave)

#monthslope
monthslope <- mask_household%>%
  dplyr::select(ID,c(S1_monthslope,S2_monthslope,S3_monthslope,
              S4_monthslope,S5_monthslope,S6_monthslope))%>%
  gather(wave,monthslope,-ID)
monthslope$wave <- gsub("[^0-9.-]", "", monthslope$wave)

#weekslope
weekslope <- mask_household%>%
  dplyr::select(ID,c(S1_weekslope,S2_weekslope,S3_weekslope,
              S4_weekslope,S5_weekslope,S6_weekslope))%>%
  gather(wave,weekslope,-ID)
weekslope$wave <- gsub("[^0-9.-]", "", weekslope$wave)

#monthcoverage
monthcoverage <- mask_household%>%
  dplyr::select(ID,c(S1_monthcoverage,S2_monthcoverage,S3_monthcoverage,
              S4_monthcoverage,S5_monthcoverage,S6_monthcoverage))%>%
  gather(wave,monthcoverage,-ID)
monthcoverage$wave <- gsub("[^0-9.-]", "", monthcoverage$wave)

#weekcoverage
weekcoverage <- mask_household%>%
  dplyr::select(ID,c(S1_weekcoverage,S2_weekcoverage,S3_weekcoverage,
              S4_weekcoverage,S5_weekcoverage,S6_weekcoverage))%>%
  gather(wave,weekcoverage,-ID)
weekcoverage$wave <- gsub("[^0-9.-]", "", weekcoverage$wave)

#benefit_house
benefit_house <- mask_household%>%
  dplyr::select(ID,c(S1_benefit_house,S2_benefit_house,S3_benefit_house,
              S4_benefit_house,S5_benefit_house,S6_benefit_house))%>%
  gather(wave,benefit_house,-ID)
benefit_house$wave <- gsub("[^0-9.-]", "", benefit_house$wave)

#resources
resources <- mask_household%>%
  dplyr::select(ID,c(S1_resources,S2_resources,S3_resources,
              S4_resources,S5_resources,S6_resources))%>%
  gather(wave,resources,-ID)
resources$wave <- gsub("[^0-9.-]", "", resources$wave)

#benefit_community
benefit_community <- mask_household%>%
  dplyr::select(ID,c(S1_benefit_community,S2_benefit_community,S3_benefit_community,
              S4_benefit_community,S5_benefit_community,S6_benefit_community))%>%
  gather(wave,benefit_community,-ID)
benefit_community$wave <- gsub("[^0-9.-]", "", benefit_community$wave)

#mask_behavior
mask_behavior <- mask_household%>%
  dplyr::select(ID,c(S1_mask_behavior,S2_mask_behavior,S3_mask_behavior,
              S4_mask_behavior,S5_mask_behavior,S6_mask_behavior))%>%
  gather(wave,mask_behavior,-ID)
mask_behavior$wave <- gsub("[^0-9.-]", "", mask_behavior$wave)


mlmprep <- 
  left_join(news_consumption, monthslope, by = c("ID", "wave")) %>%
  left_join(weekslope, by = c("ID", "wave")) %>%
  left_join(monthcoverage, by = c("ID", "wave")) %>%
  left_join(weekcoverage,by = c("ID", "wave")) %>%
  left_join(benefit_house, by = c("ID", "wave")) %>%
  left_join(benefit_community, by = c("ID", "wave")) %>%
  left_join(resources, by = c("ID", "wave")) %>%
  left_join(mask_behavior, by = c("ID", "wave"))%>%
  left_join(dplyr::select(mask_household,c(ID:political)), by = "ID")

mlmprep$wave <- as.character(mlmprep$wave)
mlmprep$gender <- as.character(mlmprep$gender)
mlmprep$education <- as.character(mlmprep$education)
mlmprep$income <- as.character(mlmprep$income)
mlmprep$ethnicity <- as.character(mlmprep$ethnicity)

mlmprep$news_exposure_w = mlmprep$news_consumption*mlmprep$weekcoverage
mlmprep$news_exposure_m = mlmprep$news_consumption*mlmprep$monthcoverage

mlmprep <- mlmprep %>% 
  mutate(across(where(is.numeric), scale))

model <- '
    level:1
        mask_behavior ~ wave + resources + benefit_house + benefit_community + monthslope*weekslope
        resources ~ news_exposure_w*news_exposure_m
        benefit_house ~ news_exposure_w*news_exposure_m
        benefit_community ~ news_exposure_w*news_exposure_m
    level:2
        mask_behavior ~ age + gender + education + income + ethnicity + party + political
'


model <- '
    level:1
        mask_behavior ~ wave + resources + benefit_house + benefit_community + weekslope
        benefit_house ~ news_exposure_w
        benefit_community ~ news_exposure_w
    level:2
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
'


mask_fit <- sem(model = model, data = mlmprep, 
                cluster = "ID",
                verbose = TRUE, optim.method = "em", em.iter.max = 20000,
                em.fx.tol = 1e-02, em.dx.tol = 1e-04)

summary(mask_fit,fit.measures = TRUE)


###### Quarantine ######

quarantine <- trendy %>%
  rowwise()%>%
  mutate(S1_articles_m = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_articles_w = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Weekstart]))) %>%
  mutate(S1_protect_m = ifelse(is.na(S1_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S1_Date & quarantine_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_protect_w = ifelse(is.na(S1_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S1_Date & quarantine_perday$PD_new > S1_Weekstart])))%>%
  mutate(S2_articles_m = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_articles_w = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Weekstart]))) %>%
  mutate(S2_protect_m = ifelse(is.na(S2_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S2_Date & quarantine_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_protect_w = ifelse(is.na(S2_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S2_Date & quarantine_perday$PD_new > S2_Weekstart])))%>%
  mutate(S3_articles_m = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_articles_w = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Weekstart]))) %>%
  mutate(S3_protect_m = ifelse(is.na(S3_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S3_Date & quarantine_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_protect_w = ifelse(is.na(S3_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S3_Date & quarantine_perday$PD_new > S3_Weekstart])))%>%
  mutate(S4_articles_m = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_articles_w = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Weekstart]))) %>%
  mutate(S4_protect_m = ifelse(is.na(S4_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S4_Date & quarantine_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_protect_w = ifelse(is.na(S4_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S4_Date & quarantine_perday$PD_new > S4_Weekstart])))%>%
  mutate(S5_articles_m = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_articles_w = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Weekstart]))) %>%
  mutate(S5_protect_m = ifelse(is.na(S5_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S5_Date & quarantine_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_protect_w = ifelse(is.na(S5_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S5_Date & quarantine_perday$PD_new > S5_Weekstart])))%>%
  mutate(S6_articles_m = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_articles_w = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Weekstart]))) %>%
  mutate(S6_protect_m = ifelse(is.na(S6_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S6_Date & quarantine_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_protect_w = ifelse(is.na(S6_Date), as.integer(NA),sum(quarantine_perday$n[quarantine_perday$PD_new < S6_Date & quarantine_perday$PD_new > S6_Weekstart])))

quarantine <- quarantine %>%
  mutate(S1_pct_m = round(S1_protect_m/S1_articles_m,2))%>%
  mutate(S1_pct_w = round(S1_protect_w/S1_articles_w,2))%>%
  mutate(S2_pct_m = round(S2_protect_m/S2_articles_m,2))%>%
  mutate(S2_pct_w = round(S2_protect_w/S2_articles_w,2))%>%
  mutate(S3_pct_m = round(S3_protect_m/S3_articles_m,2))%>%
  mutate(S3_pct_w = round(S3_protect_w/S3_articles_w,2))%>%
  mutate(S4_pct_m = round(S4_protect_m/S4_articles_m,2))%>%
  mutate(S4_pct_w = round(S4_protect_w/S4_articles_w,2))%>%
  mutate(S5_pct_m = round(S5_protect_m/S5_articles_m,2))%>%
  mutate(S5_pct_w = round(S5_protect_w/S5_articles_w,2))%>%
  mutate(S6_pct_m = round(S6_protect_m/S6_articles_m,2))%>%
  mutate(S6_pct_w = round(S6_protect_w/S6_articles_w,2))

mlmreg <- quarantine

quarantine_household <- data.frame(
  ID = mlmreg$X.U.FEFF.PROLIFIC_PID,
  age = mlmreg$S1_Age,gender=mlmreg$S1_Gender,
  education = mlmreg$S1_Education,income = mlmreg$S1_Income,
  ethnicity = mlmreg$S1_Ethnicity,party = mlmreg$S1_Political_Party,
  political = mlmreg$S1_Political_Orientation,
  S1_news_consumption = mlmreg$S1_media,S2_news_consumption = mlmreg$S2_media,
  S3_news_consumption = mlmreg$S3_media,S4_news_consumption = mlmreg$S4_media,
  S5_news_consumption = mlmreg$S5_media,S6_news_consumption = mlmreg$S6_media, 
  S2_monthslope = mlmreg$monthslope2,S2_weekslope = mlmreg$weekslope2,
  S2_monthcoverage = mlmreg$S2_pct_m,S2_weekcoverage = mlmreg$S2_pct_w,
  S2_benefit_house = mlmreg$S2_Quarantine_Risks_low_household,S2_resources = mlmreg$S2_Quarantine_Resources,
  S2_benefit_community = mlmreg$S2_Quarantine_Risks_low_community,
  S3_monthslope = mlmreg$monthslope3,S3_weekslope = mlmreg$weekslope3,
  S3_monthcoverage = mlmreg$S3_pct_m,S3_weekcoverage = mlmreg$S3_pct_w,
  S3_benefit_house = mlmreg$S3_Quarantine_Risks_low_household,S3_resources = mlmreg$S3_Quarantine_Resources,
  S3_benefit_community = mlmreg$S3_Quarantine_Risks_low_community,
  S4_monthslope = mlmreg$monthslope4,S4_weekslope = mlmreg$weekslope4,
  S4_monthcoverage = mlmreg$S4_pct_m,S4_weekcoverage = mlmreg$S4_pct_w,
  S4_benefit_house = mlmreg$S4_Quarantine_Risks_low_household,S4_resources = mlmreg$S4_Quarantine_Resources,
  S4_benefit_community = mlmreg$S4_Quarantine_Risks_low_community,
  S5_monthslope = mlmreg$monthslope5,S5_weekslope = mlmreg$weekslope5,
  S5_monthcoverage = mlmreg$S5_pct_m,S5_weekcoverage = mlmreg$S5_pct_w,
  S5_benefit_house = mlmreg$S5_Quarantine_Risks_low_household,S5_resources = mlmreg$S5_Quarantine_Resources,
  S5_benefit_community = mlmreg$S5_Quarantine_Risks_low_community,
  S6_monthslope = mlmreg$monthslope6,S6_weekslope = mlmreg$weekslope6,
  S6_monthcoverage = mlmreg$S6_pct_m,S6_weekcoverage = mlmreg$S6_pct_w,
  S6_benefit_house = mlmreg$S6_Quarantine_Risks_low_household,S6_resources = mlmreg$S6_Quarantine_Resources,
  S6_benefit_community = mlmreg$S6_Quarantine_Risks_low_community,
  S2_quarantine_behavior = mlmreg$S2_Quarantine_Household_action, 
  S3_quarantine_behavior = mlmreg$S3_Quarantine_Household_action,
  S4_quarantine_behavior = mlmreg$S4_Quarantine_Household_action,
  S5_quarantine_behavior = mlmreg$S5_Quarantine_Household_action,
  S6_quarantine_behavior = mlmreg$S6_Quarantine_Household_action)

#newsconsumption
news_consumption <-  quarantine_household%>%
  dplyr::select(ID,c(S2_news_consumption:S6_news_consumption))%>%
  gather(wave,news_consumption,-ID)
news_consumption$wave <- gsub("[^0-9.-]", "", news_consumption$wave)

#monthslope
monthslope <- quarantine_household%>%
  dplyr::select(ID,c(S2_monthslope,S3_monthslope,
              S4_monthslope,S5_monthslope,S6_monthslope))%>%
  gather(wave,monthslope,-ID)
monthslope$wave <- gsub("[^0-9.-]", "", monthslope$wave)

#weekslope
weekslope <- quarantine_household%>%
  dplyr::select(ID,c(S2_weekslope,S3_weekslope,
              S4_weekslope,S5_weekslope,S6_weekslope))%>%
  gather(wave,weekslope,-ID)
weekslope$wave <- gsub("[^0-9.-]", "", weekslope$wave)

#monthcoverage
monthcoverage <- quarantine_household%>%
  dplyr::select(ID,c(S2_monthcoverage,S3_monthcoverage,
              S4_monthcoverage,S5_monthcoverage,S6_monthcoverage))%>%
  gather(wave,monthcoverage,-ID)
monthcoverage$wave <- gsub("[^0-9.-]", "", monthcoverage$wave)

#weekcoverage
weekcoverage <- quarantine_household%>%
  dplyr::select(ID,c(S2_weekcoverage,S3_weekcoverage,
              S4_weekcoverage,S5_weekcoverage,S6_weekcoverage))%>%
  gather(wave,weekcoverage,-ID)
weekcoverage$wave <- gsub("[^0-9.-]", "", weekcoverage$wave)

#benefit_house
benefit_house <- quarantine_household%>%
  dplyr::select(ID,c(S2_benefit_house,S3_benefit_house,
              S4_benefit_house,S5_benefit_house,S6_benefit_house))%>%
  gather(wave,benefit_house,-ID)
benefit_house$wave <- gsub("[^0-9.-]", "", benefit_house$wave)

#resources
resources <- quarantine_household%>%
  dplyr::select(ID,c(S2_resources,S3_resources,
              S4_resources,S5_resources,S6_resources))%>%
  gather(wave,resources,-ID)
resources$wave <- gsub("[^0-9.-]", "", resources$wave)

#benefit_community
benefit_community <- quarantine_household%>%
  dplyr::select(ID,c(S2_benefit_community,S3_benefit_community,
              S4_benefit_community,S5_benefit_community,S6_benefit_community))%>%
  gather(wave,benefit_community,-ID)
benefit_community$wave <- gsub("[^0-9.-]", "", benefit_community$wave)

#quarantine_behavior
quarantine_behavior <- quarantine_household%>%
  dplyr::select(ID,c(S2_quarantine_behavior,S3_quarantine_behavior,
              S4_quarantine_behavior,S5_quarantine_behavior,S6_quarantine_behavior))%>%
  gather(wave,quarantine_behavior,-ID)
quarantine_behavior$wave <- gsub("[^0-9.-]", "", quarantine_behavior$wave)


mlmprep <- 
  left_join(news_consumption, monthslope, by = c("ID", "wave")) %>%
  left_join(weekslope, by = c("ID", "wave")) %>%
  left_join(monthcoverage, by = c("ID", "wave")) %>%
  left_join(weekcoverage,by = c("ID", "wave")) %>%
  left_join(benefit_house, by = c("ID", "wave")) %>%
  left_join(benefit_community, by = c("ID", "wave")) %>%
  left_join(resources, by = c("ID", "wave")) %>%
  left_join(quarantine_behavior, by = c("ID", "wave"))%>%
  left_join(dplyr::select(quarantine_household,c(ID:political)), by = "ID")

mlmprep$wave <- as.character(mlmprep$wave)
mlmprep$gender <- as.character(mlmprep$gender)
mlmprep$education <- as.character(mlmprep$education)
mlmprep$income <- as.character(mlmprep$income)
mlmprep$ethnicity <- as.character(mlmprep$ethnicity)

mlmprep$news_exposure_w = mlmprep$news_consumption*mlmprep$weekcoverage
mlmprep$news_exposure_m = mlmprep$news_consumption*mlmprep$monthcoverage

mlmprep <- mlmprep %>% 
  mutate(across(where(is.numeric), scale))

model <- '
    level:1
        quarantine_behavior ~ wave + resources + benefit_house + benefit_community + monthslope*weekslope
        resources ~ news_exposure_w*news_exposure_m
        benefit_house ~ news_exposure_w*news_exposure_m
        benefit_community ~ news_exposure_w*news_exposure_m
    level:2
        quarantine_behavior ~ age + gender + education + income + ethnicity + party + political
'



model <- '
    level:1
        quarantine_behavior ~ wave + resources + benefit_house + benefit_community + weekslope
        benefit_house ~ news_exposure_w
        benefit_community ~ news_exposure_w
    level:2
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
'


quarantine_fit <- sem(model = model, data = mlmprep, 
                cluster = "ID",
                verbose = TRUE, optim.method = "em", em.iter.max = 20000,
                em.fx.tol = 1e-02, em.dx.tol = 1e-04)

summary(quarantine_fit,fit.measures = TRUE)


###### Avoid ######

avoidall <- trendy %>%
  rowwise()%>%
  mutate(S1_articles_m = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_articles_w = ifelse(is.na(S1_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S1_Date & articles_perday$PD_new > S1_Weekstart]))) %>%
  mutate(S1_protect_m = ifelse(is.na(S1_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S1_Date & avoidall_perday$PD_new > S1_Monthstart]))) %>%
  mutate(S1_protect_w = ifelse(is.na(S1_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S1_Date & avoidall_perday$PD_new > S1_Weekstart])))%>%
  mutate(S2_articles_m = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_articles_w = ifelse(is.na(S2_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S2_Date & articles_perday$PD_new > S2_Weekstart]))) %>%
  mutate(S2_protect_m = ifelse(is.na(S2_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S2_Date & avoidall_perday$PD_new > S2_Monthstart]))) %>%
  mutate(S2_protect_w = ifelse(is.na(S2_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S2_Date & avoidall_perday$PD_new > S2_Weekstart])))%>%
  mutate(S3_articles_m = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_articles_w = ifelse(is.na(S3_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S3_Date & articles_perday$PD_new > S3_Weekstart]))) %>%
  mutate(S3_protect_m = ifelse(is.na(S3_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S3_Date & avoidall_perday$PD_new > S3_Monthstart]))) %>%
  mutate(S3_protect_w = ifelse(is.na(S3_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S3_Date & avoidall_perday$PD_new > S3_Weekstart])))%>%
  mutate(S4_articles_m = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_articles_w = ifelse(is.na(S4_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S4_Date & articles_perday$PD_new > S4_Weekstart]))) %>%
  mutate(S4_protect_m = ifelse(is.na(S4_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S4_Date & avoidall_perday$PD_new > S4_Monthstart]))) %>%
  mutate(S4_protect_w = ifelse(is.na(S4_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S4_Date & avoidall_perday$PD_new > S4_Weekstart])))%>%
  mutate(S5_articles_m = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_articles_w = ifelse(is.na(S5_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S5_Date & articles_perday$PD_new > S5_Weekstart]))) %>%
  mutate(S5_protect_m = ifelse(is.na(S5_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S5_Date & avoidall_perday$PD_new > S5_Monthstart]))) %>%
  mutate(S5_protect_w = ifelse(is.na(S5_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S5_Date & avoidall_perday$PD_new > S5_Weekstart])))%>%
  mutate(S6_articles_m = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_articles_w = ifelse(is.na(S6_Date), as.integer(NA),sum(articles_perday$n[articles_perday$PD_new < S6_Date & articles_perday$PD_new > S6_Weekstart]))) %>%
  mutate(S6_protect_m = ifelse(is.na(S6_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S6_Date & avoidall_perday$PD_new > S6_Monthstart]))) %>%
  mutate(S6_protect_w = ifelse(is.na(S6_Date), as.integer(NA),sum(avoidall_perday$n[avoidall_perday$PD_new < S6_Date & avoidall_perday$PD_new > S6_Weekstart])))

avoidall <- avoidall %>%
  mutate(S1_pct_m = round(S1_protect_m/S1_articles_m,2))%>%
  mutate(S1_pct_w = round(S1_protect_w/S1_articles_w,2))%>%
  mutate(S2_pct_m = round(S2_protect_m/S2_articles_m,2))%>%
  mutate(S2_pct_w = round(S2_protect_w/S2_articles_w,2))%>%
  mutate(S3_pct_m = round(S3_protect_m/S3_articles_m,2))%>%
  mutate(S3_pct_w = round(S3_protect_w/S3_articles_w,2))%>%
  mutate(S4_pct_m = round(S4_protect_m/S4_articles_m,2))%>%
  mutate(S4_pct_w = round(S4_protect_w/S4_articles_w,2))%>%
  mutate(S5_pct_m = round(S5_protect_m/S5_articles_m,2))%>%
  mutate(S5_pct_w = round(S5_protect_w/S5_articles_w,2))%>%
  mutate(S6_pct_m = round(S6_protect_m/S6_articles_m,2))%>%
  mutate(S6_pct_w = round(S6_protect_w/S6_articles_w,2))

mlmreg <- avoidall

avoidall_household <- data.frame(
  ID = mlmreg$X.U.FEFF.PROLIFIC_PID,
  age = mlmreg$S1_Age,gender=mlmreg$S1_Gender,
  education = mlmreg$S1_Education,income = mlmreg$S1_Income,
  ethnicity = mlmreg$S1_Ethnicity,party = mlmreg$S1_Political_Party,
  political = mlmreg$S1_Political_Orientation,
  S1_news_consumption = mlmreg$S1_media,S2_news_consumption = mlmreg$S2_media,
  S3_news_consumption = mlmreg$S3_media,S4_news_consumption = mlmreg$S4_media,
  S5_news_consumption = mlmreg$S5_media,S6_news_consumption = mlmreg$S6_media, 
  S1_monthslope = mlmreg$monthslope1,S1_weekslope = mlmreg$weekslope1,
  S1_monthcoverage = mlmreg$S1_pct_m,S1_weekcoverage = mlmreg$S1_pct_w,
  S1_benefit_house = mean(mlmreg$S1_Avoid_public_Risks_low_household,
                          mlmreg$S1_Avoid_people_Risks_low_household,
                          mlmreg$S1_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S1_Avoid_public_Resources=mlmreg$S1_Avoid_public_Resources,
  S1_Avoid_people_Resources=mlmreg$S1_Avoid_people_Resources,
  S1_Avoid_travel_Resources= mlmreg$S1_Avoid_travel_Resources,
  S1_benefit_community = mean(mlmreg$S1_Avoid_public_Risks_low_community,
                              mlmreg$S1_Avoid_people_Risks_low_community,
                              mlmreg$S1_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S2_monthslope = mlmreg$monthslope2,S2_weekslope = mlmreg$weekslope2,
  S2_monthcoverage = mlmreg$S2_pct_m,S2_weekcoverage = mlmreg$S2_pct_w,
  S2_benefit_house = mean(mlmreg$S2_Avoid_public_Risks_low_household,
                          mlmreg$S2_Avoid_people_Risks_low_household,
                          mlmreg$S2_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S2_Avoid_public_Resources=mlmreg$S2_Avoid_public_Resources,
  S2_Avoid_people_Resources=mlmreg$S2_Avoid_people_Resources,
  S2_Avoid_travel_Resources= mlmreg$S2_Avoid_travel_Resources,
  S2_benefit_community = mean(mlmreg$S2_Avoid_public_Risks_low_community,
                              mlmreg$S2_Avoid_people_Risks_low_community,
                              mlmreg$S2_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S3_monthslope = mlmreg$monthslope3,S3_weekslope = mlmreg$weekslope3,
  S3_monthcoverage = mlmreg$S3_pct_m,S3_weekcoverage = mlmreg$S3_pct_w,
  S3_benefit_house = mean(mlmreg$S3_Avoid_public_Risks_low_household,
                          mlmreg$S3_Avoid_people_Risks_low_household,
                          mlmreg$S3_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S3_Avoid_public_Resources=mlmreg$S3_Avoid_public_Resources,
  S3_Avoid_people_Resources=mlmreg$S3_Avoid_people_Resources,
  S3_Avoid_travel_Resources= mlmreg$S3_Avoid_travel_Resources,
  S3_benefit_community = mean(mlmreg$S3_Avoid_public_Risks_low_community,
                              mlmreg$S3_Avoid_people_Risks_low_community,
                              mlmreg$S3_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S4_monthslope = mlmreg$monthslope4,S4_weekslope = mlmreg$weekslope4,
  S4_monthcoverage = mlmreg$S4_pct_m,S4_weekcoverage = mlmreg$S4_pct_w,
  S4_benefit_house = mean(mlmreg$S4_Avoid_public_Risks_low_household,
                          mlmreg$S4_Avoid_people_Risks_low_household,
                          mlmreg$S4_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S4_Avoid_public_Resources=mlmreg$S4_Avoid_public_Resources,
  S4_Avoid_people_Resources=mlmreg$S4_Avoid_people_Resources,
  S4_Avoid_travel_Resources= mlmreg$S4_Avoid_travel_Resources,
  S4_benefit_community = mean(mlmreg$S4_Avoid_public_Risks_low_community,
                              mlmreg$S4_Avoid_people_Risks_low_community,
                              mlmreg$S4_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S5_monthslope = mlmreg$monthslope5,S5_weekslope = mlmreg$weekslope5,
  S5_monthcoverage = mlmreg$S5_pct_m,S5_weekcoverage = mlmreg$S5_pct_w,
  S5_benefit_house = mean(mlmreg$S5_Avoid_public_Risks_low_household,
                          mlmreg$S5_Avoid_people_Risks_low_household,
                          mlmreg$S5_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S5_Avoid_public_Resources=mlmreg$S5_Avoid_public_Resources,
  S5_Avoid_people_Resources=mlmreg$S5_Avoid_people_Resources,
  S5_Avoid_travel_Resources= mlmreg$S5_Avoid_travel_Resources,
  S5_benefit_community = mean(mlmreg$S5_Avoid_public_Risks_low_community,
                              mlmreg$S5_Avoid_people_Risks_low_community,
                              mlmreg$S5_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S6_monthslope = mlmreg$monthslope6,S6_weekslope = mlmreg$weekslope6,
  S6_monthcoverage = mlmreg$S6_pct_m,S6_weekcoverage = mlmreg$S6_pct_w,
  S6_benefit_house = mean(mlmreg$S6_Avoid_public_Risks_low_household,
                          mlmreg$S6_Avoid_people_Risks_low_household,
                          mlmreg$S6_Avoid_travel_Risks_low_household, na.rm=TRUE),
  S6_Avoid_public_Resources=mlmreg$S6_Avoid_public_Resources,
  S6_Avoid_people_Resources=mlmreg$S6_Avoid_people_Resources,
  S6_Avoid_travel_Resources= mlmreg$S6_Avoid_travel_Resources,
  S6_benefit_community = mean(mlmreg$S6_Avoid_public_Risks_low_community,
                              mlmreg$S6_Avoid_people_Risks_low_community,
                              mlmreg$S6_Avoid_travel_Risks_low_community, na.rm=TRUE),
  S1_avoidall_behavior = mean(mlmreg$S1_Avoid_public_Household_action,
                              mlmreg$S1_Avoid_people_Household_action,
                              mlmreg$S1_Avoid_travel_Household_action, na.rm=TRUE),
  S2_avoidall_behavior = mean(mlmreg$S2_Avoid_public_Household_action,
                              mlmreg$S2_Avoid_people_Household_action,
                              mlmreg$S2_Avoid_travel_Household_action, na.rm=TRUE), 
  S3_avoidall_behavior = mean(mlmreg$S3_Avoid_public_Household_action,
                              mlmreg$S3_Avoid_people_Household_action,
                              mlmreg$S3_Avoid_travel_Household_action, na.rm=TRUE),
  S4_avoidall_behavior = mean(mlmreg$S4_Avoid_public_Household_action,
                              mlmreg$S4_Avoid_people_Household_action,
                              mlmreg$S4_Avoid_travel_Household_action, na.rm=TRUE),
  S5_avoidall_behavior = mean(mlmreg$S5_Avoid_public_Household_action,
                              mlmreg$S5_Avoid_people_Household_action,
                              mlmreg$S5_Avoid_travel_Household_action, na.rm=TRUE),
  S6_avoidall_behavior = mean(mlmreg$S6_Avoid_public_Household_action,
                              mlmreg$S6_Avoid_people_Household_action,
                              mlmreg$S6_Avoid_travel_Household_action, na.rm=TRUE))

avoidall_household$S1_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S1_Avoid_public_Resources,
                                                             S1_Avoid_people_Resources,
                                                             S1_Avoid_travel_Resources)),na.rm = T)
avoidall_household$S2_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S2_Avoid_public_Resources,
                                                             S2_Avoid_people_Resources,
                                                             S2_Avoid_travel_Resources)),na.rm = T)
avoidall_household$S3_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S3_Avoid_public_Resources,
                                                             S3_Avoid_people_Resources,
                                                             S3_Avoid_travel_Resources)),na.rm = T)
avoidall_household$S4_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S4_Avoid_public_Resources,
                                                             S4_Avoid_people_Resources,
                                                             S4_Avoid_travel_Resources)),na.rm = T)
avoidall_household$S5_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S5_Avoid_public_Resources,
                                                             S5_Avoid_people_Resources,
                                                             S5_Avoid_travel_Resources)),na.rm = T)
avoidall_household$S6_resources = rowMeans(subset(avoidall_household, 
                                                  dplyr::select = c(S6_Avoid_public_Resources,
                                                             S6_Avoid_people_Resources,
                                                             S6_Avoid_travel_Resources)),na.rm = T)
#newsconsumption
news_consumption <-  avoidall_household%>%
  dplyr::select(ID,c(S1_news_consumption:S6_news_consumption))%>%
  gather(wave,news_consumption,-ID)
news_consumption$wave <- gsub("[^0-9.-]", "", news_consumption$wave)

#monthslope
monthslope <- avoidall_household%>%
  dplyr::select(ID,c(S1_monthslope,S2_monthslope,S3_monthslope,
              S4_monthslope,S5_monthslope,S6_monthslope))%>%
  gather(wave,monthslope,-ID)
monthslope$wave <- gsub("[^0-9.-]", "", monthslope$wave)

#weekslope
weekslope <- avoidall_household%>%
  dplyr::select(ID,c(S1_weekslope,S2_weekslope,S3_weekslope,
              S4_weekslope,S5_weekslope,S6_weekslope))%>%
  gather(wave,weekslope,-ID)
weekslope$wave <- gsub("[^0-9.-]", "", weekslope$wave)

#monthcoverage
monthcoverage <- avoidall_household%>%
  dplyr::select(ID,c(S1_monthcoverage,S2_monthcoverage,S3_monthcoverage,
              S4_monthcoverage,S5_monthcoverage,S6_monthcoverage))%>%
  gather(wave,monthcoverage,-ID)
monthcoverage$wave <- gsub("[^0-9.-]", "", monthcoverage$wave)

#weekcoverage
weekcoverage <- avoidall_household%>%
  dplyr::select(ID,c(S1_weekcoverage,S2_weekcoverage,S3_weekcoverage,
              S4_weekcoverage,S5_weekcoverage,S6_weekcoverage))%>%
  gather(wave,weekcoverage,-ID)
weekcoverage$wave <- gsub("[^0-9.-]", "", weekcoverage$wave)

#benefit_house
benefit_house <- avoidall_household%>%
  dplyr::select(ID,c(S1_benefit_house,S2_benefit_house,S3_benefit_house,
              S4_benefit_house,S5_benefit_house,S6_benefit_house))%>%
  gather(wave,benefit_house,-ID)
benefit_house$wave <- gsub("[^0-9.-]", "", benefit_house$wave)

#resources
resources <- avoidall_household%>%
  dplyr::select(ID,c(S1_resources,S2_resources,S3_resources,
              S4_resources,S5_resources,S6_resources))%>%
  gather(wave,resources,-ID)
resources$wave <- gsub("[^0-9.-]", "", resources$wave)

#benefit_community
benefit_community <- avoidall_household%>%
  dplyr::select(ID,c(S1_benefit_community,S2_benefit_community,S3_benefit_community,
              S4_benefit_community,S5_benefit_community,S6_benefit_community))%>%
  gather(wave,benefit_community,-ID)
benefit_community$wave <- gsub("[^0-9.-]", "", benefit_community$wave)

#avoidall_behavior
avoidall_behavior <- avoidall_household%>%
  dplyr::select(ID,c(S1_avoidall_behavior,S2_avoidall_behavior,S3_avoidall_behavior,
              S4_avoidall_behavior,S5_avoidall_behavior,S6_avoidall_behavior))%>%
  gather(wave,avoidall_behavior,-ID)
avoidall_behavior$wave <- gsub("[^0-9.-]", "", avoidall_behavior$wave)


mlmprep <- 
  left_join(news_consumption, monthslope, by = c("ID", "wave")) %>%
  left_join(weekslope, by = c("ID", "wave")) %>%
  left_join(monthcoverage, by = c("ID", "wave")) %>%
  left_join(weekcoverage,by = c("ID", "wave")) %>%
  left_join(benefit_house, by = c("ID", "wave")) %>%
  left_join(benefit_community, by = c("ID", "wave")) %>%
  left_join(resources, by = c("ID", "wave")) %>%
  left_join(avoidall_behavior, by = c("ID", "wave"))%>%
  left_join(dplyr::select(avoidall_household,c(ID:political)), by = "ID")

mlmprep$wave <- as.character(mlmprep$wave)
mlmprep$gender <- as.character(mlmprep$gender)
mlmprep$education <- as.character(mlmprep$education)
mlmprep$income <- as.character(mlmprep$income)
mlmprep$ethnicity <- as.character(mlmprep$ethnicity)

mlmprep$news_exposure_w = mlmprep$news_consumption*mlmprep$weekcoverage
mlmprep$news_exposure_m = mlmprep$news_consumption*mlmprep$monthcoverage

mlmprep <- mlmprep %>% 
  mutate(across(where(is.numeric), scale))

model <- '
    level:1
        avoidall_behavior ~ wave + resources + benefit_house + benefit_community + monthslope*weekslope
        resources ~ news_exposure_w*news_exposure_m
        benefit_house ~ news_exposure_w*news_exposure_m
        benefit_community ~ news_exposure_w*news_exposure_m
    level:2
        avoidall_behavior ~ age + gender + education + income + ethnicity + party + political
'


model <- '
    level:1
        avoidall_behavior ~ wave + resources + benefit_house + benefit_community + weekslope
        benefit_house ~ news_exposure_w
        benefit_community ~ news_exposure_w
    level:2
        benefit_house ~ age + gender + education + income + ethnicity + party + political
        benefit_community ~ age + gender + education + income + ethnicity + party + political
'


avoidall_fit <- sem(model = model, data = mlmprep, 
                cluster = "ID",
                verbose = TRUE, optim.method = "em", em.iter.max = 20000,
                em.fx.tol = 1e-04, em.dx.tol = 1e-04)

summary(avoidall_fit,fit.measures = TRUE)

summary(vaccinate_fit,fit.measures = TRUE)

summary(wash_fit,fit.measures = TRUE)

summary(mask_fit,fit.measures = TRUE)

summary(quarantine_fit,fit.measures = TRUE)



