---
title: "Russia Foreign Vaccines R Code"
author: "Ayse D Lokmanoglu"
date: "8/1/2022"
output: github_document
---

```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
knitr::opts_chunk$set(eval = FALSE)
```
### Supplementary Code for:
### 
#### Authors: Erik Nisbet, PhD; Ayse D. Lokmanoglu, PhD; and Olga Kamenchuk, PhD

***Due to our terms of agreement of NewsWhip API, we will not provide individual level information of articles, only aggregate date.***

Import libraries
```{r}
library(stringi) 
library(stringr)
library(qdap)
library(tm)
library(ggplot2)
library(lubridate)
library(irr)
library(quanteda)
library(ldatuning)
library(topicmodels)
library(textcat)
library(parallel)
library(RSQLite)
library(doParallel)
library(scales)
library(lsa)
library(igraph)
library(cld2) 
library(tidyverse)
library(tidytext)
library(dplyr)
library(rgexf)
library(openxlsx)
library(ggthemes)
```

Load pre-processed data frame with text column labelled as text.
1. Tokenize it
```{r}
toks <- tokens(mydata$text,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE,
               include_docvars = TRUE,
               padding = FALSE) %>%
  tokens_remove(c(stopwords("ru"),
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
```
2. Change it into a [document-feature matrix](https://quanteda.io/reference/dfm.html)
```{r}
dfm_counts<- dfm(toks) 
rm(toks) #remove unused files to save space
```
3. Match your dfm object with your original data frame through index
```{r}
docnames(dfm_counts)<-mydata$index
```
4. Check for sparsity and trim accordingly
```{r}
sparsity(dfm_counts)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.95, min_docfreq=0.05,docfreq_type="prop")
sparsity(dfm_counts2)
rm(dfm_counts) #remove for space
```
5. Convert dfm object to an LDA object
```{r}
dtm_lda <- convert(dfm_counts2, to = "topicmodels",docvars = dfm_counts2@docvars)
full_data<-dtm_lda
n <- nrow(full_data) #number of rows for cross-validation method
rm(dfm_counts2) #remove for space
```
6. Run the cross-validation, save the results. The method is from the [supplemental code](https://github.com/aysedeniz09/AJPH2020/blob/master/AJPH%20GITupload.R), citation: Walter, D., Ophir, Y., & Jamieson, K. H. (2020). Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015–2017. American Journal of Public Health, 110(5), 718–724. <https://doi.org/10.2105/AJPH.2019.305564>.
```{r}
print(Sys.time())
# create container for results
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
# set possible alpha and k values
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # candidates for alpha values
candidate_k <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
# run the 10-fold cross validation
for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  cluster <- makeCluster(detectCores(logical = TRUE) - 1) # We are leaving one Core spare. If number of corse on pc is 1, then -1 in this line should be removed.
  registerDoParallel(cluster)
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  folds <- 10
  splitfolds <- sample(1:folds, n, replace = TRUE)
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(alpha=eachalpha) )
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
}
print ("DONE!")
print(Sys.time())
save(MainresultDF, file="Main_Results_DF_DATE.Rda")
```
7. Examine the output by visualizing
```{r}
MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

ggplot(MainresultDF)+geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
ggplot(MainresultDF)+geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha))
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))


p<-ggplot(MainresultDF, aes(x = k, y = perplexity))
Alpha<-MainresultDF$myalpha
p+geom_point(aes(color=Alpha),size=0.1)+geom_smooth(se = FALSE, aes(color=Alpha))+
  ggtitle("5-fold cross-validation of topic modelling (5% of data)",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
ggsave("Alpha and Perplexity.jpg")

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  scale_color_discrete(name = "Alpha Levels")+
  xlab("K (Number of Topics)")+
  ylab("Perplexity")
b<-ggplot(MainresultDF) +
  geom_smooth(se = FALSE, aes(x=k, y=perplexity,color=myalpha)) +  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  xlab("Topics (k)")+
  ylab("Perplexity")+
  theme_wsj(color="white") +
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="top", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
```
8. Choose the alpha and test approximate topic numbers, in our case it was alpha=0.05 and k=65
```{r}
# Identify 2nd derivative max point on perplexity  
MainresultDF_MYALPHA<-MainresultDF[MainresultDF$myalpha==0.05,]
cars.spl <- with(MainresultDF_MYALPHA, smooth.spline(k, perplexity, df = 3))
plot(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)), type = "l",
     #main = "My title",
     #sub = "My subtitle",
     xlab = "Topics (k)",
     ylab = "Perplexity Second Derivative") + abline(v=30)
data<-data.frame(with(cars, predict(cars.spl, x = MainresultDF_MYALPHA$k, deriv = 2)))

a<- ggplot(data,aes(x=x,y=y))+
  geom_line(color = "grey11")+
  geom_vline(xintercept=65) +
  scale_x_continuous(breaks = seq(from = 0, to = 150, by = 20)) +
  ggtitle(" ")+
  xlab("Topics (k)")+
  ylab("Perplexity Second Derivative")+
  theme_wsj(color="white")+
  theme(text=element_text(size=10,family="Times New Roman"),
        title=element_text(size=12,family="Times New Roman"),
        axis.text.x=element_text(angle=60, hjust=1, family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        axis.title.y=element_text(vjust=-0.25, size=10, family="Times New Roman"),
        legend.position="bottom", legend.box="vertical", legend.margin=margin(),
        legend.key = element_rect(fill="white"), legend.background = element_rect(fill=NA),
        legend.text=element_text(size=8, family="Times New Roman"))
ggpubr::ggarrange(b, a, ncol = 1, nrow = 2)
```
![Al-naba Topic Number Perplexity Graph](https://github.com/nwccpp/russia_foreignvaccine_paper/blob/main/images/Figure_S1_1_Perpelexity.jpg?raw=true)
### Topic Modeling
1. Run the topic model for the identified k and alpha
```{r}
Sys.time()
lda.65.05 <- LDA(full_data, 
                 k = 65, 
                 method = "Gibbs",
                 control = list(alpha=0.05,seed=9512))

save(lda.65.05, file="NW_RU_k65_05_lda_DATE.Rda")
Sys.time()
beepr::beep()

LDAfit<-lda.65.05 #copy the object it with a different name for backup
```
2. Extract top words from the topic model
```{r}
#mark text column
datacolnum=which( colnames(mydata)=="text")

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
openxlsx::write.xlsx(topwords, file="NW_RU_k65_topwords_DATE.xlsx")
```
3. Extract top articles
```{r}
metadf<-mydata #your original dataframe
meta_theta_df<-cbind(metadf[,"text"],LDAfit@gamma)
ntext=30
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
openxlsx::write.xlsx(toptexts, file="NW_RU_k65_toptexts_DATE.xlsx")
```
4. Save the complete file with topic loadings and all information
```{r}
meta_theta_df3<-cbind(metadf,LDAfit@gamma)
first<-which(colnames(meta_theta_df3)=="1")
last<-ncol(meta_theta_df3)
colnames(meta_theta_df3)[first:last] <- paste("X", colnames(meta_theta_df3[,c(first:last)]), sep = "_")
save(meta_theta_df3, file="NW_RU_k65_MetaThetaDF_DATE.Rda")
```
### Networked Clusters
The method is from the [supplemental code](https://github.com/DrorWalt/ANTMN), citation: Walter, D., & Ophir, Y. (2019). News Frame Analysis: An Inductive Mixed-Method Computational Approach. Communication Methods and Measures. <https://doi.org/10.1080/19312458.2019.1639145>.
1. Load topic names assigned by researchers
```{r}
topic_names<-readxl::read_excel("Russia_FVaccine_600char_k65_topicnames.xlsx")
topic_names<-topic_names$Topic_Names_v2
deleted_topics=(grep("DELETE:",topic_names))
```
2. Calculate the topic sizes
```{r}
#use meta_theta_df3
first<-which(colnames(meta_theta_df3)=="X_1")
last<-which(colnames(meta_theta_df3)=="X_65")
topicsize<-colMeans(meta_theta_df3[,first:last])
```
3. Create [ANTMN](https://github.com/DrorWalt/ANTMN) function
```{r}
# load libraries
library(igraph)
library(corpustools)
library(lsa)

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
  
  # calculate the adjacency matrix using cosine similarity on the theta matrix
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
  V(newg)$degree <- degree(newg)                        # Degree centrality
  V(newg)$eig <- evcent(newg)$vector                    # Eigenvector centrality
  V(newg)$hubs <- hub.score(newg)$vector                # "Hub" centrality
  V(newg)$authorities <- authority.score(newg)$vector   # "Authority" centrality
  V(newg)$closeness <- closeness(newg)                  # Closeness centrality
  V(newg)$betweenness <- betweenness(newg)  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write.graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}
```
4. Run the ANTMN function with your lda object
```{r}
mynewnet<-network_from_LDA(LDAobject=LDAfit,
                           topic_names=mynames,
                           topic_size=topicsize,
                           deleted_topics=deleted_topics=c(8, 26),                     save_filename="NW_RU_antmn_DATE",
                           bbone=TRUE)

save(mynewnet, file="NW_RU_antmn_DATE.Rda")
```
5. Open file in [Gephi](https://gephi.org/) to visualize it with Walktrap Algorithm.![Network Graph](https://github.com/nwccpp/russia_foreignvaccine_paper/blob/main/images/ANTMN_WalkTrap_Publication_080122.png?raw=true) 
6. Calculate networked clusters
```{r}
meta_theta_df_comm_wt<-meta_theta_df2
rm(meta_theta_df2)

# Calculating cumulative Theme loading from walktrap
# 1 – Purple
# 2 – Light Green
# 3 – Light Blue
# 4 – Gray/Brown
# 5 – Orange
# 6 – Dark Green
# 7 - Pink

which(colnames(meta_theta_df_comm_wt)=="X_1") #### -1 for rowsums
meta_theta_df_comm_wt$PurpleCom1<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 1)]))+48]) 
meta_theta_df_comm_wt$LightGreenCom2<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 2)]))+48]) 
meta_theta_df_comm_wt$LighBlueCom3<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 3)]))+48]) 
meta_theta_df_comm_wt$GrayCom4<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 4)]))+48]) 
meta_theta_df_comm_wt$OrangeCom5<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 5)]))+48]) 
meta_theta_df_comm_wt$DarkGreenCom6<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 6)]))+48]) 
meta_theta_df_comm_wt$PinkCom7<-rowSums(meta_theta_df_comm_wt[,(as.numeric(V(mynewnet)$label[which(V(mynewnet)$walktrap == 7)]))+48]) 

save(meta_theta_df_comm_wt, file="Russia_FV_k65_Meta_Theta_Df_WalkTrap_DATE.Rda")
```
### Sentiment Analysis
1. Load [NRC](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) to create NRC for Russian
```{r}
nrcmain<-get_sentiments("nrc")
NRC_All <- readxl::read_excel("NRC_All.xlsx")
names(NRC_All)[1]<-"word"
nrc<-inner_join(NRC_All, nrcmain)
nrcRU<-dplyr::select(nrc, 'Russian (ru)', sentiment)
names(nrcRU)[1]<-"word"
save(nrcRU,file="NRC_Russian.Rda")
```
2. Change name of Meta_Theta_DF to standardize code
```{r}
data_master<-meta_theta_df_comm_wt
```
3. Create a stopwords dataframe
```{r}
stopwords<-c(stopwords("ru"),
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
```
4. Run sentiment dictionary
```{r}
temp_test<-data_master %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords) %>%
  inner_join(nrcRU) %>%
  group_by(index) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0) 
temp_test3<-full_join(data_master, temp_test, by="index")

##create a net sentiment column
temp_test3<-temp_test3 %>% mutate(sentiment=positive-negative)
```
5. Calculate cluster net sentiment  and trust score by multiplying with cluster prominence
```{r}
### trust
trust_k65<-select(temp_test3, PurpleCom1:PinkCom7)
colnames(trust_k65) <- paste("trust", colnames(trust_k65), sep = "_")

for (i in 1:ncol(trust_k65)) {
  trust_k65[ , i] <- trust_k65[ , i] * temp_test3$trust
}
temp_test3<-cbind(temp_test3, trust_k65)
temptrust<-cbind(temp_test3[,"date"],trust_k65)
colnames(temptrust)[1]<-"date"

### sentiment
sent_k65<-select(temp_test3, PurpleCom1:PinkCom7)
colnames(sent_k65) <- paste("sentiment", colnames(sent_k65), sep = "_")

for (i in 1:ncol(sent_k65)) {
  sent_k65[ , i] <- sent_k65[ , i] * temp_test3$sentiment
}

temp_test3<-cbind(temp_test3, sent_k65)

### save the master file 
save(temp_test3,file="Russia_FV_k65_Meta_Theta_Df_WalkTrap_Sentiment_DATE.Rda")
```
### Creating the Dependent and Independent Variables
1. Create independent variables of cluster net sentiment and cluster trust
```{r}
## create a total engagement column and select the clusters and sentiment clusters
temp <-temp_test3 %>% 
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, tw_data.tw_count)) %>%
  dplyr::select(date, index, 
                uuid, 
                engagement,
                PurpleCom1, LightGreenCom2, LighBlueCom3, GrayCom4,
                OrangeCom5, DarkGreenCom6, PinkCom7,
                neg_PurpleCom1:sentiment_PinkCom7)

### Create the daily mean 
temp_by_day_wt<-aggregate(x=temp[,5:ncol(temp)],by=list(temp$date), FUN="mean", na.rm=TRUE)
colnames(temp_by_day_wt)[1]<-"date"  
save(temp_by_day_wt,file="Russia_FV_k65_WT_IV_DATE.Rda")
```
2. Create the depedent variables, retrieving them from retrieved from the API of The Global COVID-19 Trends and Impact Survey Open Data API [Fan et al., 2020a](https://gisumd.github.io/COVID-19-API-Documentation/). 
```{r}
## Load from harvard dataverse
#UMDIndicatorsRussia_pct_vu_v2$date<- mdy(UMDIndicatorsRussia_pct_vu_v2$date)
summary(temp_DV$pct_vu)
summary(temp_DV$pct_cse)
```
3. Join IV's DV's for a master file ready for time series analysis, and filter the dates for papers time-frame
```{r}
temp_DV_IV_wt<-left_join(temp_by_day_wt, temp_DV, by="date")

temp_DV_IV_wt<-temp_DV_IV_wt %>%
  filter(date>'2021-05-19')
save(temp_DV_IV_wt,file="Russia_Fv_k65_WT_IV_DV_DATE.Rda")

## Look at the descriptives
vtable::sumtable(temp_DV_IV_wt)
```
### Time Series Analysis
1. Load packages
```{r}
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
```
2. Load the dataset
```{r}
## Harvard dataverse link
```
3. Create descriptive statistics
```{r}
temp_DV_IV_wt %>% dplyr::select( 
                                PurpleCom1,
                                sentiment_PurpleCom1,
                                trust_PurpleCom1,
                                LightGreenCom2,
                                sentiment_LightGreenCom2,
                                trust_LightGreenCom2,
                                LighBlueCom3,
                                sentiment_LighBlueCom3,
                                trust_LighBlueCom3,
                                pct_vu,
                                pct_cse) %>%
  vtable::sumtable(summ=c('notNA(x)', 'mean(x)', 'sd(x)', 'var(x)', 'min(x)','max(x)'),
                   out='csv', 
                   file="Russia_Fv_k65_WT_IVaDV_Desc_DATE.csv")
```
4. Run time series models, with AIC and stationary tests for Vaccine Acceptance
```{r}
######################################################
#############Model 1 PurpleCom1 Pct_VU##############
#####################################################
#######Model 1 
dat_comm1.1<-temp_DV_IV_wt %>% 
  dplyr::select(PurpleCom1, pct_vu) %>%
  drop_na()
dat_comm1.1 <- ts(dat_comm1.1)
plot(dat_comm1.1)

#Test for stationarity
Acf(dat_comm1.1[,"PurpleCom1"])
Acf(dat_comm1.1[,"pct_vu"])
adf.test(dat_comm1.1[,"PurpleCom1"], k=5)
adf.test(dat_comm1.1[,"pct_vu"], k=5)
kpss.test(dat_comm1.1[,"PurpleCom1"])
kpss.test(dat_comm1.1[,"pct_vu"])
coint1.1 <- dynlm(PurpleCom1~pct_vu, data=dat_comm1.1)
ehat1.1 <- resid(coint1.1)
adf.test(ehat1.1, k=3)

#VAR Comm1.1
fitvarcomm1.1 <- VAR(dat_comm1.1, p=3, type="both")
summary(fitvarcomm1.1) # comm1.1 pct_vu
causality(fitvarcomm1.1, cause = "PurpleCom1")
#causality(fitvarcomm1.1, cause = "")
irf_comm1.1_pct_vu <- irf(fitvarcomm1.1, impulse = "PurpleCom1", response = "pct_vu", boot = TRUE)
plot(irf_comm1.1_pct_vu)
plot(fevd(fitvarcomm1.1))

####model 1.4 trust_PurpleCom1 and pct_vu
dat_comm1.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_PurpleCom1, pct_vu) %>%
  drop_na()
dat_comm1.4 <- ts(dat_comm1.4)
plot(dat_comm1.4)

#Test for stationarity
Acf(dat_comm1.4[,"trust_PurpleCom1"])
Acf(dat_comm1.4[,"pct_vu"])
adf.test(dat_comm1.4[,"trust_PurpleCom1"], k=5)
adf.test(dat_comm1.4[,"pct_vu"], k=5)
kpss.test(dat_comm1.4[,"trust_PurpleCom1"])
kpss.test(dat_comm1.4[,"pct_vu"])
coint1.4 <- dynlm(trust_PurpleCom1~pct_vu, data=dat_comm1.4)
ehat1.4 <- resid(coint1.4)
adf.test(ehat1.4, k=5)

#VAR Comm1.4
fitvarcomm1.4 <- VAR(dat_comm1.4, p=3, type="both")
summary(fitvarcomm1.4) # comm1.4 pct_vu
causality(fitvarcomm1.4, cause = "trust_PurpleCom1")
#causality(fitvarcomm1.4, cause = "")
irf_comm1.4_pct_vu <- irf(fitvarcomm1.4, impulse = "trust_PurpleCom1", response = "pct_vu", boot = TRUE)
plot(irf_comm1.4_pct_vu)
plot(fevd(fitvarcomm1.4))

####model 1.5 sentiment_PurpleCom1 and pct_vu
dat_comm1.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_PurpleCom1, pct_vu) %>%
  drop_na()
dat_comm1.5 <- ts(dat_comm1.5)
plot(dat_comm1.5)

#Test for stationarity
Acf(dat_comm1.5[,"sentiment_PurpleCom1"])
Acf(dat_comm1.5[,"pct_vu"])
adf.test(dat_comm1.5[,"sentiment_PurpleCom1"], k=5)
adf.test(dat_comm1.5[,"pct_vu"], k=5)
kpss.test(dat_comm1.5[,"sentiment_PurpleCom1"])
kpss.test(dat_comm1.5[,"pct_vu"])
coint1.5 <- dynlm(sentiment_PurpleCom1~pct_vu, data=dat_comm1.5)
ehat1.5 <- resid(coint1.5)
adf.test(ehat1.5, k=5)

#VAR Comm1.5
fitvarcomm1.5 <- VAR(dat_comm1.5, p=3, type="both")
summary(fitvarcomm1.5) # comm1.5 pct_vu
causality(fitvarcomm1.5, cause = "sentiment_PurpleCom1")
#causality(fitvarcomm1.5, cause = "")
irf_comm1.5_pct_vu <- irf(fitvarcomm1.5, impulse = "sentiment_PurpleCom1", response = "pct_vu", boot = TRUE)
plot(irf_comm1.5_pct_vu)
plot(fevd(fitvarcomm1.5))

######retrieve info from main models for tables
varcomm1.1G <- fitvarcomm1.1[["varresult"]]$pct_vu
varcomm1.4G <- fitvarcomm1.4[["varresult"]]$pct_vu
varcomm1.5G <- fitvarcomm1.5[["varresult"]]$pct_vu

######################################################
#############Model 2 LightGreenCom2 Pct_VU##############
#####################################################
#######Model 2.1 
dat_comm2.1<-temp_DV_IV_wt %>% 
  dplyr::select(LightGreenCom2, pct_vu) %>%
  drop_na()
dat_comm2.1 <- ts(dat_comm2.1)
plot(dat_comm2.1)

#Test for stationarity
Acf(dat_comm2.1[,"LightGreenCom2"])
Acf(dat_comm2.1[,"pct_vu"])
adf.test(dat_comm2.1[,"LightGreenCom2"], k=5)
adf.test(dat_comm2.1[,"pct_vu"], k=5)
kpss.test(dat_comm2.1[,"LightGreenCom2"])
kpss.test(dat_comm2.1[,"pct_vu"])
coint2.1 <- dynlm(LightGreenCom2~pct_vu, data=dat_comm2.1)
ehat2.1 <- resid(coint2.1)
adf.test(ehat2.1, k=5)

#VAR Comm2.1
fitvarcomm2.1 <- VAR(dat_comm2.1, p=3, type="both")
summary(fitvarcomm2.1) # comm2.1 pct_vu
causality(fitvarcomm2.1, cause = "LightGreenCom2")
#causality(fitvarcomm2.1, cause = "")
irf_comm2.1_pct_vu <- irf(fitvarcomm2.1, impulse = "LightGreenCom2", response = "pct_vu", boot = TRUE)
plot(irf_comm2.1_pct_vu)
plot(fevd(fitvarcomm2.1))

####model 2.4 trust_LightGreenCom2 and pct_vu
dat_comm2.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_LightGreenCom2, pct_vu) %>%
  drop_na()
dat_comm2.4 <- ts(dat_comm2.4)
plot(dat_comm2.4)

#Test for stationarity
Acf(dat_comm2.4[,"trust_LightGreenCom2"])
Acf(dat_comm2.4[,"pct_vu"])
adf.test(dat_comm2.4[,"trust_LightGreenCom2"], k=5)
adf.test(dat_comm2.4[,"pct_vu"], k=5)
kpss.test(dat_comm2.4[,"trust_LightGreenCom2"])
kpss.test(dat_comm2.4[,"pct_vu"])
coint2.4 <- dynlm(trust_LightGreenCom2~pct_vu, data=dat_comm2.4)
ehat2.4 <- resid(coint2.4)
adf.test(ehat2.4, k=5)

#VAR Comm2.4
fitvarcomm2.4 <- VAR(dat_comm2.4, p=3, type="both")
summary(fitvarcomm2.4) # comm2.4 pct_vu
causality(fitvarcomm2.4, cause = "trust_LightGreenCom2")
#causality(fitvarcomm2.4, cause = "")
irf_comm2.4_pct_vu <- irf(fitvarcomm2.4, impulse = "trust_LightGreenCom2", response = "pct_vu", boot = TRUE)
plot(irf_comm2.4_pct_vu)
plot(fevd(fitvarcomm2.4))

####model 2.5 sentiment_LightGreenCom2 and pct_vu
dat_comm2.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_LightGreenCom2, pct_vu) %>%
  drop_na()
dat_comm2.5 <- ts(dat_comm2.5)
plot(dat_comm2.5)

#Test for stationarity
Acf(dat_comm2.5[,"sentiment_LightGreenCom2"])
Acf(dat_comm2.5[,"pct_vu"])
adf.test(dat_comm2.5[,"sentiment_LightGreenCom2"], k=5)
adf.test(dat_comm2.5[,"pct_vu"], k=5)
kpss.test(dat_comm2.5[,"sentiment_LightGreenCom2"])
kpss.test(dat_comm2.5[,"pct_vu"])
coint2.5 <- dynlm(sentiment_LightGreenCom2~pct_vu, data=dat_comm2.5)
ehat2.5 <- resid(coint2.5)
adf.test(ehat2.5, k=5)

#VAR Comm2.5
fitvarcomm2.5 <- VAR(dat_comm2.5, p=3, type="both")
summary(fitvarcomm2.5) # comm2.5 pct_vu
causality(fitvarcomm2.5, cause = "sentiment_LightGreenCom2")
#causality(fitvarcomm2.5, cause = "")
irf_comm2.5_pct_vu <- irf(fitvarcomm2.5, impulse = "sentiment_LightGreenCom2", response = "pct_vu", boot = TRUE)
plot(irf_comm2.5_pct_vu)
plot(fevd(fitvarcomm2.5))

######retrieve info from main models for tables
varcomm2.1G <- fitvarcomm2.1[["varresult"]]$pct_vu
varcomm2.4G <- fitvarcomm2.4[["varresult"]]$pct_vu
varcomm2.5G <- fitvarcomm2.5[["varresult"]]$pct_vu

######################################################
#############Model 3 LighBlueCom3 Pct_VU##############
#####################################################
#######Model 3.1 
dat_comm3.1<-temp_DV_IV_wt %>% 
  dplyr::select(LighBlueCom3, pct_vu) %>%
  drop_na()
dat_comm3.1 <- ts(dat_comm3.1)
plot(dat_comm3.1)

#Test for stationarity
Acf(dat_comm3.1[,"LighBlueCom3"])
Acf(dat_comm3.1[,"pct_vu"])
adf.test(dat_comm3.1[,"LighBlueCom3"], k=5)
adf.test(dat_comm3.1[,"pct_vu"], k=5)
kpss.test(dat_comm3.1[,"LighBlueCom3"])
kpss.test(dat_comm3.1[,"pct_vu"])
coint3.1 <- dynlm(LighBlueCom3~pct_vu, data=dat_comm3.1)
ehat3.1 <- resid(coint3.1)
adf.test(ehat3.1, k=5)

#VAR Comm3.1
fitvarcomm3.1 <- VAR(dat_comm3.1, p=3, type="both")
summary(fitvarcomm3.1) # comm3.1 pct_vu
causality(fitvarcomm3.1, cause = "LighBlueCom3")
#causality(fitvarcomm3.1, cause = "")
irf_comm3.1_pct_vu <- irf(fitvarcomm3.1, impulse = "LighBlueCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.1_pct_vu)
plot(fevd(fitvarcomm3.1))

####model 3.4 trust_LighBlueCom3 and pct_vu
dat_comm3.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_LighBlueCom3, pct_vu) %>%
  drop_na()
dat_comm3.4 <- ts(dat_comm3.4)
plot(dat_comm3.4)

#Test for stationarity
Acf(dat_comm3.4[,"trust_LighBlueCom3"])
Acf(dat_comm3.4[,"pct_vu"])
adf.test(dat_comm3.4[,"trust_LighBlueCom3"], k=5)
adf.test(dat_comm3.4[,"pct_vu"], k=5)
kpss.test(dat_comm3.4[,"trust_LighBlueCom3"])
kpss.test(dat_comm3.4[,"pct_vu"])
coint3.4 <- dynlm(trust_LighBlueCom3~pct_vu, data=dat_comm3.4)
ehat3.4 <- resid(coint3.4)
adf.test(ehat3.4, k=5)

#VAR Comm3.4
fitvarcomm3.4 <- VAR(dat_comm3.4, p=3, type="both")
summary(fitvarcomm3.4) # comm3.4 pct_vu
causality(fitvarcomm3.4, cause = "trust_LighBlueCom3")
#causality(fitvarcomm3.4, cause = "")
irf_comm3.4_pct_vu <- irf(fitvarcomm3.4, impulse = "trust_LighBlueCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.4_pct_vu)
plot(fevd(fitvarcomm3.4))

####model 3.5 sentiment_LighBlueCom3 and pct_vu
dat_comm3.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_LighBlueCom3, pct_vu) %>%
  drop_na()
dat_comm3.5 <- ts(dat_comm3.5)
plot(dat_comm3.5)

#Test for stationarity
Acf(dat_comm3.5[,"sentiment_LighBlueCom3"])
Acf(dat_comm3.5[,"pct_vu"])
adf.test(dat_comm3.5[,"sentiment_LighBlueCom3"], k=5)
adf.test(dat_comm3.5[,"pct_vu"], k=5)
kpss.test(dat_comm3.5[,"sentiment_LighBlueCom3"])
kpss.test(dat_comm3.5[,"pct_vu"])
coint3.5 <- dynlm(sentiment_LighBlueCom3~pct_vu, data=dat_comm3.5)
ehat3.5 <- resid(coint3.5)
adf.test(ehat3.5, k=5)

#VAR Comm3.5
fitvarcomm3.5 <- VAR(dat_comm3.5, p=3, type="both")
summary(fitvarcomm3.5) # comm3.5 pct_vu
causality(fitvarcomm3.5, cause = "sentiment_LighBlueCom3")
#causality(fitvarcomm3.5, cause = "")
irf_comm3.5_pct_vu <- irf(fitvarcomm3.5, impulse = "sentiment_LighBlueCom3", response = "pct_vu", boot = TRUE)
plot(irf_comm3.5_pct_vu)
plot(fevd(fitvarcomm3.5))

######retrieve info from main models for tables
varcomm3.1G <- fitvarcomm3.1[["varresult"]]$pct_vu
varcomm3.4G <- fitvarcomm3.4[["varresult"]]$pct_vu
varcomm3.5G <- fitvarcomm3.5[["varresult"]]$pct_vu
```
4. Run time series models, with AIC and stationary tests for Side Effect Worry
```{r}
######################################################
#############Model 1 PurpleCom1 pct_cse##############
#####################################################
#######Model 1 
dat_comm1_2.1<-temp_DV_IV_wt %>% 
  dplyr::select(PurpleCom1, pct_cse) %>%
  drop_na()
dat_comm1_2.1 <- ts(dat_comm1_2.1)
plot(dat_comm1_2.1)

#Test for stationarity
Acf(dat_comm1_2.1[,"PurpleCom1"])
Acf(dat_comm1_2.1[,"pct_cse"])
adf.test(dat_comm1_2.1[,"PurpleCom1"], k=5)
adf.test(dat_comm1_2.1[,"pct_cse"], k=5)
kpss.test(dat_comm1_2.1[,"PurpleCom1"])
kpss.test(dat_comm1_2.1[,"pct_cse"])
coint1_2.1 <- dynlm(PurpleCom1~pct_cse, data=dat_comm1_2.1)
ehat1_2.1 <- resid(coint1_2.1)
adf.test(ehat1_2.1, k=3)

#VAR Comm1_2.1
fitvarcomm1_2.1 <- VAR(dat_comm1_2.1, p=3, type="both")
summary(fitvarcomm1_2.1) # comm1_2.1 pct_cse
causality(fitvarcomm1_2.1, cause = "PurpleCom1")
#causality(fitvarcomm1_2.1, cause = "")
irf_comm1_2.1_pct_cse <- irf(fitvarcomm1_2.1, impulse = "PurpleCom1", response = "pct_cse", boot = TRUE)
plot(irf_comm1_2.1_pct_cse)
plot(fevd(fitvarcomm1_2.1))

####model 1_2.4 trust_PurpleCom1 and pct_cse
dat_comm1_2.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_PurpleCom1, pct_cse) %>%
  drop_na()
dat_comm1_2.4 <- ts(dat_comm1_2.4)
plot(dat_comm1_2.4)

#Test for stationarity
Acf(dat_comm1_2.4[,"trust_PurpleCom1"])
Acf(dat_comm1_2.4[,"pct_cse"])
adf.test(dat_comm1_2.4[,"trust_PurpleCom1"], k=5)
adf.test(dat_comm1_2.4[,"pct_cse"], k=5)
kpss.test(dat_comm1_2.4[,"trust_PurpleCom1"])
kpss.test(dat_comm1_2.4[,"pct_cse"])
coint1_2.4 <- dynlm(trust_PurpleCom1~pct_cse, data=dat_comm1_2.4)
ehat1_2.4 <- resid(coint1_2.4)
adf.test(ehat1_2.4, k=5)

#VAR Comm1_2.4
fitvarcomm1_2.4 <- VAR(dat_comm1_2.4, p=3, type="both")
summary(fitvarcomm1_2.4) # comm1_2.4 pct_cse
causality(fitvarcomm1_2.4, cause = "trust_PurpleCom1")
#causality(fitvarcomm1_2.4, cause = "")
irf_comm1_2.4_pct_cse <- irf(fitvarcomm1_2.4, impulse = "trust_PurpleCom1", response = "pct_cse", boot = TRUE)
plot(irf_comm1_2.4_pct_cse)
plot(fevd(fitvarcomm1_2.4))

####model 1_2.5 sentiment_PurpleCom1 and pct_cse
dat_comm1_2.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_PurpleCom1, pct_cse) %>%
  drop_na()
dat_comm1_2.5 <- ts(dat_comm1_2.5)
plot(dat_comm1_2.5)

#Test for stationarity
Acf(dat_comm1_2.5[,"sentiment_PurpleCom1"])
Acf(dat_comm1_2.5[,"pct_cse"])
adf.test(dat_comm1_2.5[,"sentiment_PurpleCom1"], k=5)
adf.test(dat_comm1_2.5[,"pct_cse"], k=5)
kpss.test(dat_comm1_2.5[,"sentiment_PurpleCom1"])
kpss.test(dat_comm1_2.5[,"pct_cse"])
coint1_2.5 <- dynlm(sentiment_PurpleCom1~pct_cse, data=dat_comm1_2.5)
ehat1_2.5 <- resid(coint1_2.5)
adf.test(ehat1_2.5, k=5)

#VAR Comm1_2.5
fitvarcomm1_2.5 <- VAR(dat_comm1_2.5, p=3, type="both")
summary(fitvarcomm1_2.5) # comm1_2.5 pct_cse
causality(fitvarcomm1_2.5, cause = "sentiment_PurpleCom1")
#causality(fitvarcomm1_2.5, cause = "")
irf_comm1_2.5_pct_cse <- irf(fitvarcomm1_2.5, impulse = "sentiment_PurpleCom1", response = "pct_cse", boot = TRUE)
plot(irf_comm1_2.5_pct_cse)
plot(fevd(fitvarcomm1_2.5))

######retrieve info from main models for tables
varcomm1_2.1G <- fitvarcomm1_2.1[["varresult"]]$pct_cse
varcomm1_2.4G <- fitvarcomm1_2.4[["varresult"]]$pct_cse
varcomm1_2.5G <- fitvarcomm1_2.5[["varresult"]]$pct_cse

######################################################
#############Model 2 LightGreenCom2 pct_cse##############
#####################################################
#######Model 2_2.1 
dat_comm2_2.1<-temp_DV_IV_wt %>% 
  dplyr::select(LightGreenCom2, pct_cse) %>%
  drop_na()
dat_comm2_2.1 <- ts(dat_comm2_2.1)
plot(dat_comm2_2.1)

#Test for stationarity
Acf(dat_comm2_2.1[,"LightGreenCom2"])
Acf(dat_comm2_2.1[,"pct_cse"])
adf.test(dat_comm2_2.1[,"LightGreenCom2"], k=5)
adf.test(dat_comm2_2.1[,"pct_cse"], k=5)
kpss.test(dat_comm2_2.1[,"LightGreenCom2"])
kpss.test(dat_comm2_2.1[,"pct_cse"])
coint2_2.1 <- dynlm(LightGreenCom2~pct_cse, data=dat_comm2_2.1)
ehat2_2.1 <- resid(coint2_2.1)
adf.test(ehat2_2.1, k=5)

#VAR Comm2_2.1
fitvarcomm2_2.1 <- VAR(dat_comm2_2.1, p=3, type="both")
summary(fitvarcomm2_2.1) # comm2_2.1 pct_cse
causality(fitvarcomm2_2.1, cause = "LightGreenCom2")
#causality(fitvarcomm2_2.1, cause = "")
irf_comm2_2.1_pct_cse <- irf(fitvarcomm2_2.1, impulse = "LightGreenCom2", response = "pct_cse", boot = TRUE)
plot(irf_comm2_2.1_pct_cse)
plot(fevd(fitvarcomm2_2.1))

####model 2_2.4 trust_LightGreenCom2 and pct_cse
dat_comm2_2.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_LightGreenCom2, pct_cse) %>%
  drop_na()
dat_comm2_2.4 <- ts(dat_comm2_2.4)
plot(dat_comm2_2.4)

#Test for stationarity
Acf(dat_comm2_2.4[,"trust_LightGreenCom2"])
Acf(dat_comm2_2.4[,"pct_cse"])
adf.test(dat_comm2_2.4[,"trust_LightGreenCom2"], k=5)
adf.test(dat_comm2_2.4[,"pct_cse"], k=5)
kpss.test(dat_comm2_2.4[,"trust_LightGreenCom2"])
kpss.test(dat_comm2_2.4[,"pct_cse"])
coint2_2.4 <- dynlm(trust_LightGreenCom2~pct_cse, data=dat_comm2_2.4)
ehat2_2.4 <- resid(coint2_2.4)
adf.test(ehat2_2.4, k=5)

#VAR Comm2_2.4
fitvarcomm2_2.4 <- VAR(dat_comm2_2.4, p=3, type="both")
summary(fitvarcomm2_2.4) # comm2_2.4 pct_cse
causality(fitvarcomm2_2.4, cause = "trust_LightGreenCom2")
#causality(fitvarcomm2_2.4, cause = "")
irf_comm2_2.4_pct_cse <- irf(fitvarcomm2_2.4, impulse = "trust_LightGreenCom2", response = "pct_cse", boot = TRUE)
plot(irf_comm2_2.4_pct_cse)
plot(fevd(fitvarcomm2_2.4))

####model 2_2.5 sentiment_LightGreenCom2 and pct_cse
dat_comm2_2.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_LightGreenCom2, pct_cse) %>%
  drop_na()
dat_comm2_2.5 <- ts(dat_comm2_2.5)
plot(dat_comm2_2.5)

#Test for stationarity
Acf(dat_comm2_2.5[,"sentiment_LightGreenCom2"])
Acf(dat_comm2_2.5[,"pct_cse"])
adf.test(dat_comm2_2.5[,"sentiment_LightGreenCom2"], k=5)
adf.test(dat_comm2_2.5[,"pct_cse"], k=5)
kpss.test(dat_comm2_2.5[,"sentiment_LightGreenCom2"])
kpss.test(dat_comm2_2.5[,"pct_cse"])
coint2_2.5 <- dynlm(sentiment_LightGreenCom2~pct_cse, data=dat_comm2_2.5)
ehat2_2.5 <- resid(coint2_2.5)
adf.test(ehat2_2.5, k=5)

#VAR Comm2_2.5
fitvarcomm2_2.5 <- VAR(dat_comm2_2.5, p=3, type="both")
summary(fitvarcomm2_2.5) # comm2_2.5 pct_cse
causality(fitvarcomm2_2.5, cause = "sentiment_LightGreenCom2")
#causality(fitvarcomm2_2.5, cause = "")
irf_comm2_2.5_pct_cse <- irf(fitvarcomm2_2.5, impulse = "sentiment_LightGreenCom2", response = "pct_cse", boot = TRUE)
plot(irf_comm2_2.5_pct_cse)
plot(fevd(fitvarcomm2_2.5))

######retrieve info from main models for tables
varcomm2_2.1G <- fitvarcomm2_2.1[["varresult"]]$pct_cse
varcomm2_2.4G <- fitvarcomm2_2.4[["varresult"]]$pct_cse
varcomm2_2.5G <- fitvarcomm2_2.5[["varresult"]]$pct_cse

######################################################
#############Model 3 LighBlueCom3 pct_cse##############
#####################################################
#######Model 3_2.1 
dat_comm3_2.1<-temp_DV_IV_wt %>% 
  dplyr::select(LighBlueCom3, pct_cse) %>%
  drop_na()
dat_comm3_2.1 <- ts(dat_comm3_2.1)
plot(dat_comm3_2.1)

#Test for stationarity
Acf(dat_comm3_2.1[,"LighBlueCom3"])
Acf(dat_comm3_2.1[,"pct_cse"])
adf.test(dat_comm3_2.1[,"LighBlueCom3"], k=5)
adf.test(dat_comm3_2.1[,"pct_cse"], k=5)
kpss.test(dat_comm3_2.1[,"LighBlueCom3"])
kpss.test(dat_comm3_2.1[,"pct_cse"])
coint3_2.1 <- dynlm(LighBlueCom3~pct_cse, data=dat_comm3_2.1)
ehat3_2.1 <- resid(coint3_2.1)
adf.test(ehat3_2.1, k=5)

#VAR Comm3_2.1
fitvarcomm3_2.1 <- VAR(dat_comm3_2.1, p=3, type="both")
summary(fitvarcomm3_2.1) # comm3_2.1 pct_cse
causality(fitvarcomm3_2.1, cause = "LighBlueCom3")
#causality(fitvarcomm3_2.1, cause = "")
irf_comm3_2.1_pct_cse <- irf(fitvarcomm3_2.1, impulse = "LighBlueCom3", response = "pct_cse", boot = TRUE)
plot(irf_comm3_2.1_pct_cse)
plot(fevd(fitvarcomm3_2.1))

####model 3_2.4 trust_LighBlueCom3 and pct_cse
dat_comm3_2.4<-temp_DV_IV_wt %>% 
  dplyr::select(trust_LighBlueCom3, pct_cse) %>%
  drop_na()
dat_comm3_2.4 <- ts(dat_comm3_2.4)
plot(dat_comm3_2.4)

#Test for stationarity
Acf(dat_comm3_2.4[,"trust_LighBlueCom3"])
Acf(dat_comm3_2.4[,"pct_cse"])
adf.test(dat_comm3_2.4[,"trust_LighBlueCom3"], k=5)
adf.test(dat_comm3_2.4[,"pct_cse"], k=5)
kpss.test(dat_comm3_2.4[,"trust_LighBlueCom3"])
kpss.test(dat_comm3_2.4[,"pct_cse"])
coint3_2.4 <- dynlm(trust_LighBlueCom3~pct_cse, data=dat_comm3_2.4)
ehat3_2.4 <- resid(coint3_2.4)
adf.test(ehat3_2.4, k=5)

#VAR Comm3_2.4
fitvarcomm3_2.4 <- VAR(dat_comm3_2.4, p=3, type="both")
summary(fitvarcomm3_2.4) # comm3_2.4 pct_cse
causality(fitvarcomm3_2.4, cause = "trust_LighBlueCom3")
#causality(fitvarcomm3_2.4, cause = "")
irf_comm3_2.4_pct_cse <- irf(fitvarcomm3_2.4, impulse = "trust_LighBlueCom3", response = "pct_cse", boot = TRUE)
plot(irf_comm3_2.4_pct_cse)
plot(fevd(fitvarcomm3_2.4))

####model 3_2.5 sentiment_LighBlueCom3 and pct_cse
dat_comm3_2.5<-temp_DV_IV_wt %>% 
  dplyr::select(sentiment_LighBlueCom3, pct_cse) %>%
  drop_na()
dat_comm3_2.5 <- ts(dat_comm3_2.5)
plot(dat_comm3_2.5)

#Test for stationarity
Acf(dat_comm3_2.5[,"sentiment_LighBlueCom3"])
Acf(dat_comm3_2.5[,"pct_cse"])
adf.test(dat_comm3_2.5[,"sentiment_LighBlueCom3"], k=5)
adf.test(dat_comm3_2.5[,"pct_cse"], k=5)
kpss.test(dat_comm3_2.5[,"sentiment_LighBlueCom3"])
kpss.test(dat_comm3_2.5[,"pct_cse"])
coint3_2.5 <- dynlm(sentiment_LighBlueCom3~pct_cse, data=dat_comm3_2.5)
ehat3_2.5 <- resid(coint3_2.5)
adf.test(ehat3_2.5, k=5)

#VAR Comm3_2.5
fitvarcomm3_2.5 <- VAR(dat_comm3_2.5, p=3, type="both")
summary(fitvarcomm3_2.5) # comm3_2.5 pct_cse
causality(fitvarcomm3_2.5, cause = "sentiment_LighBlueCom3")
#causality(fitvarcomm3_2.5, cause = "")
irf_comm3_2.5_pct_cse <- irf(fitvarcomm3_2.5, impulse = "sentiment_LighBlueCom3", response = "pct_cse", boot = TRUE)
plot(irf_comm3_2.5_pct_cse)
plot(fevd(fitvarcomm3_2.5))

######retrieve info from main models for tables
varcomm3_2.1G <- fitvarcomm3_2.1[["varresult"]]$pct_cse
varcomm3_2.4G <- fitvarcomm3_2.4[["varresult"]]$pct_cse
varcomm3_2.5G <- fitvarcomm3_2.5[["varresult"]]$pct_cse
```
5. Print significant tables for publication
```{r}
library(sjPlot)
## model these   
# varcom1.5G,
# varcomm1_2.1G,
# varcomm2_2.5G,
# varcomm3_2.1G,
# varcomm3_2.4G,

coef_remove_pctvu<-c("pct_vu.l1", 
               "pct_vu.l2",
               "pct_vu.l3",
               "const")
tab_model(varcomm1.5G, 
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = FALSE,
          rm.terms = coef_remove_pctvu,
          pred.labels = c("Purple Community (#1) Net Sentiment (lag 1)", "Purple Community (#1) Net Sentiment (lag 2)", "Purple Community (#1) Net Sentiment (lag 3)", "trend"),
          dv.labels = c("Vaccination Acceptance"),
                  p.style = c("scientific_stars"))

coef_remove_pctcse<-c("pct_cse.l1", 
                     "pct_cse.l2",
                     "pct_cse.l3",
                     "const")

tab_model(varcomm1_2.1G, 
          show.se = TRUE,
          show.p = FALSE,
          digits = 3,
          wrap.labels = 25,
          collapse.se = FALSE,
          rm.terms = coef_remove_pctcse,
          pred.labels = c("Purple Community (#1) (lag 1)", "Purple Community (#1) (lag 2)", "Purple Community (#1) (lag 3)", "trend"),
          dv.labels = c("Side Effect Concern"),
          p.style = c("scientific_stars"))

tab_model(varcomm2_2.5G, 
          show.se = TRUE,
          show.p = FALSE,
          digits = 3,
          wrap.labels = 25,
          collapse.se = FALSE,
          rm.terms = coef_remove_pctcse,
          pred.labels = c("Light Green Community (#2) Net Sentiment (lag 1)", "Light Green Community (#2) Net Sentiment (lag 2)", "Light Green Community (#2) Net Sentiment (lag 3)", "trend"),
          dv.labels = c("Side Effect Concern"),
          p.style = c("scientific_stars"))

tab_model(varcomm3_2.1G, 
          show.se = TRUE,
          show.p = FALSE,
          digits = 3,
          wrap.labels = 25,
          collapse.se = FALSE,
          rm.terms = coef_remove_pctcse,
          pred.labels = c("Light Blue Community (#3) (lag 1)", "Light Blue Community (#3) (lag 2)", "Light Blue Community (#3) (lag 3)", "trend"),
          dv.labels = c("Side Effect Concern"),
          p.style = c("scientific_stars"))

tab_model(varcomm3_2.4G, 
          show.se = TRUE,
          show.p = FALSE,
          digits = 3,
          wrap.labels = 25,
          collapse.se = FALSE,
          rm.terms = coef_remove_pctcse,
          pred.labels = c("Light Blue Community (#3) Trust (lag 1)", "Light Blue Community (#3) Trust (lag 2)", "Light Blue Community (#3) Trust (lag 3)", "trend"),
          dv.labels = c("Side Effect Concern"),
          p.style = c("scientific_stars"))
```
