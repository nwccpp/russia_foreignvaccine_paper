---
title: "Russia Foreign Vaccines Figures"
author: "Ayse D Lokmanoglu"
date: "8/1/2022"
output: github_document
---

```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
```
### Supplementary Code for:
### 
#### Authors: Erik Nisbet, PhD; Olga Kamenchuk, PhD; and Ayse D. Lokmanoglu, PhD

1. Load libraries
```{r warning=FALSE, message=FALSE}
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
library(readr)
```

#### Load the dataset
```{r }
mydataforfigures <- read.csv("https://raw.githubusercontent.com/nwccpp/russia_foreignvaccine_paper/main/datasets/Russia_FV_DataforFigures.csv")
### remove the X that comes with loading CSV
mydataforfigures <- dplyr::select(mydataforfigures, -X)
glimpse(mydataforfigures)
```
### Figure 1 Engagement and Article Volume
```{r }
mydata_long<-mydataforfigures %>%
  mutate(engagement = fb_data.total_engagement_count,
         forcount = 1) %>%
  dplyr::select(date, forcount, engagement) %>%
  pivot_longer(!date,
               names_to = "Variable",
               values_to = "Volume",
               values_drop_na = TRUE) %>%
  group_by(date, Variable) %>%
  summarize(sum = sum(Volume)) %>%
  filter(!date=='2022-01-01') %>%
  ungroup() %>%
  mutate(date=ymd(date))
head(mydataforfigures)
```
```{r }
labels_volume <- c(
  forcount = "Article Volume",
  engagement = "Facebook Engagement Volume")

ggplot(mydata_long, aes(x=date, y=sum)) + 
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
  theme(axis.text.x=element_text(size=8, angle=60, hjust=1, family="sans"),
        axis.text.y=element_text(size=8, family="sans"),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=10, family="sans"),
        legend.position="none", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())
```
### Figure S3.1 Sample Size
```{r }
UMDIndicatorsRussia_vaccinated_appointment_or_accept <- read.csv("https://raw.githubusercontent.com/nwccpp/russia_foreignvaccine_paper/main/datasets/UMDIndicatorsRussia_vaccinated_appointment_or_accept.csv")
UMDIndicatorsRussia_concernedsideeffects_052022 <- read.csv("https://raw.githubusercontent.com/nwccpp/russia_foreignvaccine_paper/main/datasets/UMDIndicatorsRussia_concernedsideeffects_052022.csv")

pct_vu <- dplyr::select(UMDIndicatorsRussia_vaccinated_appointment_or_accept, date, sample_size) %>%
  mutate(variable = c("Vaccination Acceptance"))

pct_cse <- dplyr::select(UMDIndicatorsRussia_concernedsideeffects_052022, date, sample_size) %>%
  mutate(variable = c("Side Effect Concern"))

UMD_sample <- full_join(pct_vu, pct_cse) 
UMD_sample$date <- ymd(UMD_sample$date)
glimpse(UMD_sample)
```
```{r }
UMD_sample %>%
  ggplot(aes(x=date, y=sample_size)) +
  geom_line(aes(color = variable)) +
  scale_x_date(date_breaks="15 days", date_labels = "%d-%b-%Y") +
  scale_y_continuous(label=comma) +
  scale_color_manual(values = wesanderson::wes_palette('Moonrise2')) +
  labs(title = "UMD Survey Sample Size",
       x="", 
       y="")+
  facet_wrap(~variable, 
             nrow=2,
             scales = "free_y") +
  theme_wsj(color="white") +
  theme(plot.title = element_text(family="sans", size = 10),
        axis.text.x=element_text(size=8, angle = 45, hjust=1, family="sans"),
        axis.text.y=element_text(family="sans", size = 8),
        axis.title.x=element_text(vjust=-0.25, size=10, family="sans"),
        axis.title.y=element_text(vjust=-0.50, size=10, family="sans"),
        legend.position="none", 
        legend.box="vertical", 
        legend.title = element_blank(),
        legend.margin=margin(),
        legend.key = element_rect(fill=NA), 
        legend.background = element_rect(fill=NA),
        legend.box.background = element_blank())
```
### Figure 3 Significant dependent variables and independent variables over time
```{r}

temp_DV_IV_wt <- read.csv("https://raw.githubusercontent.com/nwccpp/russia_foreignvaccine_paper/main/datasets/Russia_Fv_k65_WT_IV_DV_071222.csv")
### remove the X that comes with loading CSV
temp_DV_IV_wt <- dplyr::select(temp_DV_IV_wt, -X)

```
```{r }
## pivot longer
variables_long<-temp_DV_IV_wt %>% 
  dplyr::select(date,
                PurpleCom1,
                sentiment_PurpleCom1,
                sentiment_LightGreenCom2,
                LighBlueCom3,
                trust_LighBlueCom3,
                pct_vu,
                pct_cse) %>%
  mutate(date = ymd(date)) %>%
  rename('Purple Community(#1) Cluster Prominence' = 'PurpleCom1',
         'Purple Community(#1) Net Sentiment' = 'sentiment_PurpleCom1',
         'Light Green Community(#2) Net Sentiment' = 'sentiment_LightGreenCom2',
         'Light Blue Community(#3) Cluster Prominence' = 'LighBlueCom3',
         'Light Blue Community (#3) Trust Sentiment' = 'trust_LighBlueCom3',
         'Vaccine Acceptance' = 'pct_vu',
         'Side Effect Concern' = 'pct_cse') %>%
  pivot_longer(!date, names_to = "variable", values_to = "score")
glimpse(variables_long)
```

```{r}
## plot it
ggplot(variables_long, aes(x=date, y=score)) + 
  geom_line(aes(color=variable)) +
  facet_wrap(~variable, 
             nrow = 4,
             ncol = 2,
             scales="free_y") +
  scale_color_manual(values = NatParksPalettes::natparks.pals(name="DeathValley",n=7,type="discrete")) +
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
```
### Figure 4-6 IRF Functions
```{r }
plot_all <- read.csv("https://raw.githubusercontent.com/nwccpp/russia_foreignvaccine_paper/main/datasets/IRF_Plot_All.csv")
### remove the X that comes with loading CSV
plot_all <- dplyr::select(plot_all, -X)
plot_all_purplecom1 <- plot_all[1:22,]
plot_all_lightgreencom2 <- plot_all[23:33,]
plot_all_lightbluecom3 <- plot_all[34:55,]
```
### Figure 4 IRF Function for Purple Community
```{r }
#create a labeler
to_string_purple <- as_labeller(c("sentiment_PurpleCom1pct_vu" = "Purple Com (#1) Net Sentiment w/ Vaccine Acceptance", 
                                  "PurpleCom1pct_cse" = "Purple Com (#1) Cluster Prominence w/ Side Effect Worry"))

## plot it 
ggplot(plot_all_purplecom1, aes(x = Theme, y = irf)) +
  geom_line(aes(x = Theme, y = irf), color = "red2", size = 1.5) +
  geom_line(aes(x = Theme, y = Upper) , linetype = "dashed")+
  geom_line(aes(x = Theme, y = Lower), linetype = "dashed")+
  geom_hline(aes(yintercept=0),
             linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) +
  facet_wrap(.~group,
             labeller = to_string_purple,
             nrow = 2,
             scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "time(days)",
       y= " ",
       title = "") +
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
```
### Figure 5 IRF Function for Light Green Community
```{r }
## create labeler
to_string_lightgreen <- as_labeller(c(
  "sentiment_LightGreenCom2pct_cse" = "Light Green Community (#2) Net Sentiment w/ Side Effect Worry"))

## plot it
ggplot(plot_all_lightgreencom2, aes(x = Theme, y = irf)) +
  geom_line(aes(x = Theme, y = irf), color = "red2", size = 1.5) +
  geom_line(aes(x = Theme, y = Upper) , linetype = "dashed")+
  geom_line(aes(x = Theme, y = Lower), linetype = "dashed")+
  geom_hline(aes(yintercept=0),
             linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) +
  facet_wrap(.~group,
             labeller = to_string_lightgreen,
             nrow = 1,
             scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "time(days)",
       y= " ",
       title = "") +
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
```
### Figure 6 IRF Function for Light Blue Community
```{r }
### create labeler
to_string_lightblue<- as_labeller(c(
  "LighBlueCom3pct_cse" = "Light Blue Com (#3) Cluster Prominence w/ Side Effect Worry",
  "trust_LighBlueCom3pct_cse" = "Light Blue Com (#3) Trust Cluster Prominence w/ Side Effect Worry"))

## plot it
ggplot(plot_all_lightbluecom3, aes(x = Theme, y = irf)) +
  geom_line(aes(x = Theme, y = irf), color = "red2", size = 1.5) +
  geom_line(aes(x = Theme, y = Upper) , linetype = "dashed")+
  geom_line(aes(x = Theme, y = Lower), linetype = "dashed")+
  geom_hline(aes(yintercept=0),
             linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) +
  facet_wrap(.~group,
             labeller = to_string_lightblue,
             nrow = 2,
             scales = "free_y") +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(x = "time(days)",
       y= " ",
       title = "") +
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
```
