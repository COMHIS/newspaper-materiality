library(tidyverse)
library(ggplot2)
library(sqldf)
library(viridis)
library(plotly)
library(shiny)
library(lubridate)

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

newspapers <- read.csv("newspapers-utf8.csv", na.strings=c(""), stringsAsFactors = FALSE)
newspapers <- newspapers %>% left_join(read.csv("publication_locations-utf8.csv", na.strings=c(""), stringsAsFactors = FALSE),by=c("ISSN"))
newspapers <- newspapers %>% mutate(size = replace(KOKO, nchar(as.character(KOKO))>2, NA))
newspapers <- newspapers %>% mutate(byear=as.numeric(gsub("\\d\\d\\.","",ILM_ALPVM)))
newspapers <- newspapers %>% mutate(eyear=coalesce(as.numeric(gsub("\\d\\d\\.","",ILM_LOPVM)),9999))
npsizes <- read.csv("npsizes.csv", stringsAsFactors = FALSE)
npsizes <- npsizes %>% mutate(year = as.numeric(gsub("\\d\\d\\.","",date)), area = width*height, date = as.Date(date,"%d.%m.%Y"))

papersizes <- data.frame("type" = c("A0","A1","A2","A3","A4","A5"), "area" = c(814*1189,594*841,420*594,297*420,210*297,148*210))
papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
papersizes <- papersizes %>% mutate(uboundary = coalesce((area + lag(area))/2,9999999999),lboundary=coalesce((area + lead(area))/2,0))

npsizes <- sqldf("select ISSN,issueId,date,page,width,height,year,npsizes.area,type from npsizes left join papersizes on npsizes.area between lboundary and uboundary")

npcolumns <- read.csv("npcolumns.csv", stringsAsFactors = FALSE)
npcolumns <- npcolumns %>% mutate(year = as.numeric(substr(date,1,4)),date = as.Date(date,"%Y-%m-%d"))

npwordschars <- read.csv("npwordschars.csv", stringsAsFactors = FALSE)
npwordschars <- npwordschars %>% mutate(year = as.numeric(substr(date,1,4)),date = as.Date(date,"%Y-%m-%d"))

nppagedata <- npsizes %>% inner_join(npcolumns,by=c("ISSN","issueId","date","page","year")) %>% inner_join(npwordschars,by=c("ISSN","issueId","date","page","year"))  %>% inner_join(newspapers %>% select(ISSN,PAANIMEKE) %>% distinct(),by=c("ISSN")) %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week")) %>% arrange(ISSN,date,issueId,page)
cat("nppagedata",nrow(nppagedata),"npsizes",nrow(npsizes),"npwordschars",nrow(npwordschars),"npcolumns",nrow(npcolumns))
rm(npcolumns,npsizes,npwordschars,papersizes)
inppagedata <- nppagedata %>% group_by(ISSN,year,month,week,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year,issueId)
ytnppagedata <- nppagedata %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,week)

npdatesbetween <- nppagedata %>% group_by(ISSN,date) %>% summarise() %>% group_by(ISSN) %>% arrange(ISSN,date) %>% mutate(datesbetween=date-lag(date)) %>% ungroup() %>% arrange(ISSN,date)
nppages <- nppagedata %>%group_by(ISSN,issueId) %>% summarise(pages=n()) %>% ungroup()
npissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("ISSN","date")) %>% group_by(ISSN,year,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=first(datesbetween),pages=n(),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week")) %>% arrange(ISSN,date,issueId)
ytnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("ISSN","date")) %>% inner_join(nppages,by=c("ISSN","issueId")) %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("ISSN","date")) %>% inner_join(nppages,by=c("ISSN","issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("ISSN","date")) %>% inner_join(nppages,by=c("ISSN","issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,week)
rm(nppages,npdatesbetween)

newspapers <- newspapers %>% left_join(npissuedata %>% group_by(ISSN) %>% summarise(fyear=min(year),lyear=max(year)),by=c("ISSN")) %>% ungroup()

save(list = ls(all.names = TRUE),file="app/app.RData")