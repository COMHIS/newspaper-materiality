library(tidyverse)
library(sqldf)
library(lubridate)

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

newspapers <- read_csv("newspapers-utf8.csv",col_types=cols(ILM_ALPVM=col_date("%d.%m.%Y"),ILM_LOPVM=col_date("%d.%m.%Y"),HIST_ALPVM=col_date("%d.%m.%Y")))
newspapers <- newspapers %>% left_join(read_csv("publication_locations-utf8.csv"),by=c("ISSN"))
newspapers <- newspapers %>% mutate(byear=year(ILM_ALPVM))
newspapers <- newspapers %>% mutate(eyear=coalesce(year(ILM_LOPVM),9999))
npsizes <- read_csv("npsizes2-s.csv",col_types=cols(date='c'))
npsizes <- npsizes %>% mutate(width=width/10,height=height/10) %>% mutate(date = coalesce(as.Date(date,"%Y-%m-%d"),as.Date(date,"%Y-%m")), year = year(date), area = width*height)
npsizes2 <- read_csv("npsizes3-s.csv",col_types=cols(date='c'))
npsizes2 <- npsizes2 %>% mutate(pwidth=width/10,pheight=height/10,parea=pwidth*pheight) %>% select(issueId, page, pwidth,pheight,parea)
npsizes <- npsizes %>% inner_join(npsizes2,by=c("issueId","page"))
rm(npsizes2)
npsoftware <- read_csv("npsoftware-s.csv",col_type=cols(date='c'))
npsoftware <- npsoftware %>% select(-ISSN,-date)
npstyles <- read_csv("npstyles-s.csv.xz",col_type=cols(bold='l',italics='l',underlined='l',date='c'))
npstyles <- npstyles %>% mutate(date = coalesce(as.Date(date,"%Y-%m-%d"),as.Date(date,"%Y-%m")),year = year(date),fraktur=font=='Fraktur',styled=bold | italics | underlined)
npfonts <- npstyles %>% group_by(issueId,page,font) %>% summarise(words=sum(words),chars=sum(chars),area=sum(area)) %>% group_by(issueId,page) %>% mutate(wproportion=words/sum(words),cproportion=chars/sum(chars),aproportion=area/sum(area)) %>% ungroup() %>% group_by(issueId,page) %>% summarise(wproportion=wproportion[which.max(words)],wmfont=font[which.max(words)],cproportion=cproportion[which.max(chars)],cmfont=font[which.max(chars)],aproportion=aproportion[which.max(area)],amfont=font[which.max(area)])
rm(npstyles)

papersizes <- data.frame("type" = c("A0","A1","A2","A3","A4","A5"), "area" = c(814*1189,594*841,420*594,297*420,210*297,148*210))
papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
papersizes <- papersizes %>% mutate(uboundary = coalesce((area + lag(area))/2,9999999999),lboundary=coalesce((area + lead(area))/2,0))

npsizes <- sqldf("select ISSN,issueId,date,page,width,height,pwidth,pheight,parea,year,npsizes.area,type from npsizes left join papersizes on npsizes.area between lboundary and uboundary")
rm(papersizes)
npcolumns <- read_csv("npcolumns-s.csv",col_types=cols(date='c'))
npcolumns <- npcolumns %>% select(-ISSN,-date)

npwordschars <- read_csv("npwordschars-s.csv",col_types=cols(date='c'))
npwordschars <- npwordschars %>% select(-ISSN,-date)

nppagedata <- npsizes %>% inner_join(npsoftware,by=c("issueId","page")) %>% inner_join(npfonts,by=c("issueId","page")) %>% inner_join(npcolumns,by=c("issueId","page")) %>% inner_join(npwordschars,by=c("issueId","page"))  %>% inner_join(newspapers %>% select(ISSN,PAANIMEKE) %>% distinct(),by=c("ISSN")) %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week")) %>% arrange(ISSN,date,issueId,page)
cat("nppagedata",nrow(nppagedata),"npsizes",nrow(npsizes),"npwordschars",nrow(npwordschars),"npcolumns",nrow(npcolumns),"npfonts",nrow(npfonts))
rm(npcolumns,npsizes,npwordschars,npsoftware,npfonts)
inppagedata <- nppagedata %>% group_by(ISSN,year,month,week,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year,issueId)
ytnppagedata <- nppagedata %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,week)

npdatesbetween <- nppagedata %>% group_by(ISSN,date,issueId) %>% summarise() %>% group_by(ISSN) %>% arrange(ISSN,date) %>% mutate(datesbetween=date-lag(date)) %>% ungroup() %>% select(-ISSN,-date)
nppages <- nppagedata %>%group_by(issueId) %>% summarise(pages=n()) %>% ungroup()
npissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("issueId")) %>% group_by(ISSN,year,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=first(datesbetween),pages=n(),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week")) %>% arrange(ISSN,date,issueId)
ytnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnpissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=first(pages),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),words=mean(words),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,week)
rm(nppages,npdatesbetween)

newspapers <- newspapers %>% left_join(npissuedata %>% group_by(ISSN) %>% summarise(fyear=min(year),lyear=max(year)),by=c("ISSN")) %>% ungroup()

save(list = ls(all.names = TRUE),file="app.RData",compress="xz")