library(tidyverse)
library(lubridate)
library(fuzzyjoin)

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

npissues <- read_csv("npissues-s.csv",col_types=cols(date='c')) %>% mutate(date = coalesce(as.Date(date,"%Y-%m-%d"),as.Date(date,"%Y-%m")), year = year(date))

npsoftware <- read_csv("npsoftware-s.csv")
npstyles <- read_csv("npstyles-s.csv.xz",col_type=cols(font=col_factor(NULL),bold='l',italics='l',underlined='l',date='c'))
npstyles <- npstyles %>% mutate(date = coalesce(as.Date(date,"%Y-%m-%d"),as.Date(date,"%Y-%m")),year = year(date),fraktur=font=='Fraktur',styled=bold | italics | underlined)
npfonts <- npstyles %>% group_by(issueId,page,font) %>% summarise(words=sum(words),chars=sum(chars),area=sum(area)) %>% group_by(issueId,page) %>% mutate(wproportion=words/sum(words),cproportion=chars/sum(chars),aproportion=area/sum(area)) %>% ungroup() %>% group_by(issueId,page) %>% summarise(wproportion=wproportion[which.max(words)],wmfont=font[which.max(words)],cproportion=cproportion[which.max(chars)],cmfont=font[which.max(chars)],aproportion=aproportion[which.max(area)],amfont=font[which.max(area)])
rm(npstyles)

papersizes <- tibble("type" = c("A0","A1","A2","A3","A4","A5"), "width" = c(814,594,420,297,210,148), "height" = c(1189,841,594,420,297,210))
papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
papersizes <- papersizes %>% mutate(area=width*height,uboundary = area,lboundary=lead(area))
papersizes$uboundary[1]=99999999 # Inf
papersizes$lboundary[6]=0

npsizes <- read_csv("npsizes2-s.csv")
npsizes <- npsizes %>% mutate(width=width/10,height=height/10) %>% mutate(area = width*height)
npsizes2 <- read_csv("npsizes3-s.csv")
npsizes2 <- npsizes2 %>% mutate(pwidth=width/10,pheight=height/10,parea=pwidth*pheight) %>% select(issueId, page, pwidth,pheight,parea)
npsizes <- npsizes %>% inner_join(npsizes2,by=c("issueId","page"))
rm(npsizes2)

npsizes <- npsizes %>% interval_inner_join(papersizes %>% select(type,lboundary,uboundary),by=c("area"="lboundary","area"="uboundary")) %>% select(-lboundary,-uboundary)
rm(papersizes)

npcolumns <- read_csv("npcolumns-s.csv")

npwordschars <- read_csv("npwordschars-s.csv")

nppagedata <- npsizes %>% inner_join(npsoftware,by=c("issueId","page")) %>% inner_join(npfonts,by=c("issueId","page")) %>% inner_join(npcolumns,by=c("issueId","page")) %>% inner_join(npwordschars,by=c("issueId","page")) %>% inner_join(npissues,by=c("issueId")) %>% inner_join(newspapers %>% select(ISSN,PAANIMEKE) %>% distinct(),by=c("ISSN")) %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% arrange(ISSN,date,issueId,page)
cat("nppagedata",nrow(nppagedata),"npsizes",nrow(npsizes),"npwordschars",nrow(npwordschars),"npcolumns",nrow(npcolumns),"npfonts",nrow(npfonts))
rm(npcolumns,npsizes,npwordschars,npsoftware,npfonts)

inppagedata <- nppagedata %>% group_by(ISSN,year,month,week,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year,issueId)
ytnppagedata <- nppagedata %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=sum(area),chars=sum(chars)) %>% ungroup() %>% arrange(ISSN,week)

npdatesbetween <- nppagedata %>% group_by(ISSN,date,issueId) %>% summarise() %>% group_by(ISSN) %>% arrange(ISSN,date) %>% mutate(datesbetween=date-lag(date)) %>% ungroup() %>% select(-ISSN,-date)
nppages <- nppagedata %>% group_by(issueId) %>% summarise(pages=n()) %>% ungroup()
npissuedata <- nppagedata %>% inner_join(npdatesbetween,by=c("issueId")) %>% group_by(ISSN,year,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=first(datesbetween),pages=n(),type=Mode(type),height=mean(height),width=mean(width),area=mean(area),pheight=mean(pheight),pwidth=mean(pwidth),parea=mean(parea),wmodecols=Mode(wmodecols),modecols=Mode(modecols),wmediancols=median(wmediancols),mediancols=median(mediancols),presoftware=Mode(presoftware),preversion=Mode(preversion),ocrsoftware=Mode(ocrsoftware),ocrversion=Mode(ocrversion),wmfont=Mode(wmfont),wproportion=mean(wproportion),cmfont=Mode(cmfont),cproportion=mean(cproportion),amfont=Mode(amfont),aproportion=mean(aproportion),words=mean(words),chars=mean(chars),twords=sum(words)) %>% ungroup() %>% mutate(month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% arrange(ISSN,date,issueId)
npweeklytext <- npissuedata %>% group_by(ISSN,week=floor_date(date,"week",week_start=1)) %>% summarise(twords=sum(twords))
ytnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords)) %>% ungroup() %>% arrange(ISSN,year)
mtnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords)) %>% ungroup() %>% arrange(ISSN,month)
wtnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords)) %>% ungroup() %>% arrange(ISSN,week)

rm(nppages,npdatesbetween,npweeklytext)

# remove currently unused stuff so the data frame fits in GitHub without LFS
nppagedata <- nppagedata %>% select(-parea,-pwidth,-pheight,-height,-width,-wmediancols,-modecols,-mediancols,-presoftware,-preversion,-ocrsoftware,-ocrversion,-wproportion,-cmfont,-cproportion,-amfont,-aproportion)
npissuedata <- npissuedata %>% select(-parea,-pwidth,-pheight,-height,-width,-wmediancols,-modecols,-mediancols,-presoftware,-preversion,-ocrsoftware,-ocrversion,-wproportion,-cmfont,-cproportion,-amfont,-aproportion)

newspapers <- newspapers %>% left_join(npissuedata %>% group_by(ISSN) %>% summarise(fyear=min(year),lyear=max(year)),by=c("ISSN")) %>% ungroup()

rm(Mode)
save(list = ls(all.names = TRUE),file="app/app.RData")

# data <- npboxes %>% filter(page==1) %>% inner_join(npsizes %>% select(type,page,issueId) %>% inner_join(npissues %>% filter(ISSN=="0356-0724"),by=c("issueId")) %>% select(-ISSN,-year,-date),by=c("issueId","page"))
# ggplot(data %>% group_by(type) %>% sample_n(20000,replace=TRUE) %>% ungroup())  + geom_rect(data=papersizes,aes(xmin=0,ymin=0,xmax=width*10,ymax=height*10),fill="gray") + geom_rect(aes(xmin=x1,ymin=y1,xmax=x2,ymax=y2),fill="red",alpha=0.005) + facet_wrap(~type) + coord_equal() + scale_y_continuous(trans = "reverse")