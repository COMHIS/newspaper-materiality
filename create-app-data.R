library(IRanges) # BiocManager::install("IRanges")
library(lubridate)
library(fuzzyjoin)
library(tidyverse)

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

newspapers <- read_csv("newspapers-utf8.csv",col_types=cols(M_ARTIKKELI=col_character(),PAAJULKAISUN_ISSN=col_character(),PAAJULKAISUN_NIMI=col_character(),ALKUKIEL_JULK=col_character(),ILM_ALPVM=col_date("%d.%m.%Y"),ILM_LOPVM=col_date("%d.%m.%Y"),HIST_ALPVM=col_date("%d.%m.%Y"),PJ_ARTIKKELI=col_character()))
newspapers <- newspapers %>% left_join(read_csv("publication_locations-utf8.csv"),by=c("ISSN"))
newspapers <- newspapers %>% mutate(byear=year(ILM_ALPVM))
newspapers <- newspapers %>% mutate(eyear=coalesce(year(ILM_LOPVM),9999))

npissues <- read_csv("npissues-s.csv",col_types=cols(date='c')) %>% mutate(date = coalesce(as.Date(date,"%Y-%m-%d"),as.Date(date,"%Y-%m")), year = year(date))

npsoftware <- read_csv("npsoftware-s.csv")
npstyles <- read_csv("npstyles-s.csv.xz",col_type=cols(font='c',bold='l',italics='l',underlined='l'))
npstyles <- npstyles %>% mutate(fraktur=font=='Fraktur',styled=bold | italics | underlined)
npfonts <- npstyles %>% group_by(issueId,page,font) %>% summarise(words=sum(words),chars=sum(chars),area=sum(area)) %>% group_by(issueId,page) %>% mutate(wproportion=words/sum(words),cproportion=chars/sum(chars),aproportion=area/sum(area)) %>% ungroup() %>% group_by(issueId,page) %>% summarise(wproportion=wproportion[which.max(words)],wmfont=font[which.max(words)],cproportion=cproportion[which.max(chars)],cmfont=font[which.max(chars)],aproportion=aproportion[which.max(area)],amfont=font[which.max(area)])
rm(npstyles)

papersizes <- tibble("type" = c("A0","A1","A2","A3","A4","A5"), "width" = c(814,594,420,297,210,148), "height" = c(1189,841,594,420,297,210))
# papersizes$type <- factor(papersizes$type, levels = c("A5","A4","A3","A2","A1","A0"))
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

inppagedata <- nppagedata %>% group_by(ISSN,year,month,week,date,issueId) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=mean(area),parea=mean(parea),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,year,issueId)
ytnppagedata <- nppagedata %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=mean(area),parea=mean(parea),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,year)
mtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=mean(area),parea=mean(parea),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,month)
wtnppagedata <- nppagedata %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% summarise(PAANIMEKE=first(PAANIMEKE),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),type=Mode(type),words=mean(words),area=mean(area),parea=mean(parea),chars=mean(chars)) %>% ungroup() %>% arrange(ISSN,week)

npdatesbetween <- nppagedata %>% group_by(ISSN,date,issueId) %>% summarise() %>% group_by(ISSN) %>% arrange(ISSN,date) %>% mutate(datesbetween=date-lag(date)) %>% ungroup() %>% select(-ISSN,-date)
nppages <- nppagedata %>% group_by(issueId) %>% summarise(pages=n()) %>% ungroup()
npissuedata <- nppagedata %>% 
  inner_join(npdatesbetween,by=c("issueId")) %>% 
  group_by(issueId) %>% 
  summarise(
    ISSN=first(ISSN),
    year=first(year),
    date=first(date),
    month=first(month),
    week=first(week),
    PAANIMEKE=first(PAANIMEKE),
            datesbetween=first(datesbetween),
            pages=n(),
            type=Mode(type),
            height=mean(height),
            width=mean(width),
            area=mean(area),
            pheight=mean(pheight),
            pwidth=mean(pwidth),
            parea=mean(parea),
            wmodecols=Mode(wmodecols),
            modecols=Mode(modecols),
            wmediancols=median(wmediancols),
            mediancols=median(mediancols),
            presoftware=Mode(presoftware),
            preversion=Mode(preversion),
            ocrsoftware=Mode(ocrsoftware),
            ocrversion=Mode(ocrversion),
            wmfont=Mode(wmfont),
            wproportion=mean(wproportion),
            cmfont=Mode(cmfont),
            cproportion=mean(cproportion),
            amfont=Mode(amfont),
            aproportion=mean(aproportion),
            words=mean(words),
            chars=mean(chars),
            twords=sum(words),
            tchars=sum(chars)) %>% 
  ungroup() %>%
  # mutate(month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% 
  arrange(ISSN,date,issueId)
npissuedata <- npissuedata %>% 
  group_by(ISSN) %>% 
  arrange(date,issueId) %>% 
  mutate(
    anomalies = 
      (wmfont!=lag(wmfont)) + 
      (pages!=lag(pages)) + 
      (
        !is.na(datesbetween) & 
        !is.na(lag(datesbetween)) & 
        datesbetween!=lag(datesbetween)
      ) + 
      (abs(area-lag(area))>lag(area)/2) + 
      (wmodecols!=lag(wmodecols)) + 
      (abs(round(parea/chars-lag(parea)/lag(chars)))>lag(parea)/lag(chars)/4)
  ) %>% 
  ungroup()
npanomalies <- npissuedata %>% select(issueId,anomalies)
npweeklytext <- npissuedata %>% group_by(ISSN,week=floor_date(date,"week",week_start=1)) %>% summarise(twords=sum(twords),tchars=sum(tchars))
ytnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npanomalies,by=c("issueId")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),parea=mean(parea),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords),tchars=mean(tchars),anomalies=mean(anomalies)) %>% ungroup() %>% arrange(ISSN,year)
mtnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npanomalies,by=c("issueId")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month")) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),parea=mean(parea),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords),tchars=mean(tchars),anomalies=mean(anomalies)) %>% ungroup() %>% arrange(ISSN,month)
wtnpissuedata <- nppagedata %>% inner_join(npweeklytext,by=c("ISSN","week")) %>% inner_join(npanomalies,by=c("issueId")) %>% inner_join(npdatesbetween,by=c("issueId")) %>% inner_join(nppages,by=c("issueId")) %>% group_by(ISSN,year,month=floor_date(date, "month"),week=floor_date(date, "week",week_start=1)) %>% summarise(PAANIMEKE=first(PAANIMEKE),datesbetween=Mode(datesbetween),pages=Mode(pages),type=Mode(type),area=mean(area),parea=mean(parea),wmodecols=Mode(wmodecols),wmfont=Mode(wmfont),words=mean(words),chars=mean(chars),twords=mean(twords),tchars=mean(tchars),anomalies=mean(anomalies)) %>% ungroup() %>% arrange(ISSN,week)

rm(nppages,npdatesbetween,npweeklytext,npanomalies)

# remove currently unused stuff so the data frame fits in GitHub without LFS
nppagedata <- nppagedata %>% select(-pwidth,-pheight,-height,-width,-wmediancols,-modecols,-mediancols,-presoftware,-preversion,-ocrsoftware,-ocrversion,-wproportion,-cmfont,-cproportion,-amfont,-aproportion)
npissuedata <- npissuedata %>% select(-pwidth,-pheight,-height,-width,-wmediancols,-modecols,-mediancols,-presoftware,-preversion,-ocrsoftware,-ocrversion,-wproportion,-cmfont,-cproportion,-amfont,-aproportion)

newspapers <- newspapers %>% left_join(npissuedata %>% group_by(ISSN) %>% summarise(fyear=min(year),lyear=max(year)),by=c("ISSN")) %>% ungroup()
newspapers <- newspapers %>% inner_join(npissuedata %>% group_by(ISSN) %>% summarise(ryears=n_distinct(year)),by=c("ISSN"))

rm(Mode)
save(list = ls(all.names = TRUE),file="app/app.RData")

# npboxes <- read_csv("npboxes-s.csv.xz",col_types=col(issueId='i',page='i',x1='i',y1='i',x2='i',y2='i'))
# data <- npboxes %>% filter(page==1) %>% inner_join(npsizes %>% select(type,page,issueId) %>% inner_join(npissues %>% filter(ISSN=="0356-0724"),by=c("issueId")) %>% select(-ISSN,-year,-date),by=c("issueId","page"))
# ggplot(data %>% group_by(type) %>% sample_n(20000,replace=TRUE) %>% ungroup())  + geom_rect(data=papersizes,aes(xmin=0,ymin=0,xmax=width*10,ymax=height*10),fill="gray") + geom_rect(aes(xmin=x1,ymin=y1,xmax=x2,ymax=y2),fill="red",alpha=0.005) + facet_wrap(~type) + coord_equal() + scale_y_continuous(trans = "reverse")