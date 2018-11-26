library(tidyverse)
library(ggplot2)
library(sqldf)
library(viridis)
library(plotly)
library(shiny)
library(lubridate)
library(DT)

load("app.RData")
newspapers <- newspapers %>% inner_join(npissuedata %>% group_by(ISSN) %>% summarise(ryears=n_distinct(year)),by=c("ISSN"))
ui <- fluidPage(
  h3("Finnish newspapers materiality explorer"),
  "by",a(href="http://iki.fi/eetu.makela","Eetu Mäkelä")," and the ",a(href="http://comhis.github.io","COMHIS research group"),
  br(),
  br(),
  fluidRow(
    column(2,sliderInput("years", "Years",min = min(newspapers$fyear,na.rm = TRUE), max = max(newspapers$lyear,na.rm = TRUE), value = c(min(newspapers$fyear,na.rm = TRUE),max(newspapers$lyear,na.rm = TRUE)),sep = "")),
    column(2,sliderInput("ryears", "Paper life (years)",min = 1, max=max(newspapers$ryears), value=c(1,max(newspapers$ryears)))),
    column(2,selectInput("languages","Languages",unique((newspapers %>% filter(ISSN %in% npissuedata$ISSN))$KIELI),multiple=TRUE,selected=c("fin","swe"))),
    column(2,selectInput("towns","Towns",unique((newspapers %>% filter(ISSN %in% npissuedata$ISSN))$KAUPUNKI_NORM),multiple=TRUE)),
    column(2,selectizeInput("issns","Titles",NULL,multiple=TRUE)),
    column(2,sliderInput("proportionFilter", "Filter props below",min = 0.0, max = 1.0, value = 0.0)),
    column(2,selectInput("by","Unit of obs",c("title","issue","page"),selected="title")),
    column(2,selectInput("aby","Calc props by",c("year","month","week"),selected="year"))
  ),
  textOutput("newspaperCount"),
  tabsetPanel(type = "tabs",
    tabPanel("Overview",
             checkboxGroupInput("plots", "Plots:",
                                inline = TRUE,
                                choices = c(
                                  "Absolute count" = "overview",
                                  "Text/week" = "textperweek",
                                  "Issue gap" = "daysbetween",
                                  "Pages" = "pages",
                                  "Columns" = "columns",
                                  "Page size" = "pagesize",
                                  "Text density" = "textdensity",
                                  "Word length" = "wordlength",
                                  "Text/page" = "textperpage",
                                  "Font" = "font"),
                                selected = c("overview","textperweek","daysbetween","pages","pagesize","textdensity")),
             checkboxInput("filterOutliers", "Filter outliers",value=TRUE),
             plotlyOutput("plot",height="950px")),
    tabPanel("Materiality categories",
             checkboxGroupInput("groupBy", "Group by:",
                                inline = TRUE,
                                choices = c("Number of pages" = "pages",
                                  "Dates between issues" = "datesbetween",
                                  "Page type" = "type",
                                  "Columns" = "wmodecols",
                                  "Main font" = "wmfont"
                                ),
                                selected = c("type")),
             sliderInput("numCategories","Number of categories to show", min = 1, max=32, value = 5),
             h4("Overall proportions"),
             plotlyOutput("materialityGraph1",height="100px"),
             h4("Proportions through time"),
             plotlyOutput("materialityGraph2",height="400px"),
             h4("Full table of overall propoprtions"),
             DTOutput("materialityCategories")),
    tabPanel("Anomalous issues",
             h4("Legend:"),
             fluidRow(
              column(4,"an=number of anomalies"),
              column(4,"pd=difference from usual page count"), 
              column(4,"p=page count for this issue"),
              column(4,"gp=usual number of pages"),
              column(4,"dd=difference in dates between issues"), 
              column(4,"db=dates between issues for this issue"),
              column(4,"gdb=usual difference in dates between issues"),
              column(4,"ad=difference in surface area"),
              column(4,"pt=page type for this issue"),
              column(4,"gpt=usual page type"),
              column(4,"f=main font for this issue"),
              column(4,"gf=usual main font"),
              column(4,"cd=difference in number of columns"), 
              column(4,"c=number of columns in this issue"),
              column(4,"gc=usual number of columns"),
              column(4,"wd=difference in word counts"), 
              column(4,"w=word count per page for this issue"),
              column(4,"gw=usual word count per page")),
             br(),
             br(),
             DTOutput("ianomalies")),
    tabPanel("Anomalous pages",
             h4("Legend:"),
             fluidRow(
               column(4,"an=number of anomalies"),
               column(4,"po=offset from usual page count (if positive, represents an added page)"), 
               column(4,"gp=usual number of pages"),
               column(4,"dd=difference in dates between issues"), 
               column(4,"db=dates between issues for this issue"),
               column(4,"gdb=usual difference in dates between issues"),
               column(4,"ad=difference in surface area"),
               column(4,"pt=page type for this page"),
               column(4,"gpt=usual page type"),
               column(4,"f=main font for this page"),
               column(4,"gf=usual main font"),
               column(4,"cd=difference in number of columns"), 
               column(4,"c=number of columns on this page"),
               column(4,"gc=usual number of columns"),
               column(4,"wd=difference in word counts"), 
               column(4,"w=word count for this page"),
               column(4,"gw=usual word count per page")),
             br(),
             br(),
             DTOutput("panomalies"))
  )
)
# Server logic
server <- function(input, output, session) {
  fnewspapers1 <- reactive({
    fn <- newspapers %>% filter(lyear>=input$years[1],fyear<=input$years[2],ryears>=input$ryears[1],ryears<=input$ryears[2])
    if (length(input$languages)>0) fn <- fn %>% filter(KIELI %in% input$languages)
    if (length(input$towns)>0) fn <- fn %>% filter(KAUPUNKI_NORM %in% input$towns)
    fn
  })
  fnpids <- reactive({
    newspaperIDs = fnewspapers1()$ISSN
    names(newspaperIDs) <- paste(fnewspapers1()$PAANIMEKE,' (',fnewspapers1()$ISSN,')')
    newspaperIDs
  })
  observe({
    updateSelectizeInput(session, 'issns', choices = fnpids(), server = TRUE)
  })
  fnewspapers <- reactive({
    fn <- fnewspapers1()
    if (length(input$issns)>0) fn <- fn %>% filter(ISSN %in% input$issns)
    fn$ISSN
  })
  output$newspaperCount = renderText({paste("Number of newspapers:",length(fnewspapers()))})
  bnpissuedata <- reactive({
    if (input$by=="title") switch(input$aby,
                                  year = ytnpissuedata,
                                  month = mtnpissuedata,
                                  week = wtnpissuedata)
    else npissuedata
  })
  bnppagedata <- reactive({
    switch(input$by,
           title = switch(input$aby,
                          year = ytnppagedata,
                          month = mtnppagedata,
                          week = wtnppagedata),
           issue = inppagedata,
           page = nppagedata)
  })
  fnpissuedata <- reactive({ 
    bnpissuedata() %>% 
    filter(ISSN %in% fnewspapers()) %>% 
    filter(year>=input$years[1],year<=input$years[2])
  })
  fnpissuedata2 <- reactive({
    npissuedata %>% filter(ISSN %in% fnewspapers()) %>% 
    filter(year>=input$years[1],year<=input$years[2])
  })
  fnppagedata <- reactive({ 
    bnppagedata() %>% 
    filter(ISSN %in% fnewspapers()) %>% 
    filter(year>=input$years[1],year<=input$years[2])
  })
  fnppagedata2 <- reactive({
    nppagedata %>% filter(ISSN %in% fnewspapers()) %>% 
      filter(year>=input$years[1],year<=input$years[2])
  })
  p1data <- reactive({
    if (length(unique(fnpissuedata()$ISSN))==1 && input$by !="title") {
      tmp <- npissuedata %>% filter(ISSN %in% fnewspapers())
      if (input$filterOutliers) tmp <- tmp %>% filter(datesbetween<=14)
      tmp %>% group_by_at(c(input$aby,"datesbetween")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
    } else {
      tmp <- switch(input$aby,
             year = ytnpissuedata %>% filter(ISSN %in% fnewspapers()) %>% filter(datesbetween<=14), 
             month = mtnpissuedata %>% filter(ISSN %in% fnewspapers()) %>% filter(datesbetween<=14),
             week = wtnpissuedata %>% filter(ISSN %in% fnewspapers()) %>% filter(datesbetween<=14)
      )
      if (input$filterOutliers) tmp <- tmp %>% filter(datesbetween<=14)
      tmp %>% group_by_at(c(input$aby,"datesbetween")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
    }
  })
  textperweekdata <- reactive({
    tmp <- fnpissuedata() %>% group_by_at(input$aby) %>% group_by(a2s=round(tchars/(4*3000)),add=TRUE)
    if (input$filterOutliers) tmp <- tmp %>% filter(a2s<=14)
    tmp %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  wordlengthdata <- reactive({
    tmp <- fnppagedata() %>% mutate(wordlength = round(chars/words*5)/5)
    # if (input$filterOutliers) tmp <- tmp %>% filter(lettersize<=50)
    tmp %>% group_by_at(c(input$aby,"wordlength")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p2data <- reactive({ 
    tmp <- fnpissuedata()
    if (input$filterOutliers) tmp <- tmp %>% filter(pages<=20)
    tmp %>% group_by_at(c(input$aby,"pages")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p3data <- reactive({ 
    tmp <- fnppagedata()
    if (input$filterOutliers) tmp <- tmp %>% filter(wmodecols<=16)
    tmp %>% group_by_at(c(input$aby,"wmodecols")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p0data <- reactive({ 
    fnppagedata() %>% group_by_at(c(input$aby,"ISSN")) %>% summarise(count=n(),title=first(PAANIMEKE))
  })
  p4data <- reactive({ 
    fnppagedata() %>% group_by_at(c(input$aby,"type")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p5data <- reactive({
    fnppagedata() %>% mutate(a4s = round(chars/3000)) %>% group_by_at(c(input$aby,"a4s")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p6data <- reactive({
    tmp <- fnppagedata() %>% mutate(textdensity = round(parea/chars/5)*5)
    if (input$filterOutliers) tmp <- tmp %>% filter(textdensity<=50)
    tmp %>% group_by_at(c(input$aby,"textdensity")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p7data <- reactive({
    tmp <- fnppagedata()
    if (input$filterOutliers) tmp <- tmp %>% filter(wmfont!='Courier New',wmfont!='Unknown',wmfont!='null')
    tmp %>% group_by_at(c(input$aby,"wmfont")) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
  })
  p0 <- reactive({ 
    switch(input$aby,
           year = ggplot(p0data(),aes(x=year,y=count,fill=ISSN,text=title)) + geom_bar(stat='identity') + theme(legend.position = "none") + labs(x="Year",y="Abs Count",fill="Title") + scale_x_continuous(position="top",breaks=seq(0,2000,by=10),sec.axis = dup_axis(name=NULL)),
           month = ggplot(p0data(),aes(x=month,y=count,fill=ISSN,text=title)) + geom_bar(stat='identity') + theme(legend.position = "none") + labs(x="Month",y="Abs Count",fill="Title") + scale_x_date(date_breaks="10 year",date_labels = "%Y"),
           week = ggplot(p0data(),aes(x=week,y=count,fill=ISSN,text=title)) + geom_bar(stat='identity') + theme(legend.position = "none") + labs(x="Week",y="Abs Count",fill="Title") + scale_x_date(date_breaks="10 year",date_labels = "%Y")
    )
  })
  p1 <- reactive({ 
    switch(input$aby,
           year = ggplot(p1data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Issue gap",fill="Proportion") + scale_x_continuous(position="top",breaks=seq(0,2000,by=10),sec.axis = dup_axis(name=NULL)),
           month = ggplot(p1data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Issue gap",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y"),
           week = ggplot(p1data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Week",y="Issue gap",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y")
    )
  })
  p2 <- reactive({ 
    switch(input$aby,
           year = ggplot(p2data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Pages",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p2data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Pages",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(p2data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Week",y="Pages",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p3 <- reactive({ 
    switch(input$aby,
           year = ggplot(p3data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Columns",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p3data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Columns",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(p3data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Week",y="Columns",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p4 <- reactive({ 
    switch(input$aby,
           year = ggplot(p4data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Page size",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p4data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Month",y="Page size",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(p4data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Week",y="Page size",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  ptextperweek <- reactive({ 
    switch(input$aby,
           year = ggplot(textperweekdata() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=a2s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Text/week",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(textperweekdata() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=a2s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Month",y="Text/week",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(textperweekdata() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=a2s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Week",y="Text/week",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  pwordlength <- reactive({
    switch(input$aby,
           year = ggplot(wordlengthdata() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=wordlength,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Word length",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(wordlengthdata() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=wordlength,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Month",y="Word length",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(wordlengthdata() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=wordlength,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Week",y="Word length",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p5 <- reactive({ 
    switch(input$aby,
           year = ggplot(p5data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=a4s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Text/page",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p5data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=a4s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Text/page)",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(p5data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=a4s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Week",y="Text/page)",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p6 <- reactive({ 
    switch(input$aby,
           year = ggplot(p6data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=textdensity,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Year",y='sqmm/letter',fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL),
           month = ggplot(p6data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=textdensity,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Month",y='sqmm/letter',fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL),
           week = ggplot(p6data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=textdensity,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Week",y='sqmm/letter',fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL)
    )
  })
  p7 <- reactive({ 
    switch(input$aby,
           year = ggplot(p7data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=wmfont,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Font",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p7data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=wmfont,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Month",y="Font",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           week = ggplot(p7data() %>% filter(proportion>=input$proportionFilter),aes(x=week,y=wmfont,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Week",y="Font",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  output$plot <- renderPlotly({
    subplots <- list()
    heights <- numeric()
    if ("overview" %in% input$plots) {
      subplots <- c(subplots,list(p0()))
      heights <- append(heights,1.0)
    }
    if ("textperweek" %in% input$plots) {
      subplots <- c(subplots,list(ptextperweek()))
      heights <- append(heights,1.5)
    }
    if ("daysbetween" %in% input$plots) {
      subplots <- c(subplots,list(p1()))
      heights <- append(heights,1.0)
    }
    if ("pages" %in% input$plots) {
      subplots <- c(subplots,list(p2()))
      heights <- append(heights,1.0)
    }
    if ("pagesize" %in% input$plots) {
      subplots <- c(subplots,list(p4()))
      heights <- append(heights,0.75)
    }
    if ("columns" %in% input$plots) {
      subplots <- c(subplots,list(p3()))
      heights <- append(heights,0.75)
    }
    if ("textperpage" %in% input$plots) {
      subplots <- c(subplots,list(p5()))
      heights <- append(heights,1.25)
    }
    if ("wordlength" %in% input$plots) {
      subplots <- c(subplots,list(pwordlength()))
      heights <- append(heights,0.75)
    }
    if ("font" %in% input$plots) {
      subplots <- c(subplots,list(p7()))
      heights <- append(heights,0.5)
    }
    if ("textdensity" %in% input$plots) {
      subplots <- c(subplots,list(p6()))
      heights <- append(heights,0.75)
    }
    subplot(subplots,nrows=length(input$plots),titleY = TRUE, shareX = TRUE,heights = heights/(sum(heights)))
  })
  
  panomalies <- reactive({
    fnppagedata2() %>% inner_join(fnpissuedata2() %>% select(issueId,datesbetween),by = c("issueId")) %>% inner_join(fnppagedata(),by=switch(input$aby,
                                          year = c("ISSN","year"),
                                          month = c("ISSN","month"),
                                          week = c("ISSN","week")),suffix=c("",".y")) %>% 
    inner_join(fnpissuedata(),by=switch(input$aby,
                                         year = c("ISSN","year"),
                                         month = c("ISSN","month"),
                                         week = c("ISSN","week")),suffix=c("",".y2")) %>% 
    mutate(
      ad = round(area - area.y), 
      cd = wmodecols-wmodecols.y, 
      wd = round(words-words.y),
      po = page-pages,
      dd = datesbetween - datesbetween.y2,
      link = paste0('<a href="https://digi.kansalliskirjasto.fi/sanomalehti/binding/',issueId,'?page=',page,'">[O]</a>')
    ) %>% mutate(an = (wmfont!=wmfont.y) + (po>0) + (is.na(dd) || dd!=0) + (abs(ad)>area.y/2) + (cd!=0) + (abs(wd)>words.y/4)) %>% mutate(words.y=round(words.y,2)) %>% select(ISSN,title = PAANIMEKE,date,l=link,an,po,gp=pages,dd,db = datesbetween,gdb = datesbetween.y2,ad,pt=type,gpt=type.y,f=wmfont,gf=wmfont.y,cd,c=wmodecols,gc=wmodecols.y,wd,w=words,gw=words.y)
  })
  ianomalies <- reactive({
    fnpissuedata2() %>% inner_join(fnpissuedata(),by=switch(input$aby,
                                          year = c("ISSN","year"),
                                          month = c("ISSN","month"),
                                          week = c("ISSN","week")),suffix=c("",".y")) %>% 
      mutate(
        ad = round(area - area.y), 
        cd = wmodecols-wmodecols.y, 
        wd = round(words-words.y),
        pd = pages-pages.y,
        dd = datesbetween - datesbetween.y,
        link = paste0('<a href="https://digi.kansalliskirjasto.fi/sanomalehti/binding/',issueId,'">[0]</a>')
      ) %>% mutate(an = (wmfont!=wmfont.y) + (pd!=0) + (is.na(dd) || dd!=0) + (abs(ad)>area.y/2) + (cd!=0) + (abs(wd)>words.y/4)) %>% mutate(words.y=round(words.y,2)) %>% select(ISSN,title = PAANIMEKE,date,l=link,an,pd,p=pages,gp=pages.y,dd,db = datesbetween,gdb = datesbetween.y,ad,pt=type,gpt=type.y,f=wmfont,gf=wmfont.y,cd,c=wmodecols,gc=wmodecols.y,wd,w=words,gw=words.y)
  })
  output$panomalies <- renderDT({datatable(panomalies(), 
                                          escape = FALSE, 
                                          rownames = FALSE,
                                          extensions = c('ColReorder','Buttons'), 
                                          filter = list(position = 'top', clear = FALSE),
                                          options = list(
                                            columnDefs = list(list(orderSequence = c('desc', 'asc'), targets = "_all")),
                                            dom = 'Bfrtip', 
                                            buttons = I('colvis'),
                                            colReorder = TRUE,
                                            order = list(list(4, 'desc'))
                                          ))})
  output$ianomalies <- renderDT({datatable(ianomalies(), 
                                           escape = FALSE,
                                           rownames = FALSE,
                                           extensions = c('ColReorder','Buttons'), 
                                           filter = list(position = 'top', clear = FALSE),
                                           options = list(
                                             columnDefs = list(list(orderSequence = c('desc', 'asc'), targets = "_all")),
                                             dom = 'Bfrtip', 
                                             buttons = I('colvis'),
                                             colReorder = TRUE,
                                             order = list(list(4, 'desc'))
                                           ))})
  
  materialityCategories <- reactive({
    fnpissuedata() %>% group_by_at(input$groupBy) %>% summarise(count=n()) %>% ungroup() %>% mutate(proportion=round(count/sum(count),3))
  })
  topcats <- reactive({materialityCategories() %>% arrange(-count) %>% select(-count,-proportion) %>% unite(category,!!!syms(input$groupBy),sep=", ") %>% head(input$numCategories) %>% pull(category)})
  materialityCategories2 <- reactive({
    d <- materialityCategories() %>% unite(category,!!!syms(input$groupBy),sep=", ") %>% mutate(category=if_else(category %in% topcats(),category,"Other"))
    d %>% group_by(category) %>% summarise(count=sum(count)) %>% mutate(proportion=count/sum(count)) %>% ungroup() %>% mutate(category=reorder(category,count,sum))
  })
  materialityCategoriesByYear <- reactive({
    d <- fnpissuedata() %>% group_by_at(input$groupBy) %>% group_by(year,add=TRUE) %>% summarise(count=n()) %>% unite(category,!!!syms(input$groupBy),sep=", ") %>% mutate(category=if_else(category %in% topcats(),category,"Other"))
    d <- d %>% group_by(category,year) %>% summarise(count=sum(count)) %>% ungroup() %>% mutate(category=reorder(category,count,sum))
    d %>% group_by(year) %>% mutate(proportion=round(count/sum(count),3)) %>% ungroup() 
  })
  output$materialityCategories <- renderDT({datatable(materialityCategories(), 
                                                      options = list(
                                                        columnDefs = list(list(orderSequence = c('desc', 'asc'), targets = "_all")),
                                                        order = list(list(ncol(materialityCategories()), 'desc'))
                                                      ))})
  output$materialityGraph1 <- renderPlotly({ggplot(materialityCategories2(),aes(x=1,y=proportion,fill=category)) + geom_bar(stat='identity') + coord_flip() + theme(axis.title.y = element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())})
  output$materialityGraph2 <- renderPlotly({ggplot(materialityCategoriesByYear(),aes(x=year,y=proportion,fill=category)) + geom_bar(stat='identity')})
}

# Run the app
shinyApp(ui, server)
