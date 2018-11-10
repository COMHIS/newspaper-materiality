library(tidyverse)
library(ggplot2)
library(sqldf)
library(viridis)
library(plotly)
library(shiny)
library(lubridate)

load("app.RData")

ui <- fluidPage(
  h3("Finnish newspapers materiality explorer"),
  "by",a(href="http://iki.fi/eetu.makela","Eetu Mäkelä")," and the ",a(href="http://comhis.github.io","COMHIS research group"),
  br(),
  br(),
  fluidRow(
    column(3,sliderInput("years", "Years",min = 1771, max = 1917, value = c(1771, 1917),sep = "")),
    column(1,selectInput("languages","Languages",unique((newspapers %>% filter(ISSN %in% npissuedata$ISSN))$KIELI),multiple=TRUE,selected=c("fin","swe"))),
    column(2,selectInput("towns","Towns",unique((newspapers %>% filter(ISSN %in% npissuedata$ISSN))$KAUPUNKI_NORM),multiple=TRUE)),
    column(2,selectizeInput("issns","Titles",NULL,multiple=TRUE)),
    column(2,sliderInput("proportionFilter", "Filter props below",min = 0.0, max = 1.0, value = 0.0)),
    column(1,selectInput("by","Unit of obs",c("title","issue","page"),selected="title")),
    column(1,selectInput("aby","Calc props by",c("year","month"),selected="year"))
  ),
  textOutput("newspaperCount"),
  plotlyOutput("plot",height="700px")
)
# Server logic
server <- function(input, output, session) {
  fnewspapers1 <- reactive({
    fn <- newspapers %>% filter(lyear>=input$years[1],fyear<=input$years[2],KIELI %in% input$languages)
    if (length(input$towns)>0) fn <- fn %>% filter(KAUPUNKI_NORM %in% input$towns)
    fn
  })
  fnpids <- reactive({
    newspaperIDs = fnewspapers1()$ISSN
    names(newspaperIDs) <- fnewspapers1()$PAANIMEKE
    newspaperIDs
  })
  observe({
    updateSelectizeInput(session, 'issns', choices = fnpids(), server = TRUE)
  })
  fnewspapers <- reactive({
    fn <- fnewspapers1()
    if (length(input$issns)>0) fn <- fn %>% filter(ISSN %in% input$issns)
    fn
  })
  output$newspaperCount = renderText({paste("Number of newspapers:",nrow(fnewspapers()))})
  bnpissuedata <- reactive({
    if (input$by=="title") switch(input$aby,
                                  year = ytnpissuedata,
                                  month = mtnpissuedata)
    else npissuedata
  })
  bnppagedata <- reactive({
    switch(input$by,
           title = switch(input$aby,
                          year = ytnppagedata,
                          month = mtnppagedata),
           issue = inppagedata,
           page = nppagedata)
  })
  fnpissuedata <- reactive({ 
    bnpissuedata() %>% 
    filter(ISSN %in% fnewspapers()$ISSN) %>% 
    filter(year>=input$years[1],year<=input$years[2])
  })
  fnppagedata <- reactive({ 
    bnppagedata() %>% 
    filter(ISSN %in% fnewspapers()$ISSN) %>% 
    filter(year>=input$years[1],year<=input$years[2])
  })
  p1data <- reactive({
    if (length(unique(fnpissuedata()$ISSN))==1 && input$by !="title") {
      switch(input$aby,
             year = npissuedata %>% filter(ISSN %in% fnewspapers()$ISSN) %>% filter(datesbetween<=20) %>% group_by(year,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)),
             month = npissuedata %>% filter(ISSN %in% fnewspapers()$ISSN) %>% filter(datesbetween<=20) %>% group_by(month,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
      )
    } else {
      switch(input$aby,
             year = ytnpissuedata %>% filter(ISSN %in% fnewspapers()$ISSN) %>% filter(datesbetween<=20) %>% group_by(year,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)),
             month = mtnpissuedata %>% filter(ISSN %in% fnewspapers()$ISSN) %>% filter(datesbetween<=20) %>% group_by(month,datesbetween) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
      )
    }
  })
  p2data <- reactive({ switch(input$aby,
                              year = fnpissuedata() %>% filter(pages<=20) %>% group_by(year,pages) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)),
                              month = fnpissuedata() %>% filter(pages<=20) %>% group_by(year,month,pages) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))) })
  p3data <- reactive({ 
    switch(input$aby,
           year = fnppagedata() %>% filter(wmodecols<=16) %>% group_by(year,wmodecols) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) ,
           month = fnppagedata() %>% filter(wmodecols<=16) %>% group_by(month,wmodecols) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
    )
  })
  p4data <- reactive({ 
    switch(input$aby,
           year = fnppagedata() %>% group_by(year,type) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)),
           month = fnppagedata() %>% group_by(month,type) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
    )
  })
  p5data <- reactive({ 
    switch(input$aby,
           year = fnppagedata() %>% mutate(a4s = round(words/500)) %>% group_by(year,a4s) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)),
           month = fnppagedata() %>% mutate(a4s = round(words/500)) %>% group_by(month,a4s) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count))
    )
  })
  p6data <- reactive({ 
    switch(input$aby,
           year = fnppagedata() %>% mutate(lettersize = round(area/chars/5)*5) %>% filter(lettersize<=50) %>% group_by(year,lettersize) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) ,
           month = fnppagedata() %>% mutate(lettersize = round(area/chars/5)*5) %>% filter(lettersize<=50) %>% group_by(month,lettersize) %>% summarise(count=n(),ISSNs=paste0(unique(ISSN),collapse=", "),titles=paste0(unique(PAANIMEKE),collapse=", ")) %>% mutate(proportion=count/sum(count)) 
    )
  })
  p1 <- reactive({ 
    switch(input$aby,
           year = ggplot(p1data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Issue gap",fill="Proportion") + scale_x_continuous(position="top",breaks=seq(0,2000,by=10),sec.axis = dup_axis(name=NULL)),
           month = ggplot(p1data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=datesbetween,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Issue gap",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y")
    )
  })
  p2 <- reactive({ 
    switch(input$aby,
           year = ggplot(p2data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Pages",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p2data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=pages,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Pages",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p3 <- reactive({ 
    switch(input$aby,
           year = ggplot(p3data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Columns",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p3data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=wmodecols,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Columns",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p4 <- reactive({ 
    switch(input$aby,
           year = ggplot(p4data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Year",y="Page size",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p4data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=type,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + labs(x="Month",y="Page size",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p5 <- reactive({ 
    switch(input$aby,
           year = ggplot(p5data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=a4s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Year",y="Text/page",fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank()),
           month = ggplot(p5data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=a4s,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "none") + scale_y_continuous(breaks=c(1:1000),minor_breaks=NULL) + labs(x="Month",y="Text/page)",fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + theme(axis.title.x = element_blank())
    )
  })
  p6 <- reactive({ 
    switch(input$aby,
           year = ggplot(p6data() %>% filter(proportion>=input$proportionFilter),aes(x=year,y=lettersize,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Year",y='sqmm/letter',fill="Proportion") + scale_x_continuous(breaks= seq(0,2000,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL),
           month = ggplot(p6data() %>% filter(proportion>=input$proportionFilter),aes(x=month,y=lettersize,fill=proportion,text=titles)) + geom_raster() + scale_fill_viridis() + theme(legend.position = "bottom")  + labs(x="Month",y='sqmm/letter',fill="Proportion") + scale_x_date(date_breaks="10 year",date_labels = "%Y",sec.axis = dup_axis(labels=NULL,name=NULL)) + scale_y_continuous(breaks=seq(0,2000,by=5),minor_breaks=NULL)
    )
  })
  output$plot <- renderPlotly({subplot(p1(),p2(),p3(),p4(),p5(),p6(),nrows=6,titleY = TRUE, shareX = TRUE,heights = c(1,1,.75,.5,1,1)/5.25)})
}

# Run the app
shinyApp(ui, server)
