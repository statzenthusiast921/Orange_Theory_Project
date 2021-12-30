# Orange Theory App - 05.25.2020
#----------------------------------#
#setwd("/Users/jonzimmerman/Desktop/Data Projects/OT Data Analysis/Shiny")
library(readxl)

#this file needs to be in same folder as app.R
OT_data=read_excel("OrangeTheoryData.xlsx",sheet=1)


library(lubridate)
library(RSQLite)
library(sqldf)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(zoo)
library(dplyr)
library(htmltools)
library(formattable)
library(tibble)


#----- Perform necessary preprocessing of data -----#
OT_data$Calories=round(OT_data$Calories,0)
OT_data$Splat=round(OT_data$Splat,0)
OT_data$Date=substring(OT_data$Date,1,10)

OT_data$YearMon=as.yearmon(OT_data$Date)
Year_Mon=unique(OT_data$YearMon)



topbotchoices=c("Top 5", "Bottom 5")

OT_data$Trainer2=ifelse(OT_data$Visits_Trainer>=3,OT_data$Trainer,"Other")
OT_data$Location2=ifelse(OT_data$Visits_Studio>=3,OT_data$Location,"Other")

TrainerChoices=unique(OT_data$Trainer2)
TrainerChoices=TrainerChoices[!TrainerChoices %in% "Other"]

LocationChoices=unique(OT_data$Location2)
LocationChoices=LocationChoices[!LocationChoices %in% "Other"]

OT_data$MonthName=ifelse(OT_data$Month==1, "JAN",ifelse(
                         OT_data$Month==2, "FEB",ifelse(
                         OT_data$Month==3, "MAR",ifelse(
                         OT_data$Month==4, "APR",ifelse(
                         OT_data$Month==5, "MAY",ifelse(
                         OT_data$Month==6, "JUN",ifelse(
                         OT_data$Month==7, "JUL",ifelse(
                         OT_data$Month==8, "AUG",ifelse(
                         OT_data$Month==9, "SEP",ifelse(
                         OT_data$Month==10,"OCT",ifelse(
                         OT_data$Month==11,"NOV","DEC"
                         
)))))))))))
OT_data$MonthName = factor(OT_data$MonthName, levels=c("JAN", "FEB", "MAR",
                                                       "APR", "MAY", "JUN",
                                                       "JUL", "AUG", "SEP",
                                                       "OCT", "NOV", "DEC"))

MonthCount=OT_data %>% 
  group_by(YearMon) %>% 
  summarize(MeanVisits=mean(VisitNum_Month),MeanCost=mean(Cost_Per_Class)) 

MonthCount=as.tibble(MonthCount)

MetricChoices=c("Calories","Splat","GrayMin","BlueMin","GreenMin","OrangeMin","RedMin","AvgHR","MaxHR")


Table1=sqldf("select VisitNum as 'Visit #', Date, Location as 'Studio', Trainer, Calories, Splat
             from OT_data
             group by VisitNum, Date, Location, Trainer")

TopCalories=sqldf("select Date, Location as 'Studio', Trainer, Calories, Splat
                  from OT_data
                  order by Calories desc")

BotCalories=sqldf("select Date, Location as 'Studio', Trainer, Calories, Splat
                  from OT_data
                  order by Calories")

TopSplat=sqldf("select Date, Location as 'Studio', Trainer, Calories, Splat
               from OT_data
               order by Splat desc")

BotSplat=sqldf("select Date, Location as 'Studio', Trainer, Calories, Splat
               from OT_data
               order by Splat")

cy=OT_data %>%
  filter(OT_data$Year==year(today()))
cy=as.data.frame(cy)

my=OT_data %>%
  filter(OT_data$Mon_Yr==paste0(month(today()),"-",year(today())))
my=as.data.frame(my)


subdata = OT_data %>%
  dplyr::select(Location,YearMon,Calories)

subdata=data.frame(subdata)
subtable=as.data.frame(aggregate(x=subdata[,3],
                                 by=list(subdata[,1],
                                         subdata[,2]),
                                 FUN=mean))
subtable$x=round(subtable$x,0)

library(reshape)
subtable2=as.data.frame(reshape(subtable,direction="wide",
                                v.names="x",
                                timevar="Group.2",
                                idvar="Group.1"))
Year.Mon=as.character(unique(subtable$Group.2))
colnames(subtable2)=c("Studio",Year.Mon)
subtable3=subtable2[-1]
rownames(subtable3)=subtable2$Studio

# Define UI for application
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title="Orange Theory Analysis"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome",
                 tabName = "welcome",
                 icon=icon("door-open")
        ),
        menuItem("All Data",
                 tabName = "AllData",
                 icon=icon("table")
        ),
        menuItem("Top/Bottom",
                 tabName = "Topbottom",
                 icon=icon("sort")
        ),
        
        menuItem("Trainer Boxplots",
                 tabName = "trainbox",
                 icon=icon("user")
        ),
        
        menuItem("Studio Boxplots",
                 tabName = "studiobox",
                 icon=icon("dumbbell")
        ),
        
        menuItem("Performance Metrics",
                 tabName = "heat_table",
                 icon=icon("calculator")
        ),
        menuItem("Cost",
                 tabName = "cost_chart",
                 icon=icon("dollar-sign")
        ),
        menuItem("Time",
                 tabName = "time",
                 icon=icon("hourglass-half"))
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "welcome",
          fluidPage(h1("Welcome to my Orange Theory Shiny Dashboard!"),
                    br(),
                    p(strong(tags$u("What is Orange Theory?"))),
                    p(a("Orange Theory", href = "https://en.wikipedia.org/wiki/Orangetheory_Fitness"), "is a 60-minute, full-body workout, focused on training endurance, strength, and power.  
                      It is a", a("High Intensity Interval Training (HIIT)",href="https://en.wikipedia.org/wiki/High-intensity_interval_training")," workout, which alternates between short periods of intense exercise and long recovery periods."), 
                    p("Studios are split into three stations: treadmills, rowing machines, and weight training.  Members cycle between theses stations over the course of a class at the instruction of a coach."),
                    p("Each member wears a heart rate (HR) monitor during the workout to track statistics throughout the class."),
                    br(),
                    p(strong(tags$u("How does the HR monitor track performance?"))),
                    p(tags$li("Amount of time spent in each of the five designated HR zones:")),
                    p(tags$ul(tags$li(div("Gray Zone: <=60% of your max HR",style = "color:grey")))),
                    p(tags$ul(tags$li(div("Blue Zone: Between 61% and 70% of your max HR",style = "color:blue")))),
                    p(tags$ul(tags$li(div("Green Zone: Between 71% and 83% of your max HR",style = "color:green")))),
                    p(tags$ul(tags$li(div("Orange Zone: Between 84% and 91% of your max HR",style = "color:orange")))),
                    p(tags$ul(tags$li(div("Red Zone: >=92% of your max HR",style = "color:red")))),
                    p(tags$li("Calories Burned, Average and Max HR, Splat Points (Orange Min + Red Min)")),
                    br(),
                    p(strong(tags$u("How can I use this dashboard?"))),
                    p("You can click on any of the menu items on the left to see a different analysis of my exercise data.")
                    )),
        tabItem(
          tabName = "AllData",
          DTOutput("alldata")
        ),
        
        tabItem(
          tabName = "Topbottom",
          selectInput(inputId = "topbottomselect",label="Select Top 5 or Bottom 5:",choices=topbotchoices,selected="Top 5"),
          DTOutput("topbottom1"),
          DTOutput("topbottom2")
        ),
        tabItem(
          tabName = "trainbox",
          selectInput(inputId = "trainerselect",label="Select a Trainer:",choices=TrainerChoices),
          fluidPage(splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("trainbox_Calories"),
                                plotOutput("trainbox_Splat")
          ))
        ),
        tabItem(
          tabName = "studiobox",
          selectInput(inputId = "studioselect",label="Select a Studio:",choices=LocationChoices),
          fluidPage(splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("studiobox_Calories"),
                                plotOutput("studiobox_Splat")
          ))
        ),
        
        tabItem(
          tabName = "heat_table",
          selectInput(inputId = "metricselect",label="Select a Metric:",choices=MetricChoices),
          DTOutput("heat_table")
          
        ),
        tabItem(
          tabName = "cost_chart",
          fluidRow(align="center",p(strong("Overall Cost Metrics"))),
          fluidRow(splitLayout(cellWidths = c("33.3%", "33.3%","33.3%"),
                               valueBoxOutput("vbox1",width=10),
                               valueBoxOutput("vbox2",width=10),
                               valueBoxOutput("vbox3",width=10))),
          fluidRow(align="center",
                   sliderInput("slider", "Choose Ideal Monthly Cost Per Class: ",min=5, max=50, step=5,value=0,pre="$",sep=",")),
          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                               valueBoxOutput("vbox8",width=10),
                               valueBoxOutput("vbox9",width=10))),
          plotOutput("cost_chart")
          ),
        tabItem(
          tabName = "time",
          fluidRow(column(width=6,
                          selectInput(inputId = "yearmonselect",label="Select Month-Year:",choices=unique(OT_data$YearMon))),
                         
                   column(width=6,
                          dateRangeInput('dateRange',label = 'Select Date range:',
                                         start = OT_data$Date[1], end = OT_data$Date[dim(OT_data)[1]]))),
          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                          valueBoxOutput("vbox4",width=10),
                          valueBoxOutput("vbox6",width=10))),
          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                          valueBoxOutput("vbox5",width=10),
                          valueBoxOutput("vbox7",width=10))),
          fluidRow(align="center",
                   selectInput(inputId = "yearselect",label="Select Year:",choices=unique(OT_data$Year))),
          fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                               plotOutput("AvgSplat"),
                               plotOutput("AvgCalories")))
                          )))))
    
# Define server logic 
server <- function(input, output) {
  
  
  
  #----------ALL DATA TABLE----------#
  output$alldata=renderDT({
    datatable(Table1,options=list(pageLength=10,
                                  lengthMenu=c(10,15,20,25)
    ),
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black; font-weight: bold",
      'All Data'
    ))
  })
  
  
  #---------TOP AND BOTTOM TABLES--------#
  TopRankData=reactive({
    if (input$topbottomselect == "Top 5") {
      ordered=head(Table1[order(-Table1$Calories), ],n=5)
    } else {
      ordered=head(Table1[order(Table1$Calories), ],n=5)
    }
    return(ordered)
  })
  
  BotRankData=reactive({
    if (input$topbottomselect == "Top 5") {
      ordered=head(Table1[order(-Table1$Splat), ],n=5)
    } else {
      ordered=head(Table1[order(Table1$Splat), ],n=5)
    }
    return(ordered)
  })
  
  
  output$topbottom1=renderDT({
    
    datatable(TopRankData(),rownames = FALSE,
              options = list(pageLength = 5, lengthChange = FALSE, dom='t'),
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color:black; font-weight: bold",
                input$topbottomselect,'Classes by Calories Burned'
              ))%>% 
      formatStyle(columns = 5,
                  backgroundColor = "orange",
                  fontWeight = "bold")
    
  })
  
  output$topbottom2=renderDT({
    
    datatable(BotRankData(),rownames = FALSE,
              options = list(pageLength =5, lengthChange = FALSE, dom='t'),
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color:black; font-weight: bold",
                input$topbottomselect, 'Classes by Splat Points Earned'
              ))  %>% 
      formatStyle(columns = 6,
                  backgroundColor = "orange",
                  fontWeight = "bold")
    
  })
  
  #---------- TRAINER BOXPLOTS ----------#
  
  
  TrainVar=reactive({
    NewDummy=ifelse(OT_data$Trainer2==input$trainerselect,input$trainerselect,"Other")
    NewDummy=factor(NewDummy,levels=c(input$trainerselect,"Other"))
    return(NewDummy)
  })
  
  LocVar=reactive({
    NewDummy2=ifelse(OT_data$Location2==input$studioselect,input$studioselect,"Other")
    NewDummy2=factor(NewDummy2,levels=c(input$studioselect,"Other"))
    return(NewDummy2)
  })
  
  output$trainbox_Calories=renderPlot({
    NewDummy=TrainVar()
    plot=ggplot(OT_data,aes(y=Calories,x=as.factor(NewDummy)))+geom_boxplot(aes(col=as.factor(NewDummy)))
    plot=plot+ggtitle(paste0("Calories Burned with ",input$trainerselect," vs. All Other Trainers"))
    plot=plot+theme(legend.position="none")
    plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
    plot=plot+xlab("Trainer")
    plot
  })
  
  output$trainbox_Splat=renderPlot({
    NewDummy=TrainVar()
    plot=ggplot(OT_data,aes(y=Splat,x=as.factor(NewDummy)))+geom_boxplot(aes(col=as.factor(NewDummy)))
    plot=plot+ggtitle(paste0("Splat Points Earned with ",input$trainerselect," vs. All Other Trainers"))
    plot=plot+theme(legend.position="none")
    plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
    plot=plot+xlab("Trainer")
    plot
  })
  
  #----------LOCATION BOXPLOTS----------#
  
  output$studiobox_Calories=renderPlot({
    NewDummy2=LocVar()
    plot=ggplot(OT_data,aes(y=Calories,x=as.factor(NewDummy2)))+geom_boxplot(aes(col=as.factor(NewDummy2)))
    plot=plot+ggtitle(paste0("Calories Burned at ",input$studioselect," Studio vs. All Other Studios"))
    plot=plot+theme(legend.position="none")
    plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
    plot=plot+xlab("Studio")
    plot
  })
  
  
  output$studiobox_Splat=renderPlot({
    NewDummy2=LocVar()
    plot=ggplot(OT_data,aes(y=Splat,x=as.factor(NewDummy2)))+geom_boxplot(aes(col=as.factor(NewDummy2)))
    plot=plot+ggtitle(paste0("Splat Points Earned at ",input$studioselect," Studio vs. All Other Studios"))
    plot=plot+theme(legend.position="none")
    plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
    plot=plot+xlab("Studio")
    plot
  })
  
  
  
  
  
  
  #----------PERFORMANCE METRICS----------#
  
  HighlightTableData=reactive({
    
    #Make the highlight table static first
    
    subdata = OT_data %>%
      dplyr::select(Location,YearMon,input$metricselect)
    
    subdata=data.frame(subdata)
    subtable=as.data.frame(aggregate(x=subdata[,3],
                                     by=list(subdata[,1],
                                             subdata[,2]),
                                     FUN=mean))
    subtable$x=round(subtable$x,0)
    
    library(reshape)
    subtable2=as.data.frame(reshape(subtable,direction="wide",
                                    v.names="x",
                                    timevar="Group.2",
                                    idvar="Group.1"))
    Year.Mon=as.character(unique(subtable$Group.2))
    colnames(subtable2)=c("Studio",Year.Mon)
    subtable3=subtable2[-1]
    rownames(subtable3)=subtable2$Studio
    return(subtable3)
    
    
    
    
  })
  
  
  
  
  output$heat_table=renderDT({
    
    
    brks <- quantile(HighlightTableData(), probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    
    datatable(HighlightTableData(),rownames=TRUE,
              options = list(scrollX = TRUE,
                             lengthChange=FALSE,
                             dom = 't'
              ),
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: center; color:black; font-weight: bold",
                'Average Performance by Studio and Month')) %>%
      formatStyle(names(subtable3), backgroundColor = styleInterval(brks, clrs))
    
  })
  
  
  #---------- COST CHARTS ----------#
  
  output$vbox1 <- renderValueBox({
    valueBox(
      paste0("$",round(sum(OT_data$Cost_Per_Class)/nrow(OT_data),2)),
      "Overall Cost Per Class",
      icon = icon("credit-card"),
      color="green"
    )
  })
  
  
  
  output$vbox2 <- renderValueBox({
    valueBox(
      paste0("$",round(mean(cy$Cost_Per_Class),2)),
      "YTD Cost Per Class",
      icon = icon("credit-card"),
      color = "green"
    )
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      paste0("$",round(mean(my$Cost_Per_Class),2)),
      "MTD Cost Per Class",
      icon = icon("credit-card"),
      color = "green" )
  })
    
    
    output$vbox8 <- renderValueBox({
      valueBox(
        round(233/(input$slider),0),
        "Classes Per Month Needed to Meet Goal",
        icon = icon("check-square"),
        color = "teal"
      )
    })
    
    
    ClassNumFilter=reactive({
      MC=MonthCount %>%
        filter(MeanVisits>=round(233/(input$slider),0))
      return(MC)
    })
    
    

      
      output$vbox9 <- renderValueBox({
        valueBox(
          dim(ClassNumFilter())[1],
          "Number of Months I Met This Goal",
          icon = icon("check-square"),
          color = "teal")
    
    
  })
      

      filtered <- reactive({
        MonthCount$Specific <- ifelse(MonthCount$MeanVisits >= round(233/(input$slider),0), 1,0)
        return(MonthCount)
      })
      #MonthCount$Specific <- ifelse(MonthCount$MeanVisits == round(233/(25),0), 1,0)
      

      output$cost_chart=renderPlot({
   
        
        ggplot(data = filtered(), aes(x = YearMon, y = MeanCost,fill=as.factor(Specific))) +
          geom_bar(stat = "identity", position = "dodge")+
          theme(legend.position = "none")+
          ggtitle("Avg Monthly Cost Per Class")+
          theme(plot.title = element_text(color="black", size=14, face="bold",hjust=0.5))+
          xlab("Month-Year")+ylab("Average Monthly Cost")+
          scale_x_yearmon(breaks = filtered()$YearMon)
        

      })
  
  
#----------TIME CHARTS----------#
  
  
  Time_Filter=reactive({
    filter=subset(OT_data,YearMon==input$yearmonselect)
    return(filter)
  })
  
  
  Range_Filter=reactive({
    filter=subset(OT_data,Date>=input$dateRange[1] & Date<=input$dateRange[2])
    return(filter)
  })
  
  
  
  
  output$vbox4 <- renderValueBox({
    valueBox(
      round(sum(Time_Filter()$Splat),0),
      "Total Splat Points Earned",
      icon = icon("walking"),
      color="red"
    )
  })
  
  
  
  output$vbox5 <- renderValueBox({
    valueBox(
      round(sum(Time_Filter()$Calories),0),
      "Total Calories Burned",
      icon = icon("fire"),
      color="orange"
    )
  })
  
  output$vbox6 <- renderValueBox({
    valueBox(
      round(mean(Range_Filter()$Splat),2),
      "Avg. Splat Points Earned",
      icon = icon("walking"),
      color="red"
    )
  })
  
  output$vbox7 <- renderValueBox({
    valueBox(
      round(mean(Range_Filter()$Calories),2),
      "Avg. Calories Burned",
      icon = icon("fire"),
      color="orange"
    )
  })
  
  Year_Filter=reactive({
    filter=subset(OT_data,Year==input$yearselect)
    return(filter)
  })
  
  output$AvgSplat <-renderPlot({
    ggplot(Year_Filter(),aes(y=SplatPts_Month,x=MonthName,group='l'))+geom_line()+geom_point()+
      ylab("Avg Splat Points Per Month")+ylim(c(0,20))+
      xlab("Month")+
      ggtitle("Avg Splat Points Earned Per Month")+
      theme(plot.title = element_text(color="black", size=14, face="bold",hjust=0.5))
    

  })
  
  output$AvgCalories <-renderPlot({
    ggplot(Year_Filter(),aes(y=Calories_Month,x=MonthName,group='l'))+geom_line()+geom_point()+
      ylab("Avg Calories Per Month")+ylim(c(0,700))+
      xlab("Month")+
      ggtitle("Avg Calories Burned Per Month")+
      theme(plot.title = element_text(color="black", size=14, face="bold",hjust=0.5))
 

    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




