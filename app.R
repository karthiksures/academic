#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("chron")
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library("ggplot2")
library(reshape2)
library(chron)


#PLEASE CHANGE THE WORKING DIRECTORY TO THIS FOLDER
setwd("/home/hp/Trivago") 
#Reading the file 
bi_data=read.csv("Case Study - Business Analyst - Advertiser Intelligence (2_2).csv")
#Converting the Data
bi_data$insert_date=as.Date(lubridate::ymd(bi_data$insert_date))
#calculating the month and week 
bi_data$week=week(bi_data$insert_date)
bi_data$month=month(bi_data$insert_date)
#calculating the required parameters
bi_data$click_share=bi_data$clicks/sum(bi_data$clicks)
bi_data$cost_share=bi_data$cost/sum(bi_data$cost)

ad_campaign<-bi_data%>%group_by(adv_id,week)%>%summarise(clicks=sum(clicks),cost=sum(cost),booking_volume=sum(booking_volume),bookings=sum(bookings),top_pos=sum(top_pos),
                                                         beat=sum(beat),meet=sum(meet),lose=sum(lose),hotel_impressions=sum(hotel_impressions),partner_impressions=sum(partner_impressions),click_share=sum(click_share))%>%
  mutate(top_imression_ratio=top_pos/partner_impressions,beat_ratio=beat/(beat+ meet+lose),
         unavailability=(hotel_impressions-(beat+ meet+lose)),pay_percent=(cost/booking_volume)*100,hotels_per_partner=hotel_impressions/partner_impressions,top_pos_share=top_pos/sum(top_pos))
is.na(ad_campaign)<-sapply(ad_campaign, is.infinite)
ad_campaign[is.na(ad_campaign)]<-0        

# Define UI for application
ui <-dashboardPage(dashboardHeader(titleWidth=0,tags$li(class = "dropdown",tags$div(
                     tags$h1(p(strong("Trivago Case Study"),style = "color:white;font-family: 'arial';text-align: center;"),align = "right")))),
                   dashboardSidebar(disable = TRUE),
                   dashboardBody(fluidRow(tabsetPanel(id="tab",tabPanel(fluidRow(infoBoxOutput("posbox"),
                                          infoBoxOutput("ad_idbox"),
                                          infoBoxOutput("timeline")),
                                          
                                 box(width=12,textOutput("text"),tags$head(tags$style("#text{color:black;
                                                                                                          font-size:20px;
                                                                                                          font-family: 'arial';
                                                                                                          font-style:bold,italic;}"))),
                                 fluidRow(box(title=p("Pos Select",style = "color:black;font-family:'arial';"),solidHeader = FALSE,
                                                                                                                uiOutput("Firstselection")),
                                   box(title=p("Select Time Line",style = "color:black;font-family:'arial';"),solidHeader = FALSE,
                                       uiOutput("Secondselection"))),
                                         fluidRow(DT::dataTableOutput(outputId="table")),fluidRow(column(6,plotOutput("plot1")),column(6,plotOutput("plot2"))),title = "POS analysis",width = 12),
                                 tabPanel(fluidRow(box(title=p("Adv_ID Select",style = "color:black;font-family:'arial';"),solidHeader = FALSE,
                                                                uiOutput("adv_id_select"))),
                                          box(width=12,textOutput("text2"),tags$head(tags$style("#text2{color:black;font-size:20px;font-family: 'arial';font-style:bold,italic;}"))),
                                                            fluidRow(infoBoxOutput("clickbox"),
                                                            infoBoxOutput("costbox"), 
                                                            infoBoxOutput("posid2")),                   
                                          fluidRow(column(6,plotOutput("plot3")),column(6,plotOutput("plot4"))),
                                          fluidRow(column(6,plotOutput("plot5")),column(6,plotOutput("plot6"))),
                                          fluidRow(column(6,plotOutput("plot7")),column(6,plotOutput("plot8"))),title = "Adv_ID Analysis",width = 12))
                                                                                                                                                                                                                             
                                   ))
                               
                   
                   
)

# Define server logic required 
server <- function(input, output) {
  output$Firstselection<-renderUI({selectInput("pos",
                                               "Select Pos:",
                                               c("All",unique(as.character(bi_data$pos))))})
  output$Secondselection<-renderUI({selectInput("time_line",
                                                "Select time frame:",
                                                c("month","week","insert_date"))})
  output$table <- DT::renderDataTable({
    column=input$time_line
    pos="pos"
    table<-bi_data%>%group_by_(input$time_line)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                           cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                                cost_share_within_selection=cost/sum(cost))
    if (input$pos != "All"){
      table<-bi_data%>%group_by_(input$time_line,pos)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                  cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                       cost_share_within_selection=cost/sum(cost))
      table<-table[table$pos== input$pos,]
      }
    
    table
  },filter="top",selection = "single"
  )
  
  output$plot1 <- renderPlot({column=input$time_line
  pos="pos"
  table<-bi_data%>%group_by_(input$time_line)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                          cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                               cost_share_within_selection=cost/sum(cost))
  if (input$pos != "All"){
    table<-bi_data%>%group_by_(input$time_line,pos)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                                cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                                     cost_share_within_selection=cost/sum(cost))
    table<-table[table$pos== input$pos,]
  }
  column=as.character(input$time_line)
  columns=c("click_share","cost_share")
  cols=c(columns,column)
  display_data=melt(table[,cols],id.vars = cols[3])
  ggplot(display_data,aes_string(x=cols[3],y="value"))+ 
    geom_bar(stat="identity", width=.5,fill="blue",color="black")+
    facet_wrap(~variable)+ggtitle(paste0("Share of Click and Cost"))+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  })
  output$plot2 <- renderPlot({column=input$time_line
  pos="pos"
  table<-bi_data%>%group_by_(input$time_line)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                          cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                               cost_share_within_selection=cost/sum(cost))
  if (input$pos != "All"){
    table<-bi_data%>%group_by_(input$time_line,pos)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                                cost_share=sum(cost_share))%>%mutate(click_share_within_selection=clicks/sum(clicks),
                                                                                                     cost_share_within_selection=cost/sum(cost))
    table<-table[table$pos== input$pos,]
  }
  column=as.character(input$time_line)
  columns=c("click_share_within_selection","cost_share_within_selection")
  cols=c(columns,column)
  display_data=melt(table[,cols],id.vars = cols[3])
  ggplot(display_data,aes_string(x=cols[3],y="value"))+ 
    geom_bar(stat="identity", width=.5,fill="blue",color="black")+
    facet_wrap(~variable)+ggtitle(paste0("Share of Click and Cost Within ",input$time_line))+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  })
  output$ad_idbox<-renderInfoBox({infoBox("Total Ad id in the selected POS",if (input$pos != "All"){length(unique(bi_data$adv_id[bi_data$pos==input$pos]))}
                                          else{length(unique(bi_data$adv_id))},icon = icon("line-chart"),color="orange")})
  output$posbox<-renderInfoBox({infoBox("Total Pos",length(unique(bi_data$pos)),icon = icon("line-chart"),color="blue")})
  output$timeline<-renderInfoBox({infoBox(paste0("Total ",input$time_line),length(unique(bi_data[,input$time_line])),icon = icon("line-chart"),color="green")})
  
  output$text=renderText(paste("Attributes: 1) click_share and cost_share :- share of the total trivago clicks or cost,
                               2) within_selection:share of the clicks or cost within selected ",input$time_line))
  
  output$adv_id_select<-renderUI({selectInput("adv_id",
                                               "Select Adv_ID:",
                                               c("None",unique(as.character(bi_data$adv_id))))})
  output$text2=renderText(if(input$adv_id !="None"){paste("The following metrics are for Adv_ID:  ",input$adv_id," PS:if the booking volume is 0 it means booking is
registered or the advertiser doesn’t have a working pixel at the time"
                               )}else{paste("Please select an Adv_ID to View the Metrics")})
  
  output$plot3 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=unavailability)) + 
      geom_bar(stat="identity",color='blue',fill='blue')+ggtitle("Weekly Unavailability")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
    }})
  output$plot4 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=pay_percent)) + 
      geom_bar(stat="identity",color='red',fill='red')+ggtitle("Weekly Booking Cost Percentage")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  }})
  
  output$plot5 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=click_share)) + 
      geom_line(color='blue')+ggtitle("Weekly Click Share")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  }})
  
  output$plot6 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=top_pos_share)) + 
      geom_line(color='red')+ggtitle("Weekly Top Position Share")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  }})
  
  output$plot7 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=hotels_per_partner)) + 
      geom_bar(stat="identity",color='blue',fill='blue')+ggtitle("Weekly Hotel Impressions per Partner Impressions")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  }})
  
  output$plot8 <- renderPlot({if(input$adv_id !="None"){
    ggplot(ad_campaign[ad_campaign$adv_id==input$adv_id,], aes(x=week,y=beat_ratio)) + 
      geom_bar(stat="identity",color='red',fill='red')+ggtitle("Weekly Ratio of Beats")+theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5))
  }})
  
  output$clickbox<-renderInfoBox({infoBox(paste("Total clicks in Adv_ID: ",input$adv_id," is") ,if (input$adv_id !="None"){sum(ad_campaign$clicks[ad_campaign$adv_id==input$adv_id])}
                                          ,icon = icon("line-chart"),color="orange")})
  output$costbox<-renderInfoBox({infoBox(paste("Total Cost by Adv_ID: ",input$adv_id," is") ,if (input$adv_id !="None"){sum(ad_campaign$cost[ad_campaign$adv_id==input$adv_id])},icon = icon("line-chart"),color="blue")})
  output$posid2<-renderInfoBox({infoBox(paste("Total Bookings in Adv_ID: ",input$adv_id," is") ,if (input$adv_id !="None"){sum(ad_campaign$bookings[ad_campaign$adv_id==input$adv_id])},icon = icon("line-chart"),color="green")})
}

# Run the application 
shinyApp(ui = ui, server = server)

