library(shiny)
library(shinythemes)
library(TTR)
library(xtable)
library(compiler)
library(reshape)
library(plyr)
library(chron)
library(ggplot2)
library(quantmod)
library(rvest)
library(data.table)
library(finreportr)
library(dqshiny)
library(DT)
library(XML)
library(gsubfn)
library(dplyr)
library(gplots)

#setwd("C:/Users/206582739/Dropbox (NBCUniversal)/Drew Personal/Shiny/JWA")
#setwd("~/Google Drive/Medallion Fund/data_grabber")

#source("s_p_data.R")

source("stock_db_converter.R")
source("returns_wrangler.R")
source("sector_analyzer.R")
source("alpha_calculator.R")

sp_dt = readRDS("sp_dt")
sp_list = readRDS("sp_list")
stock_db = readRDS("stock_db")
sector_sums = readRDS("sector_sums")
sector_avgs = readRDS("sector_avgs")
sector_wtd_avgs = readRDS("sector_wtd_avgs")

title="S&P 500"

ticker_options = sort(names(sp_list))

min_date = sp_dt$Date[1]

max_date = sp_dt$Date[nrow(sp_dt)]

start_date_v1 = as.POSIXlt(max_date)
start_date_v1$year=start_date_v1$year-1
start_date = as.Date(start_date_v1)

ui<-navbarPage(
  theme=shinytheme("slate"),
  title=paste(title,': Historical Data', sep=" "),
  
  ###########################################################################################################################
  ###########################################################################################################################
  tabPanel("Overview",
      fluidRow(
        column(2),
        column(8,
               wellPanel(
                 h1("General"),
                 h5(""),
                 h5("The purpose of this 'website' is two-fold. First, I wanted to build an app in which I could dive
                    deeper into stock performance as a way of being a smarter investor once I've actually accumulated some
                    personal wealth, leading to data-driven decisions rather than reading some MarketWatch articles 
                    and just throwing money into something that sounds good. Second, I wanted to use this as a way to 'advertise' 
                    my abilities in data science/web development/data analysis. Let me be clear: I didn't create this app because I'm solely
                    interested in finance. I created this app because finance data is the most accessible, and because I think everybody could
                    stand to make wiser investment decisions when it comes to their 401k/IRA and therefore this app is useful to all site visitors.  
                    I am currently seeking a job in analytics and am hoping that this can be used as a conversation starter. Once I have secured a 
                    position I will remove the past projects/bio section but I plan on continuing to work on the shiny app over the next couple of 
                    decades, making it available to friends and family. This app is not fully polished by any means; rather, it is a minimum viable product
                    that I developed over the course of a week and a half that works and shows my skill set in data curation, transformation, 
                    and presentation. It can standalone with minimal upkeep, which means I can work on it when I have free time and ignore it
                    when I don't. If you're a friend or family member, I hope you find the site useful and interesting. If you're an employer, 
                    please hire me!"),
                 h5(""),
                 tags$hr(style="border-color: yellow;"),
                 h1("Stock Analysis"),
                 h5(""),
                 h3("Invidividual Stock"),
                 h5("Look up individual company information such as company headquarters, P/E, shares outstanding,
                    etc. Also shows the stock price over a given timeframe. All stocks listed in the S&P 500 are 
                    available for analysis."),
                 h5(""),
                 h3("Comparison"),
                 h5("This tab allows you to do side-by-side comparisons of individual stocks."),
                 h5(""),
                 h3("Sector Analysis"),
                 h5("This tab allows you to compare the 11 different sectors of the S&P 500 to each other, aggregated by sum, average, or 
                    weighted average where weight is represented by a company's market cap."),
                 h5(""),
                 h3("Alpha"),
                 h5("This tab displays the Alpha and Beta values of individual stocks, displayed one sector at a time. For the layman, 
                    a stock's 'Beta' is a historical measure of volatility. Beta measures how a stock or portfolio moves relative to a 
                    benchmark or index like the S&P 500. A Beta = 1.0 means the stock moves in lockstep with the index. A Beta of 2.0 means the stock moves
                    twice as much as the benchmark, and because it's twice as volatile, the stock is twice as risky and therefore, to be compensated 
                    for the risk that you're holding, the stock is expected to generate twice the returns. A stock's 'Alpha' is a measure 
                    of how a stock performs once its risk-adjusted return is taken into account. So, if a stock has a Beta = 1.5 and the index
                    increases by 3%, the expected risk-adjusted return is 4.5%. If the stock actually returns 7%, the alpha associated with that stock
                    is 7%-4.5% or 2.5%. If the stock were to only actually return 3%, its alpha would be -1.5%.
                    When it comes to creating a portfolio, managers are seeking out high alphas rather than high returns because this actually takes
                    risk into account. This is where Bridgewater's flagship fund 'Pure Alpha Fund' gets its name, as well as the website 'Seeking Alpha'."),
                 h5(""),
                 h5("On this tab you can select one of three models: Capital Asset Pricing Model (CAPM), Fama and French's 3 Factor Model, or 
                    Fama and French's 5 Factor Model."),
                 tags$li("CAPM: The capital asset pricing model helps to calculate investment risk and what return on investment an investor should expect.
                         It consists of two components, alpha and beta. The general equation for the CAPM model is: stock_rf ~ stock_beta*mkt_rf + alpha, 
                         where stock_rf is the stock return minus the risk free rate and mkt_rf is the index return minus the risk free rate. 
                         For more details click on the investopedia link below."),
                 uiOutput("CAPM_link"),
                 tags$li("Three Factor: This model was developed by French and Fama, two nobel laureates from the University of Chicago (no big deal) as a way to 
                         also factor in value risk as well as size risk on top of the CAPM model. These professors found through research that value stocks typically
                         outperform growth stocks when accounting for market risk. Similarly, they found that small-cap stocks also typically outperform large-cap stocks.
                         To accomodate these new factors, they introduced HML and SMB as covariates that are used to predict how individual stocks will perform with these
                         characteristics included. The general equation for the Three factor model is: stock_rf ~ stock_beta*mkt_rf + hml_beta*HML + smb_beta*SMB + alpha. 
                         For more information, click the investopedia link below."),
                 uiOutput("Three_Fact_link"),
                 tags$li("Five Factor: Same as Three Factor model, but with two additional factors: RMW and CMA. The new model adds the concept that companies reporting 
                         higher future earnings have higher returns in the stock market, a factor referred to as profitability. The fifth factor, referred to as investment, 
                         relates the concept of internal investment and returns, suggesting that companies directing profit towards major growth projects are likely to 
                         experience losses in the stock market. RMW (Robust Minus Weak) is the average return on the two robust operating profitability portfolios minus the 
                         average return on the two weak operating profitability portfolios. CMA (Conservative Minus Aggressive) is the average return on the two conservative 
                         investment portfolios minus the average return on the two aggressive investment portfolios. The general equation for the Five factor model is: 
                         stock_rf ~ stock_beta*mkt_rf + hml_beta*HML + smb_beta*SMB + rmw_beta*RMW + cma_beta*CMA + alpha. For more information, click the French data link below."),
                 uiOutput("Five_Fact_link"),
                 tags$hr(style="border-color: yellow;"),
                 h1("Bio"),
                 h5(""),
                 h3("Background"),
                 h5(""),
                 h5("My name is Drew Ficken. I grew up in Northwest Indiana, and went to Purdue University where I majored in Mechanical Engineering. 
                    While there I interned for Rolls-Royce for two summers - the first I worked on Navy Ship propellers, the second I worked on the 
                    F-35 lift fan system (the fan's thrust allows the jet to take off and land vertically). After graduating I joined a proprietary trading
                    firm called Andrie Trading, where I managed the company's gold options portfolio. We used algorithmic-based trading software to execute 
                    trades as efficiently as possible, relying on analytics tools we developed in house (using Shiny apps like this one) to create signals as
                    to when to put on and take off risk, looking to identify inefficiencies within the market microstructure that we could profit off of. 
                    After 3 years at this firm, I enrolled in the University of Chicago's dual degree MBA/MPCS program. I took as many data science 
                    classes as possible, knowing that I wanted to be working directly with data after graudating. For my internship last summer, 
                    I worked for NBCUniversal's Mobile Games Publishing team, where I built out dashboards and predictive analytics tools to help the 
                    PMs and User Acquisition team make better decisions, as well as have a better grasp for how their mobile games had been performing.
                    I graduated in June and since then have been looking for a job in analytics in Chicago. I developed this app in order to get some
                    traction with companies, hoping to provide some transparency into what I'm capable of, something that a resume simply can't accomplish."),
                 h5(""),
                 h3("Past Projects"),
                 h5(""),
                 h5("These are some of the data science assignments I worked on while at Booth. I've included these to show that we didn't just learn some top level
                    data science theories, but actually wrote code and tested models on real world data. While some of the projects include multiple students' names,
                    I only included the projects in which I was fully responsible for the entire codebase. These aren't meant to be fully grasped. Instead, they are 
                    supposed to give you an idea of some of the analytics topics I've covered in my two years at Booth, and show that I can dig down into the details
                    of a complex program and come up with a data-driven solution to a business problem."),
                 h3("Resume"),
                 h5(""),
                 tags$embed(style="height:800px; width:100%; scrolling=yes",
                            src="Drew_Ficken_Resume_0819.pdf#toolbar=0"),
                 h3("App Info"),
                 h5(""),
                 h5("This app was created using RStudio along with an R package called 'Shiny', which makes it easy to build interactive web apps straight  
                    from R. I built this app from scratch, using a couple of web scraper examples from stackoverflow and R-bloggers in order to grab data from
                    websites. You can look at my github site for the exact code I used to pull in and format the data for the site. I never used a .csv file 
                    because I want to continue grabbing updated information solely by running the app. That being said, some of the data takes some time 
                    to extract and therefore I will only update some of the data (Market Cap, P/E, etc.) once a month. If you'd like to collaborate with me on some
                    ideas or the coding, just send me an email to the address listed in my resume. I would love some help."),
                 h5(""),
                 h3("Github Site"),
                 h5(""),
                 h5("")
               )
      ),
      column(2)
  )),
  ###########################################################################################################################
  ###########################################################################################################################
  tabPanel("Individual Stock",
      fluidRow(
          column(7,
              wellPanel(
                dateRangeInput(inputId='dateRange', label="Date Range",
                               start=start_date, end=max_date, 
                               min=min_date, max = max_date),
                selectizeInput(inputId='selectTicker',label = "Select Ticker",
                                   choices=ticker_options, selected="MMM"),
                div(style='overflow-x:scroll',tableOutput("stock_header")),
                div(style='overflow-x:scroll',tableOutput("stock_info1")),
                div(style='overflow-x:scroll',tableOutput("stock_info2")),
                div(style='overflow-x:scroll',tableOutput("stock_info3")),
                div(style='overflow-x:scroll',tableOutput("stock_info4")),
                div(style='overflow-x:scroll',tableOutput("stock_info5")),
                div(style='overflow-x:scroll',tableOutput("stock_info6"))
                )
              
          ),
          column(5,
                 plotOutput("stock_plot"),
                 tableOutput("stock_table")
              )
         )
  ),
  
  ###########################################################################################################################
  ###########################################################################################################################
  tabPanel("Comparison",
     fluidRow(
       column(7,
              wellPanel(
                selectizeInput(inputId='selectMultTickers',label = "Select Tickers",
                               choices=ticker_options, selected=c("GOOG","FB","AMZN","AAPL"), multiple=TRUE),
                div(style='overflow-x:scroll',tableOutput("stock_compare"))
              )
       ),
       column(5,
              wellPanel(
               radioButtons(inputId='selectChartField', label = "Compare Companies By",
                               choices = c('Market Cap','Income','Sales','Employees','P/E','P/S','Beta','Avg Volume'),
                               selected = 'Market Cap', inline = T),
              plotOutput("compare_chart")  
       )),
     fluidRow(
       column(12,
              wellPanel(
              dateRangeInput(inputId='dateRange2', label="Date Range",
                             start=start_date, end=max_date,
                             min=min_date, max = max_date),
              radioButtons(inputId='selectChartField2', label = "Compare Companies By",
                           choices = c('Adjusted','Returns','PctRet','Volume'),
                           selected = 'Adjusted', inline = T),
              plotOutput("returns_plot"))
     ))
  )),
  ###########################################################################################################################
  ###########################################################################################################################
  tabPanel("Sector",
          fluidRow(
            column(6,
                wellPanel(
                  radioButtons(inputId='selectPieField',label = 'Compare Sectors By',
                               choices = c("MktCap","Income","Sales","Employees","AvgVol"),
                               selected = "MktCap", inline = T),
                  selectInput(inputId="selectDisplayType", label = "Aggregation Type",
                              choices = c("Sum","Average","Weighted Average"),
                              selected = "Sum"),
                  plotOutput("sector_pie")
                )),
            column(6,
                  tableOutput("sector_table"),
                wellPanel(
                  selectInput(inputId = "selectSector", label = "Select Sector",
                              choices = sort(names(sector_list)),
                              selected = "Communication Services")
                )
              )
          ),
          fluidRow(
            column(12,
               wellPanel(
                 div(style='overflow-x:scroll',tableOutput("indSector"))
               ))
          )),
  ###########################################################################################################################
  ###########################################################################################################################
  
  #industry, model, display, months
  tabPanel("Alpha",
           wellPanel(
             fluidRow(
               column(3,
                      selectInput(inputId='selectModel',label = 'Choose Model',
                                   choices = c("CAPM","Three Factor","Five Factor"),
                                   selected = "CAPM")
               ),
            column(3,
                   uiOutput("ui")
             ),
            column(3,
                     selectInput(inputId = "selectIndustry", label = "Select Sector",
                                 choices = sort(names(sector_list)),
                                 selected = "Communication Services")
            ),
             column(3,
                    sliderInput(inputId='selectMonths', label = "Select Number of Recent Months",
                                min = 10, max=200, value=40, step=1))
           ),
           fluidRow(
             column(3,
                    h6("CAPM: stock_rf ~ stock_beta*mkt_rf + alpha"),
                    h6("Three Factor: stock_rf ~ stock_beta*mkt_rf + hml_beta*HML + smb_beta*SMB + alpha"),
                    h6("Five Factor: stock_rf ~ stock_beta*mkt_rf + hml_beta*HML + smb_beta*SMB + rmw_beta*RMW + cma_beta*CMA + alpha")),
             column(9,
                    plotOutput("heatmap_output",
                               height = 600))
            
           ),
           fluidRow(h1("")),
           fluidRow(
             column(3,
                    h6("Plotted Expected vs. Realized Returns")),
             column(9, 
                    plotOutput("plot_alpha", 
                               height = 600))
                    )
           )),# 
  # )
  
  ###########################################################################################################################
  ###########################################################################################################################
  
  #industry, model, display, months
  tabPanel("Past Projects",
           tabsetPanel(type = "tabs",
              tabPanel("Targeting based on Heterogeneous Treatment Effects",
                    h1(""),
                    h5("Given customer-level data from an appliance store, developed a target audience for a mailing 
                       campaign that targets people based on how effective receiving a catalog would be for the individual. 
                       Used this campaign to predict the profits that result from this mailer getting sent to individuals. 
                       Used OLS, LASSO, elastic net and causal forest regression models to predict treatment effects."),
                    tags$embed(style="height:800px; width:100%; scrolling=yes",
                                src="Heterogeneous_Targeting_Effects.pdf#toolbar=0")),
              tabPanel("Analyze Household Purchasing Characteristics Over Time",
                    h1(""),
                    h5("Given panel data for more than 60,000 households, looked at beverage consumption patterns by households from 2004 to 2014.
                       Specifically looked at carbonated sodas, carbonated diet sodas, and water consumption throughout this time period, as well as 
                       conducted brand analysis on different companies that sell water. Identified industry-wide patterns as well as competitive 
                       trends within the marketplace"),
                    tags$embed(style="height:800px; width:100%; scrolling=yes",
                              src="Household_Consumption_Patterns.pdf#toolbar=0")),
              tabPanel("Estimate Own and Competitive Brand-Level Advertising Effects",
                       h1(""),
                       h5("Given store-level data for antacid sales, looked at the effects of own & competitor pricing as well as 
                       brand advertising to predict the demand for a specific brand of antacid in a given store while taking into 
                       account time effects."),
                       tags$embed(style="height:800px; width:100%; scrolling=yes",
                                  src="Own_Comp_Adv_Effects.pdf#toolbar=0")),
              tabPanel("Understand Potential Market Share for Different TV Subscription Plans",
                       h1(""),
                       h5("Given a conjoint analysis survey for various TV subscription packages, generated a demand curve
                          for 3 different subscription plans that allowed us to understand market share and ideal price points
                          for the different plans."),
                       tags$embed(style="height:800px; width:100%; scrolling=yes",
                                  src="Willingness_to_Pay.pdf#toolbar=0")),
              tabPanel("Understanding Reasons for Conversion (Attribution)",
                       h1(""),
                       h5("Given customer-level data, generated a model to understand the path a customer takes to arrive at
                          a conversion. Broke out the different acquisition channels in terms of the effect they have on a 
                          customer converting."),
                       tags$embed(style="height:800px; width:100%; scrolling=yes",
                                  src="Attribution.pdf#toolbar=0"))
          
              
                  
                  
             
           )
  )
  
  
)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

server<-function(input,output,session) {
  
  capm_url <- a("Investopedia - CAPM", href="https://www.investopedia.com/terms/c/capm.asp")
  output$CAPM_link <- renderUI({
    tagList("URL link:", capm_url)
  })
  
  three_f_url <- a("Investopedia - Three Factor Model", href="https://www.investopedia.com/terms/f/famaandfrenchthreefactormodel.asp")
  output$Three_Fact_link <- renderUI({
    tagList("URL link:", three_f_url)
  })
  
  five_f_url <- a("French Data - Five Factor Model", href="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_5_factors_2x3.html")
  output$Five_Fact_link <- renderUI({
    tagList("URL link:", five_f_url)
  })
  
########################################################################################################
  
  stock_table<-reactive({
    date_range_start<-input$dateRange[1]
    date_range_finish<-input$dateRange[2]
    
    ticker <- input$selectTicker
    
    tick_idx = which(names(sp_list)==ticker)
    
    stock_header = sp_list[tick_idx][[1]][[1]]
    
    stock_xts = sp_list[tick_idx][[1]][[2]]
    
    stock_xts = stock_xts[stock_xts$Date >= date_range_start]
    
    stock_xts = stock_xts[stock_xts$Date <= date_range_finish]
    
    stock_xts
    
  })
  
  stock_header<-reactive({

    ticker <- input$selectTicker
    
    tick_idx = which(names(sp_list)==ticker)
    
    stock_header = sp_list[tick_idx][[1]][[1]]
    
    stock_header
    
  })
  
  stock_data<-reactive({
    
    ticker <- input$selectTicker
    
    tick_idx = which(row.names(stock_db)==ticker)
    
    stock_info = stock_db[tick_idx,]
    
    stock_info
    
  })
  
  #####################################################################################################
  #####################################################################################################
  
  compare_data<-reactive({
    
    ticker <- input$selectMultTickers
    
    tick_idx = which(row.names(stock_db) %in% ticker)
    
    compare_info = stock_db[tick_idx,]
    
    compare_info
    
  })
  
  #####################################################################################################
  #####################################################################################################
  output$stock_plot<-renderPlot({
    
    stock_table<- stock_table()
    
    stock_xts = stock_table
    
    ggplot(data = stock_xts, aes(x=Date, y = Adjusted))+
      geom_line(color = "#00AFBB", size = 2)
    
  })
  #####################################################################################################
  
  output$stock_ticker <- renderPrint({
    ticker = input$selectTicker
    ticker
  })
  
  #####################################################################################################
  
  output$date_start <- renderPrint({
    date_start = input$dateRange[1]
    date_start
  })
  
  ####################################################################################################
  
  output$date_finish <- renderPrint({
    date_finish = input$dateRange[2]
    date_finish
  })
  
  ####################################################################################################
  
  output$stock_table <- renderTable({
    stock_table<- stock_table()
    stock_table
  })
  
  output$stock_header <- renderTable({
    stock_header<- stock_header()
    stock_header
  })
  
  output$stock_info1 <- renderTable({
    stock_info1<- stock_data()
    stock_info1<-stock_info1[,c(1:10)]
    stock_info1
  })

  output$stock_info2 <- renderTable({
    stock_info2<- stock_data()
    stock_info2<-stock_info2[,c(11:20)]
    stock_info2
  })

  output$stock_info3 <- renderTable({
    stock_info3<- stock_data()
    stock_info3<-stock_info3[,c(21:30)]
    stock_info3
  })
  
  output$stock_info4 <- renderTable({
    stock_info4<- stock_data()
    stock_info4<-stock_info4[,c(31:40)]
    stock_info4
  })
  
  output$stock_info5 <- renderTable({
    stock_info5<- stock_data()
    stock_info5<-stock_info5[,c(41:50)]
    stock_info5
  })
  
  output$stock_info6 <- renderTable({
    stock_info6<- stock_data()
    stock_info6<-stock_info6[,c(51:60)]
    stock_info6
  })
  
  ###########################################################################################################################
  ###########################################################################################################################
  
  output$stock_compare <- renderTable({
    compare_data<- compare_data()
    ticker<-rownames(compare_data)
    compare_data<-cbind(ticker,compare_data)
    compare_data
  })
  
  output$compare_chart<-renderPlot({
    
    list_of_tickers = input$selectMultTickers
    
    column_name = input$selectChartField
    
    chart = table_converter(list_of_tickers,column_name)
    
    chart
    
  })
  
  output$returns_plot<-renderPlot({

    list_of_tickers = input$selectMultTickers
    
    date_range = input$dateRange2
    
    plot_val = input$selectChartField2

    chart = returns_wrangler(list_of_tickers, date_range, plot_val)
    
    chart
  })
  
#################################################################################################
#################################################################################################
  output$sector_pie<-renderPlot({
    
    col_name = input$selectPieField
    
    display_type = input$selectDisplayType
    
    chart = sector_display(display_type,col_name)
    
    chart
  })
  
  output$sector_table<-renderTable({
    
    display_type = input$selectDisplayType
    
    if(display_type == "Sum"){
      sector_sums
    }else if(display_type == "Average"){
      sector_avgs
    }else{
      sector_wtd_avgs
    }
    
    
  },
  digits = -4)
  
  output$indSector<-renderTable({
    
    sector = input$selectSector
    
    ind_sector = sector_list[[sector]]
  })
  
  ###################################################################################
  ###################################################################################
  
  output$ui <- renderUI({

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$selectModel,
           "CAPM" = selectInput("selectOutput", "Output Type",
                                  choices = c("Alpha","Mkt Beta"),
                                selected = "Alpha"),
           "Three Factor" = selectInput("selectOutput", "Output Type",
                                choices = c("Alpha","Mkt Beta", "HML Beta","SMB Beta"),
                                selected = "Alpha"),
           "Five Factor" = selectInput("selectOutput", "Output Type",
                                choices = c("Alpha","Mkt Beta", "HML Beta","SMB Beta","RMW Beta","CMA Beta"),
                                selected = "Alpha")
    )
  })
  
  output$heatmap_output<-renderPlot({
    model = input$selectModel
    display = input$selectOutput
    industry = input$selectIndustry
    months = input$selectMonths

    # model = "CAPM"
    # display = "Alpha"
    # industry = "Materials"
    # months = 40
    #test = paste0(model,display,industry,months)
    create_heatmap(industry, model, display, months)
    #test
  })
  
  output$plot_alpha <- renderPlot({
    model = input$selectModel
    display = input$selectOutput
    industry = input$selectIndustry
    months = input$selectMonths
    
    create_graph(industry, model, display, months)
  })
  
  }

shinyApp(ui=ui,server=server)

#devtools::install_github("rstudio/shinyapps")
