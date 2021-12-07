# importing library plotrix for pie3D()
#install.packages('plotrix')
#install.packages('dplyr')
#install.packages('bs4Dash')
#install.packages('shiny')
#install.packages('DT')
#install.packages('ggplot2')
#install.packages('shinyjs')
#install.packages('car')
#install.packages('caret')
#install.packages('factoextra')
#install.packages('shinycssloaders')

library(plotrix)
library(dplyr)
library(bs4Dash)
library(shiny)
library(DT)
library(ggplot2) 
library(shinyjs)
library(car)
library(caret)
library(factoextra)
library(shinycssloaders)

data <- read.csv("./datasets/marketing_campaign.csv",sep='\t')

##############---Pre-Processing---###############

#Remove value outline
data$Age <- 2021-data$Year_Birth
education<-factor(data$Education)

#Deleting the outliers in the data
data<-subset(data, Income!=666666)
data<-subset(data,Age<90)

#Creating range Agae 
data<-data %>%
  mutate(rangeAge=cut(data$Age,c(24,35,50,65,Inf)))
#Color

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

#Total_accept_cmp
data['accepted']=data$AcceptedCmp1+data$AcceptedCmp2+data$AcceptedCmp3+data$AcceptedCmp4+data$AcceptedCmp5

#Total Spent
data$Total_Spent<-data$MntWines+data$MntSweetProducts+data$MntMeatProducts+data$MntGoldProds+data$MntFruits+data$MntFishProducts

#Average consumption by education level
statistical_Edu_Purchases <- data %>%
  group_by(Education) %>%
  summarize(n=n(), mean_wines = mean(MntWines),
            mean_MntFruits = mean(MntFruits),
            mean_MntMeatProducts= mean(MntMeatProducts),
            mean_MntFishProducts = mean(MntFishProducts),
            mean_MntSweetProducts = mean(MntSweetProducts),
            mean_MntGoldProds = mean(MntGoldProds),
  )
#Average consumption by marital status level
statistical_Marital_Purchases <- data %>%
  group_by(Marital_Status) %>%
  summarize(n=n(), mean_wines = mean(MntWines),
            mean_MntFruits = mean(MntFruits),
            mean_MntMeatProducts= mean(MntMeatProducts),
            mean_MntFishProducts = mean(MntFishProducts),
            mean_MntSweetProducts = mean(MntSweetProducts),
            mean_MntGoldProds = mean(MntGoldProds),
  )

#Age
statistical_age <- data %>%
  group_by(rangeAge) %>%
  summarize(n=n(), sum = sum(Income),
            mean = mean(Income),
            sd = sd(Income),
            med = median(Income),
            q1 = quantile(Income, 0,25),
            q2 = quantile(Income, 0,5),
            q3 = quantile(Income, 0,75),
            IQR = q3-q1,
            min = min(Income), max = max(Income),
            range = max-min, CV = sd*100/mean
            
  )



#####K-Means
df1=data[c(-1,-2,-6,-7,-8,-10,-11,-12,-13,-14,-15,-21,-22,-23,-24,-25,-27,-28)]
#Creating dummy variables for categorical variables.
dmy <- dummyVars(" ~ .", data = df1, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = df1))
glimpse(dat_transformed)
dfc=dat_transformed[c(7,8,13,14,15,16,17,18,19,20,21,25,26)]
fviz_nbclust(dfc,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)
set.seed(123)
km.res <- kmeans(dfc, 3, nstart = 10)
fviz_cluster(km.res, dfc, geom = "point",ellipse.type = "norm",repel = TRUE)
dfc2<- dfc
dfc2['cluster']=as.factor(km.res$cluster)
km.res
##############---Pre-Processing---###############

#########---Tab_1---###########
uiTab1 <-  fluidPage(
  
  #Row1
  fluidRow(
    column(4,
           box(
             width = 12,
             status = "primary",
             title = "Total Customers",
             textOutput("totalCustomer"),
             collapsed = TRUE
           )
    ),
    column(4,
           box(
             width = 12,
             title = "Total children in Home",
             status = "danger",
             textOutput("totalChildren"),
             collapsed = TRUE
           )
    ),
    column(4,
           box(
             width = 12,
             status = "success",
             title = "Total Teen in Home",
             textOutput("totalTeen"),
             collapsed = TRUE
           )
    )
  ),
  #Row2
  fluidRow(
    column(4,
           box(
             width = 12,
             status = "indigo",
             title = "Number of times customers shop at the website",
             textOutput("PurchaseWeb"),
             collapsed = TRUE
           )
    ),
    column(4,
           box(
             width = 12,
             title = "Number of times customers shop at the store",
             status = "pink",
             textOutput("PurchaseStore"),
             collapsed = TRUE
           )
    ),
    column(4,
           box(
             width = 12,
             title = "Number of times customers complain",
             status = "orange",
             textOutput("NumComplain"),
             collapsed = TRUE
           )
    )
  ),
  #Row3
  fluidRow(
    
    status = "primary",
    sidebarPanel(
      radioButtons(
        inputId="normchoice",
        label="What do you want to check?",
        choices = c("Education", "Marital_Status","Kidhome","Teenhome","Age"),
        selected = "Education"
      )
    ),
    mainPanel(
      box(
        width = 12,
        status = "orange",
        solidHeader=TRUE,
        tabsetPanel(
          tabPanel("BarPlot", plotOutput("plot")), 
          tabPanel("Boxplot",plotOutput("boxplot")),
          tabPanel("PieChart",plotOutput("pie")),
          tabPanel("Summary", verbatimTextOutput("summary")), 
          tabPanel("Table", tableOutput("table"))
        )
      )
    )
    
  ),
  tags$style(HTML("
    .tabbable > .nav {height:40px}
    .tabbable > .nav > li > a {background-color: transparent;   color:#1E909E;padding:10px}
    .tabbable > .nav > li> a[class=active] {background-color: #7D8C8C; color:white}
  ")),
)


############---Tab-2---##########
uiTab2 <- fluidPage(
  selectInput(
    inputId = "education",
    label = "Select education",
    choices = c("--All--",levels(education))
  ),
  sliderInput("t2Age", "Age than more:",
              min = min(data$Age), max = max(data$Age), value =  min(data$Age)),
  DT::dataTableOutput("data")
)
#########---Tab3---##########
uiTab3 <- fluidPage(
  box(
    width = 12,
    title="Statistics by education level and average use of products",
    tableOutput("product_purchase_edu")
  ),
  fluidRow(
    sidebarPanel(
      selectInput(
        inputId = "products",
        label = "Select product",
        choices = names(select(statistical_Edu_Purchases,-1,-2))
      )
    ),
    mainPanel(
      plotOutput("plotByProductAndEdu")
    )
  )
)

uiTab3_2 <- fluidPage(
  box(
    width = 12,
    title="Statistics by marital status and average use of products",
    tableOutput("product_purchase_marital")
  ),
  fluidRow(
    sidebarPanel(
      selectInput(
        inputId = "products2",
        label = "Select product",
        choices = names(select(statistical_Edu_Purchases,-1,-2))
      )
    ),
    mainPanel(
      plotOutput("plotByProductAndMarital")
    )
  )
)
############--Tab-4--##########
uiTab4<- fluidPage(
  fluidRow(
    column(6,
           box(
             width = 12,
             title = strong("Total number of times customers make purchases with discounts"),
             br(),textOutput("totalDeal")
           )
    ),
    column(6,
           box(
             width = 12,
             title = strong("Number of times customers buy products with discounts Plot"),
             plotOutput("plotDeal")
           )
    )
  )
)

#######---Tab5---#########
uiTab5<- fluidPage(
  fluidRow(
    column(4,
           box(
             width = 12,
             status = "primary",
             title = "Lowest Income",
             textOutput("minIncome")
           )
    ),
    column(4,
           box(
             width = 12,
             title = "Average income",
             status = "danger",
             textOutput("medianIncome")
           )
    ),
    column(4,
           box(
             width = 12,
             status = "success",
             title = "Highest Income",
             textOutput("maxIncome")
           )
    )
  ),
  fluidRow(
    
    status = "primary",
    sidebarPanel(
      radioButtons(
        inputId="normchoice_Income",
        label="You want to see the correlation between income and?",
        choices = c("Education", "Marital_Status","rangeAge"),
        selected = "Education"
      )
    ),
    mainPanel(
      box(
        width = 12,
        status = "success",
        solidHeader=TRUE,
        tabsetPanel(
          tabPanel("Box Plot", plotOutput("boxplot_income")), 
          tabPanel("Destiny Plot",plotOutput("density_income")),
          tabPanel("Histogram Plot",plotOutput("hist_income"))
        )
      )
    )
    
  )
)
######---Tab6---####
uiTab6<- fluidPage(
  fluidRow(
    sidebarPanel(
      useShinyjs(),
      strong("Linear Regession between Income and Toal Spent of Customers",style="margin-bottom:20px"),
      br(),
      actionButton("showhideSummary", "Show/Hide Summary"),
      verbatimTextOutput("summaryModelRegession"),
      style="background-color:white;padding:10px"
    ),
    mainPanel(
      box(
        width = 12,
        status = "danger",
        title = strong("Linear Regession Plot"),
        withSpinner(plotOutput("plotLinearIncomeSpent")),
        br(),
        "We have the equation:",strong("Total_Spent = -545.6 + 0.02219*Income")
      )
    )
  ),
  fluidRow(
    sidebarPanel(
      useShinyjs(),
      strong("Multiple Linear Regession with Spent"),
      br(),
      actionButton("showhideSummary2", "Show/Hide Summary"),
      verbatimTextOutput("summaryMultiLM"),
      style="background-color:white;padding:10px"
    ),
    mainPanel(
      box(
        width = 12,
        status = "warning",
        title = strong("Multiple Linear Regession Plot"),
        withSpinner(plotOutput("plotMultiLinearIncomeSpent")),
        br(),
        "We have the equation:",strong("Total_Spent = -235.67 + 0.01771*Income + 148.127*accepted + 48.86*NumDealsPurchases - 268.224*Kidhome - 230.163*Teenhome")
      )
    ),
    style="margin-top:20px"
  ),
  fluidRow(
    sidebarPanel(
      strong("K-Means Cluster",style="font-size:20px;margin-bottom: 20px"),
      selectInput(
        inputId = "clusterchoice",
        label = NULL,
        choices = c("Cluster Plot",names(dfc2[-14]))
      ),
      strong("Conclude",style="font-size:18px"),
      #Cluster 1
      br(),strong("Cluster 1:"),
      br(),"- Spent very low",br(),"- Median age in between  48-50 years",
      br(),"- Very low number of web purches, catalog and store purches",
      br(),"- Visit website frequently",
      #Cluster 2
      br(),strong("Cluster 2:"),
      br(),"- Spent average amount",
      br(),"- Median age above 52 years",
      br(),"- Highest number of web purches and store purches",
      br(),"- Visits the web site little",
      #Cluster 3
      br(),strong("Cluster 3:"),
      br(),"- Spent highest amount",
      br(),"- Median age in between 50-52 years",
      br(),"- Highest number of catalog purches",
      br(),"- Visit the web site rarely.",
      style="background-color:white;padding:10px"
    ),
    mainPanel(
      box(
        width = 12,
        status = "success",
        title = strong("K-Means Cluster"),
        withSpinner(plotOutput("plotCluster")),
        br(),
        "K-means clustering with 3 clusters of sizes",strong("1253, 601, 358"),
        br(),"Sum of squares is 89.7%."
      )
    ),
    style="margin-top:20px"
  )
)

#######---Main---##########
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Admin",
        color = "primary",
        image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab1",
          uiTab1
          
        ),
        tabItem(
          tabName = "tab2",
          uiTab2
          
        ),
        tabItem(
          tabName = "tab3",
          tabsetPanel(
            tabPanel("Education",uiTab3), 
            tabPanel("Marital Status",uiTab3_2)
          )
        ),
        tabItem(
          tabName = "tab4",
          uiTab4
        ),
        tabItem(
          tabName = "tab5",
          uiTab5
        ),
        tabItem(
          tabName = "tab6",
          uiTab6
        ),
        tabItem(
          tabName = "tab7",
          "Tab 7"
        )
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      inputId = "sidebarState",
      sidebarMenu(
        id = "sidebar",
        menuItem(
          text = "Dashboard",
          tabName = "tab1",
          icon = icon("address-card"),
          selected = TRUE
        ),
        menuItem(
          text = "Data Customers",
          tabName = "tab2",
          icon = icon("user")
        ),
        menuItem(
          text = "Consumer behaviour",
          icon = icon("smile"),
          startExpanded = FALSE,
          menuSubItem(
            text = "Products",
            tabName = "tab3",
            icon = icon("product-hunt")
          ),
          menuSubItem(
            text = "Promotion",
            tabName = "tab4",
            icon = icon("ad")
          )
        ),
        menuItem(
          text = "Incomes",
          tabName = "tab5",
          icon = icon("money-bill-alt")
        ),
        menuItem(
          text = "Analysis",
          tabName = "tab6",
          icon = icon("chart-line")
        )
      )
    ),
    controlbar = dashboardControlbar(
      skin = "light",
      sliderInput(
        inputId = "controller",
        label = "Update the first tabset",
        min = 1,
        max = 6,
        value = 1
      )
    ),
    footer = bs4DashFooter()
  ),
  
  
  server = function(input, output, session) {
    observe(print(input$sidebarItemExpanded))
    observe(print(input$sidebar))
    # update tabset1
    observeEvent(input$controller,
                 {
                   updateTabItems(
                     session,
                     inputId = "sidebar",
                     selected = paste0("tab", input$controller)
                   )
                 },
                 ignoreInit = TRUE
    )
    
    ###Dasboard
    output$totalCustomer <-renderText(nrow(data))
    output$totalChildren <-renderText(sum(data$Kidhome,rm.na=TRUE))
    output$totalTeen <-renderText(sum(data$Teenhome,rm.na=TRUE))
    output$PurchaseWeb <-renderText(sum(data$NumWebPurchases,rm.na=TRUE))
    output$PurchaseStore <-renderText(sum(data$NumStorePurchases,rm.na=TRUE))
    output$NumComplain <- renderText(sum(data$Complain,rm.na=TRUE))
    
    output$summary = renderPrint({
      summary(data[, input$normchoice])
    })
    output$plot = renderPlot({
      xx<-barplot(table(data[,input$normchoice]), col=sample(color, length(table(data[,input$normchoice]))),width = 0.85,xlab=input$normchoice )
      text(x = xx, y = table(data[,input$normchoice]), label = table(data[,input$normchoice]), pos = 3, cex = 1, col = "black", xpd=TRUE)
    })
    output$boxplot = renderPlot({
      boxplot(table(data[,input$normchoice]),col=sample(color,1))
    })
    output$pie = renderPlot({
      pie3D(table(data[,input$normchoice]),labels = names(table(data[,input$normchoice])))
    })
    output$table = renderTable({
      table(data[,input$normchoice])
    })
    
    
    
    ###Data_Customers
    output$data <- DT::renderDataTable(
      if(input$education=="--All--"){
        subset(data,Age>=input$t2Age)
      }else{
        subset(data[data$Education==input$education,],Age>=input$t2Age)
      },
      options = list(scrollX = TRUE),
      rownames = TRUE
    )
    
    
    
    ###Customer behavior
    ####-------------------------------------###
    #Education
    output$product_purchase_edu <- renderTable(
      statistical_Edu_Purchases,
      options = list(scrollX = TRUE)
    )
    output$plotByProductAndEdu <- renderPlot({
      y<-apply(round(statistical_Edu_Purchases[,input$products]), 2, as.numeric)
      xx<-barplot(apply(statistical_Edu_Purchases[,input$products], 2, as.numeric)~statistical_Edu_Purchases$Education,
                  col="#69b3a2",xlab="Education levels",ylab = input$products,
                  width=0.85, )
      text(x = xx, y, label = y, pos = 3, cex = 1, col = "black", xpd=TRUE)
    }
    )
    #Marital
    output$product_purchase_marital <- renderTable(
      statistical_Marital_Purchases,
      options = list(scrollX = TRUE)
    )
    output$plotByProductAndMarital <- renderPlot({
      y<-apply(round(statistical_Marital_Purchases[,input$products2]), 2, as.numeric)
      xx<-barplot(apply(statistical_Marital_Purchases[,input$products2], 2, as.numeric)~statistical_Marital_Purchases$Marital_Status,
                  col="#69b3a2",xlab="Education levels",ylab = input$products2,
                  width=0.85, )
      text(x = xx, y, label = y, pos = 3, cex = 1, col = "black", xpd=TRUE)
    }
    )
    ####-------------------------------------###
    
    
    
    ###Promotions
    output$totalDeal <-renderText(
      sum(data$NumDealsPurchases)
    )
    output$plotDeal <- renderPlot({
      xx<- barplot(table(data$NumDealsPurchases),col=sample(color, length(table(data$NumDealsPurchases))) )
      text(x = xx, y = table(data$NumDealsPurchases), label = table(data$NumDealsPurchases), pos = 3, cex = 1, col = "black", xpd=TRUE)
    })
    
    
    ####Income
    output$minIncome <- renderText(min(data$Income))
    output$medianIncome <- renderText(mean(data$Income))
    output$maxIncome <- renderText(max(data$Income))
    output$boxplot_income <-renderPlot({
      ggplot(data, aes(x=data[,input$normchoice_Income],y=Income,fill=data[,input$normchoice_Income]))+
        ylim(0,180000)+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)+xlab(input$normchoice_Income)+labs(fill = input$normchoice_Income)
      
    })
    output$density_income <-renderPlot(
      qplot(round(Income), data = data, geom = "density",  color =data[,input$normchoice_Income],linetype = data[,input$normchoice_Income])
    )
    output$hist_income <-renderPlot(
      qplot(Income, data = data, geom = "histogram", fill = data[,input$normchoice_Income])
      +labs(fill = input$normchoice_Income)
    )
    
    ####Anlysis
    observeEvent(input$showhideSummary,{
      toggle("summaryModelRegession")
    }
    )
    observeEvent(input$showhideSummary2,{
      toggle("summaryMultiLM")
    }
    )
    #Linear Regession
    output$summaryModelRegession <-renderPrint(
      {
        summary(lm(data$Total_Spent~data$Income))
      }
      
    )
    output$plotLinearIncomeSpent <- renderPlot({
      Sys.sleep(0.5) # system sleeping for 3 seconds for demo purpose
      plot(data$Total_Spent~data$Income,xlab="Income",ylab="Total Spent")
      abline(lm(data$Total_Spent~data$Income), col="red")
    }
    )
    #Multiple Linear Regession
    output$summaryMultiLM <- renderPrint({
      Sys.sleep(0.5) # system sleeping for 3 seconds for demo purpose
      summary(step(lm(Total_Spent~Income+accepted+NumDealsPurchases+Kidhome+Teenhome+Age,data=data)))
    }
    )
    output$plotMultiLinearIncomeSpent <-renderPlot({
      Sys.sleep(0.5) # system sleeping for 3 seconds for demo purpose
      modelMulti<- step(lm(Total_Spent~Income+accepted+NumDealsPurchases+Kidhome+Teenhome+Age,data=data))
      avPlots(modelMulti)
    })
    #Cluster
    output$plotCluster <- renderPlot(
      if(input$clusterchoice=="Cluster Plot"){
        fviz_cluster(km.res, dfc, geom = "point",ellipse.type = "norm",repel = TRUE)
      }else{
        ggplot(dfc2, aes(x=cluster,y=dfc2[,input$clusterchoice],fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)+ylab(input$clusterchoice)
      }
      
    )
    
  }
)


