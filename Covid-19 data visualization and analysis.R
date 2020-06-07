#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(mongolite)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(rvest)
library(leaflet)
library(readxl)
library(shiny)
library(scales)
library(plotly)

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

# Web scrap ----
url<-("https://www.worldometers.info/coronavirus/")
coviddata <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]

# Web scrap2 ----
url<-("https://developers.google.com/public-data/docs/canonical/countries_csv")
countryinfo <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
countryinfo <- countryinfo %>% select(latitude, longitude, name) %>% 
    dplyr::rename(country = name)

# Historical Data Import ----
url<-("https://covid.ourworldindata.org/data/owid-covid-data.csv")
owid_covid_data <- url %>% 
    read_csv() 
owid_covid_data <- owid_covid_data %>% 
    dplyr::rename(country = location)

owid_covid_data$total_tests[is.na(owid_covid_data$total_tests)] <- 0
owid_covid_data$new_tests[is.na(owid_covid_data$new_tests)] <- 0
owid_covid_data$total_tests_per_thousand[is.na(owid_covid_data$total_tests_per_thousand)] <- 0
owid_covid_data$total_deaths_per_million[is.na(owid_covid_data$total_deaths_per_million)] <- 0


# Data Cleanning ----
coviddata <- coviddata %>% 
    dplyr::rename(country = `Country,Other`)
coviddata <- coviddata %>% mutate_all(na_if,"") %>% 
    mutate_all(~replace(., is.na(.), 0))
coviddata <- coviddata[!(coviddata$country %in% c("0")),]
coviddata <- coviddata %>% mutate(country = recode(country,
                                                   "USA" = "United States",
                                                   "S. Korea" = "South Korea",
                                                   "UK" = "United Kingdom"
))
coviddata <- left_join(coviddata, countryinfo, by = "country")
coviddata <- coviddata %>% mutate(popup_info = paste("Country:", country, "<br/>",
                                                     "TotalCases:", TotalCases, "<br/>",
                                                     "NewCases:", NewCases, "<br/>",
                                                     "ActiveCases:", ActiveCases, "<br/>",
                                                     "TotalDeaths:", TotalDeaths, "<br/>",
                                                     "TotalRecovered:", TotalRecovered))

owid_covid_data <- owid_covid_data %>% mutate(smokers = female_smokers + male_smokers)

coviddata$Population <- as.numeric(gsub(",", "", coviddata$Population))
coviddata$`Tests/1M pop` <- as.numeric(gsub(",", "", coviddata$`Tests/1M pop`))
coviddata <- coviddata %>% mutate(pop = Population / 1000000)

covidasia <- coviddata %>% 
    filter(Continent == "Asia")
covidasia <- covidasia[!(covidasia$country %in% c("Asia","Total:")),]
covidna <- coviddata %>% 
    filter(Continent == "North America")
covidna <- covidna[!(covidna$country %in% c("North America","Total:")),]



twosamplena <- covidna %>% select(pop, `Tests/1M pop`)
twosampleasia <- covidasia %>% select(pop, `Tests/1M pop`)
n<-c(39,49)
p<-2
x1<-colMeans(twosamplena)
x2<-colMeans(twosampleasia)

d<-x1-x2
S1<-var(twosamplena)
S2<-var(twosampleasia)
Sp<-((n[1]-1)*S1+(n[2]-1)*S2)/(sum(n)-2)
t2 <- t(d)%*%solve(sum(1/n)*Sp)%*%d
t2

alpha<-0.05
cval <- (sum(n)-2)*p/(sum(n)-p-1)*qf(1-alpha,p,sum(n)-p-1)
cval





# Data for polar diagram ----

scaled <- owid_covid_data %>% group_by(country) %>% filter(date == max(date))

scaled$total_cases_per_million<-round(rescale(scaled$total_cases_per_million, to = c(0, 100), 
                                         from = range(scaled$total_cases_per_million, finite = TRUE)), digits=2)

scaled$total_deaths_per_million<-round(rescale(scaled$total_deaths_per_million, to = c(0, 100), 
                                  from = range(scaled$total_deaths_per_million, finite = TRUE)), digits=2)

scaled$total_tests_per_thousand<-round(rescale(scaled$total_tests_per_thousand, to = c(0, 100), 
                                  from = range(scaled$total_tests_per_thousand, finite = TRUE)), digits=2)
scaled$population_density<-round(rescale(scaled$population_density, to = c(0, 100), 
                                  from = range(scaled$population_density, finite = TRUE)), digits=2)
scaled$aged_65_older<-round(rescale(scaled$aged_65_older, to = c(0, 100), 
                                  from = range(scaled$aged_65_older, finite = TRUE)), digits=2)
scaled$gdp_per_capita<-round(rescale(scaled$gdp_per_capita, to = c(0, 100), 
                                  from = range(scaled$gdp_per_capita, finite = TRUE)), digits=2)
scaled$median_age<-round(rescale(scaled$median_age, to = c(0, 100), 
                                     from = range(scaled$median_age, finite = TRUE)), digits=2)
scaled$diabetes_prevalence<-round(rescale(scaled$diabetes_prevalence, to = c(0, 100), 
                                 from = range(scaled$diabetes_prevalence, finite = TRUE)), digits=2)
scaled$male_smokers<-round(rescale(scaled$male_smokers, to = c(0, 100), 
                                 from = range(scaled$male_smokers, finite = TRUE)), digits=2)
scaled$female_smokers<-round(rescale(scaled$female_smokers, to = c(0, 100), 
                                 from = range(scaled$female_smokers, finite = TRUE)), digits=2)
scaled$population_density = scaled$population_density * 30

scaled <- scaled %>% select(country, total_cases_per_million, total_deaths_per_million,
                            median_age, diabetes_prevalence, population_density,
                            aged_65_older, gdp_per_capita, male_smokers, female_smokers)
attach(owid_covid_data)

data2 <- data.frame(country, population_density, aged_65_older, gdp_per_capita, cvd_death_rate, diabetes_prevalence, smokers, total_cases_per_million)
data2 <- data2 %>% left_join(coviddata, by = "country")
data2 <- data2 %>% select(Continent, country, population_density, aged_65_older, gdp_per_capita, cvd_death_rate, diabetes_prevalence, smokers, total_cases_per_million)
data2$population_density[is.na(data2$population_density)] <- 0
data2$aged_65_older[is.na(data2$aged_65_older)] <- 0
data2$gdp_per_capita[is.na(data2$gdp_per_capita)] <- 0
data2$cvd_death_rate[is.na(data2$cvd_death_rate)] <- 0
data2$diabetes_prevalence[is.na(data2$diabetes_prevalence)] <- 0
data2$smokers[is.na(data2$smokers)] <- 0
data2$total_cases_per_million[is.na(data2$total_cases_per_million)] <- 0
data2 <- data2 %>% filter(Continent != "All")
attach(data2)
data2.pc <- prcomp(data2[,-c(1:2)], center = TRUE, scale = TRUE)
str(data2.pc)
Y <- data2[, 7]
n1 <- length(Y)
Z <- cbind(rep(1,n1),as.matrix(data2[,1:6]))
r <- dim(Z)[2]-1



# Define UI for application that draws a histogram ----
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("Covid-19"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country1",
                        label = "Comparison Country1",
                        choices = unique(scaled$country),
                        selected = "United States"
            ),

            selectInput("country2",
                        label = "Comparison Country2",
                        choices = unique(scaled$country),
                        selected = "China"
            ),
            selectInput("time",
                        label = "Select a Country to See the Time Trend",
                        choices = unique(owid_covid_data$country),
                        selected = ""
            )
        ),
        
       
        mainPanel(
            
            navbarPage("Visualization",
                       tabsetPanel(
                           tabPanel("World Map", p("Try to click on the circle marker!"), leafletOutput("world_map")),
                           tabPanel("Comparison Plot", p("Scatter polar plot showing a comparison between your two selected countries 
                  Standarized on a scale of 0 to 100."), plotlyOutput("comparison")),
                           tabPanel("Time Trend", p("This trendline plot shows prevailing direction of cases for country you selected"), 
                                    plotOutput("trend")),
                           tabPanel("Diabetes", p("Compares the prevelance of Diabetes
                                              with the total deaths per million people"), 
                                    plotOutput("diabetes")),
                           tabPanel("Smokers", p("Represents the relation between death rate and number of smokers"), 
                                    plotOutput("smokers")),
                           tabPanel("Age65", plotOutput("age65"))
                           
                       )),
            navbarPage("Analysis", 
                       tabPanel("Multi-variate Statistics Analysis", 
                                p("based on the principles of multivariate statistics, which involves observation and 
                                  analysis of more than one statistical outcome variable at a time. "),
                                    p("# As we can see from above, there are 7 variables in total.") ,
                                    p("# Popultion_density variable represents the density of a country's population which is proportional for every country"),
                                    p("# aged_65_older is an index variable representing the proportion of the population of a country that is older than 65"),
                                    p("# gdp_per_Capita, another Z variable"),
                                    p("# cvd_death_rate is the death rate of cardiovascular disease for each country"),
                                    p("# diabetes_prevalence measures the prevalence of the diabetes"),
                                    p("# smokers include male and female smokers for each country"),
                                    p("# Total_cases_per_million is our Y variable here. "),
                                tableOutput("model")),
                       tabPanel("Principal Component Analysis",
                                p("A technique for reducing the dimensionality of such datasets, increasing interpretability but at the same time minimizing information loss. 
                                  It does so by creating new uncorrelated variables that successively maximize variance"),
                                plotOutput("PCA"),
                                p("Something interesting is happening here. We can tell from the plot that the European Countries 
                                  are characterized by high prevalence of Diabetes, old populations and larger amount of smokers, while 
                                  Asia has the most amount of CVD Death Rate.")),
                       tabPanel("Hotelling T-sqaure test between the difference of the total populations and the total tests in
                                North America and Asia", 
                                p("A two-dimentional generalization of a confidence interval, 
                                  with the origin represented by the mean of difference in total populations in millions between two continents and 
                                  the mean of difference in total tests / 1M population between two continents, and with the radius measured by 
                                  C times the square root of the eigenvalues"),
                                p("So for the Two-Sample Test, 
                                we are trying to proceed with the Hotelling T-sqaure test to test between the difference of the total populations and the total tests in North America and Asia. 
                                Because on the news, people are talking about how Asia, especially South Korea and China are doing way more tests than they are doing in North America, 
                                especially the United states. 
                                  So, here I am trying to find out if the claim is statistically true by 
                                  constructing a two-sample test between Asia and North America. "),
                                p("According to the test statistics, since 4.32 < 6.28, we do not reject the null and conclude that there are no statistical difference 
                                  in the volumns of the testings in proportion to the total populations between North America and Asia. "),
                                plotOutput("twosample")),
                                
                       tabPanel("Bonferroni simultaneous confidence intervals", 
                                p("This table output gives us information about the 95% confidence intervals of two pairwise comparisons."),
                                tableOutput("bonferroni"),
                                p("Both component-wise simultaneous confidence intervals do contain 0, so they do not have significant differences.
                                  So, the populations in Millions between Asia and North America 
                                  and the test per 1 Million Populations do not have significant differences. "))

            )

        )
)
)

# server ----
server <- function(input, output) {
    


    
    data1 <- reactive({
        filter(coviddata, Continent %in% c(input$continent1, input$continent2) 
               & country %in% c(input$country1, input$country2))
    })
    
    
    output$world_map <- renderLeaflet({
        coviddata1 <- coviddata %>% drop_na(latitude, longitude)
        map1 <- leaflet() %>% addTiles() %>% 
            addCircleMarkers(data = coviddata1, lat = ~latitude, lng = ~longitude, 
                             radius = ~4, popup = ~popup_info)
        map1
        
    })
    
    

    
    output$diabetes <- renderPlot({
        g2 <- owid_covid_data %>% ggplot(mapping = aes(x = diabetes_prevalence, y = total_deaths_per_million)) +
            geom_point(alpha = 0.5, size = 1.0) +
            coord_cartesian(xlim = c(0, 8), ylim = c(0, 1000))
        
        g2

    })
    
    output$age65 <- renderPlot({
        g2 <- owid_covid_data %>% ggplot(mapping = aes(x = aged_65_older, y = total_deaths_per_million)) +
            geom_point(alpha = 0.5, size = 1.0) +
            coord_cartesian(xlim = c(0, 30), ylim = c(0, 800))
        
        g2


    })
    output$smokers <- renderPlot({
        g2 <- owid_covid_data %>% ggplot(mapping = aes(x = smokers, y = total_deaths_per_million)) +
            geom_point(alpha = 0.5, size = 1.0)
        
        g2
        
    })
    
    output$trend <- renderPlot({
        trendcountry <- owid_covid_data %>% 
            filter(country == input$time) %>% 
            arrange(country)
        g2 <- trendcountry %>% ggplot(mapping = aes(x = date, y = total_cases)) +
            geom_point(alpha = 0.5, size = 1.0) + geom_smooth(color = 'purple') 
        
        g2
        
    })
    
    
    #Data for polar diagram comparing countries
    scatterpolar_data1 <- reactive({data11 <- scaled %>% filter(country == input$country1) %>% ungroup() %>% select(names(scaled[2:10]))})
    scatterpolar_data2 <- reactive({data12 <- scaled %>% filter(country == input$country2) %>% ungroup() %>% select(names(scaled[2:10]))})
    #Plot of polar diagram
    output$comparison <- renderPlotly({
        plot_ly(
            type = 'scatterpolar',
            mode='markers',
            fill = 'toself') %>% 
            add_trace(
                name = input$country1,
                r = as.numeric(scatterpolar_data1()[1,]),
                theta = names(scatterpolar_data1())) %>%
            add_trace(
                name = input$country2,
                r = as.numeric(scatterpolar_data2()[1,]),
                theta = names(scatterpolar_data2())) %>%
            layout(polar = list(
                radialaxis = list(
                    visible = T,
                    range = "auto")), showlegend = F)
        
        })
    
    output$twosample <- renderPlot({
        
        # Confidence Region
        es<-eigen(sum(1/n)*Sp)
        e1<-es$vec %*% diag(sqrt(es$val))
        r1<-sqrt(cval)
        theta<-seq(0,2*pi,len=250)
        v1<-cbind(r1*cos(theta), r1*sin(theta))
        pts<-t(d-(e1%*%t(v1)))
        plot(pts,type="l",main="Confidence Region for Bivariate Normal",
             xlab="Populations in Millions",
             ylab="total tests / 1M population",
             #asp=1,
             xlim=c(-250,100),
             ylim=c(-30000,15000)
            )
        segments(0,d[2],d[1],d[2],lty=2) # highlight the center
        segments(d[1],0,d[1],d[2],lty=2)
        
        th2<-c(0,pi/2,pi,3*pi/2,2*pi)   #adding the axis
        v2<-cbind(r1*cos(th2), r1*sin(th2))
        pts2<-t(d-(e1%*%t(v2)))
        segments(pts2[3,1],pts2[3,2],pts2[1,1],pts2[1,2],lty=3)
        segments(pts2[2,1],pts2[2,2],pts2[4,1],pts2[4,2],lty=3)
    })
    
    output$bonferroni <- renderTable({
        wd.b<- qt(1-alpha/(2*p),n[1]+n[2]-2) *sqrt(diag(Sp)*sum(1/n))
        Cis.b<-cbind(d-wd.b,d+wd.b)
        cat("95% Bonferroni simultaneous confidence interval","\n")
        Cis.b <- data.frame(t(Cis.b))
        Cis.b <- Cis.b %>% dplyr::rename("Populations in Millions" = pop, 
                         "Tests per 1 Million Populations" = Tests.1M.pop)
        Cis.b
    })
    
    output$model <- renderTable({
        as.data.frame(head(data2))
        
    })
    
    output$PCA <- renderPlot({
        
        ggbiplot(data2.pc,ellipse = TRUE, groups = data2$Continent)
    })
    
}
    
    


# Run the application 
shinyApp(ui = ui, server = server)


















