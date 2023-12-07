setwd("C:/Users/taylor/Desktop")

library(dplyr)
library(shiny)
library(bslib)
library(ggplot2)
library(cowplot)
library(shinydashboard)
library(shinyWidgets)
library(DT)

acnh <- read.csv("acnh_data_cleaned.csv", header = T, sep = ",")

acnh$A3 <- factor(acnh$A3,levels = c("Primary school", "Secondary school","High school","Undergraduate school","Graduate school and higher"))
acnh$A1_2 <- factor(acnh$A1_2,levels = c("US/Canada","Asia","EU","Other"))
acnh$B2 <- factor(acnh$B2,levels = c("1 day","3 days","4 days","5 days","6 days","7 days","More than a week","More than 2 weeks","More than 3 weeks","More than a month","No self-isolation/social distancing"))
acnh$B3 <- factor(acnh$B3,levels = c("Not at all","A little","Worried","Very worried"))

acnh <- rename(acnh, "Catching Bugs" = E1, "Fishing" = E2,
               "Planting Trees or Flowers" = E3, "Terraforming" = E4,
               "Mystery Island Tours" = E5,"Feeling Content" = F1,
               "Feeling Happy" = F6, "Feeling Bad" = F7, "Feeling Bored" = F16,
               "We are reaching the limit of people the earth can support" = C1,
               "When humans interfere with nature it produces disastrous consequences" = C3,
               "Humans are seriously abusing the environment" = C5,
               "Plants and animals have as much right as humans to exist" = C7,
               "Despite our special abilities, humans are still subject to the laws of nature" = C9,
               "The Earth is like a spaceship with very limited room and resources" = C11,
               "The balance of nature is very delicate and easily upset" = C13,
               "If things continue on their present course, we will soon experience a major ecological catastrophe" = C15)

cor_1 <- acnh[30:34]
cor_2 <- select(acnh,"Feeling Content","Feeling Happy","Feeling Bad","Feeling Bored")
cor_3 <- select(acnh,"We are reaching the limit of people the earth can support",
                "When humans interfere with nature it produces disastrous consequences",
                "Humans are seriously abusing the environment",
                "Plants and animals have as much right as humans to exist",
                "Despite our special abilities, humans are still subject to the laws of nature",
                "The Earth is like a spaceship with very limited room and resources",
                "The balance of nature is very delicate and easily upset",
                "If things continue on their present course, we will soon experience a major ecological catastrophe")

data1 <- acnh[11]
CORB3 <- data.frame(lapply(data1,as.numeric))
colnames(CORB3) <- c("Level of COVID-19 Concern")

ui <- navbarPage("Animal Crossing New Horizons During COVID-19",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  tags$style(HTML("
      .table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: rgba(243,150,154,255)!important;}
      ")),
  setBackgroundColor(
    color = c("#f8f9fa", "#a7d7c9"),
    gradient = "linear",
    direction = "bottom"),
  tabPanel(title = "Home",
           box(status = "primary",
               solidHeader = F,
               collapsible = F,
               width = 10,
               fluidRow(style = "font-size:.95em",
                        column(width = 5,
                               p("Animal Crossing New Horizons (ACNH) is the fifth entry in a popular video game series developed and published by Nintendo.  
                                 In ACNH, the player can do many different actions such as planting flowers, fishing, collecting bugs, decorating their house, 
                                 and creating friendships with the various animal inhabitants of the island.  Already a wildly popular franchise for Nintendo, the game saw
                                 a staggering increase in popularity due to it releasing alongside the beginning of the COVID-19 pandemic.  The game offered a way for people to unwind and
                                 socialize with friends despite the conditions of the world at the time.  The game has been considered relaxing, fun, and enjoyable for individuals of all
                                 ages and backgrounds."),
                               p(HTML("<p>The data in this project was obtained <a href='https://www.scidb.cn/en/detail?dataSetId=cb5d36cce29f4e5695a586c9b85d04b6'>here</a>. Additionally,
                                      the original article that utilized the dataset can be found <a href='https://direct.mit.edu/dint/article/3/4/606/107672/A-Multinational-Data-Set-of-Game-Players-Behaviors'>here</a>.
                                      The researchers' original intent was to examine player environmental worldviews, behaviors, and general wellbeing as they relate to COVID-19 opinions.  I will present general demographic
                                      data as well as a few different correlations that I found interesting.</p>"))),
                        column(width = 5, align = "center",
                               img(src="https://animalcrossingworld.com/wp-content/uploads/2020/02/animal-crossing-new-horizons-key-artwork-january-2020-9000wide.jpg",height="450rem"))))),
  navbarMenu("Demographic Information",
             tabPanel("Gender",div(plotOutput("hist_gender",width = "75%"),align = "center")),
             tabPanel("School Level", div(plotOutput("hist_ed",width = "75%"),align = "center")),
             tabPanel("Region", div(plotOutput("hist_region",width = "75%"),align = "center")),
             tabPanel("Length of Isolation or Social Distancing", div(plotOutput("hist_b2",width = "75%"),align = "center")),
             tabPanel("Level of COVID-19 Concern", div(plotOutput("hist_b3",width = "75%"),align = "center")),
             tabPanel("Age", div(plotOutput("hist_age",width = "75%"),align = "center"))),
  navbarMenu("Correlations",
             tabPanel("Player Activities vs. Emotions", div(dataTableOutput("cortablebehaviorfeeling",width = "75%"),align = "center")),
             tabPanel(title = (HTML("COVID-19 Concern vs. Player Behavior,<br/> Feelings, and Thoughts About the Environment")), 
                      div(dataTableOutput("corconenviron",width = "75%"),align = "center"),
                      div(dataTableOutput("corconbehavior",width = "75%"),align = "center"),
                      div(dataTableOutput("corconfeeling",width = "75%"),align = "center"))
             ))

server <- function(input, output, session) {

  output$hist_gender <- renderPlot({
    ggplot(acnh, aes(x = A2, fill = A2)) +
      geom_bar(show.legend = F) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Gender of Players") +
      xlab("Gender") +
      ylab("Count") +
      ylim(NA,450) +
      scale_fill_manual(values=c("#78c2ad", 
                                 "#f3969a")) + 
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  }, bg = "transparent")
  
  output$hist_ed <- renderPlot({
    ggplot(acnh, aes(x = A3,fill = A3)) +
      geom_bar(show.legend = F) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Education Level of Players") +
      guides(fill=guide_legend(title="Level")) +
      xlab("Education Level") +
      ylab("Count") +
      ylim(NA,350) +
      scale_fill_manual(values=c("#78c2ad", 
                                 "#f3969a",
                                 "#56cc9d",
                                 "#6cc3d5",
                                 "#ffce67")) +
      scale_x_discrete(labels = c("Primary\nschool","Secondary\nschool","High\nschool","Undergraduate\nschool","Graduate school\nand higher")) +
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  },bg = "transparent")
  
  output$hist_region <- renderPlot({
    ggplot(acnh, aes(x = A1_2,fill = A1_2)) +
      geom_bar(show.legend = F) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Region of Players") +
      xlab("Region") +
      ylab("Count") +
      ylim(NA,350) +
      scale_fill_manual(values=c("#78c2ad", 
                                 "#f3969a",
                                 "#56cc9d",
                                 "#6cc3d5")) +
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  },bg = "transparent")
  
  output$hist_b2 <- renderPlot({
    ggplot(acnh, aes(x = B2,fill = B2)) +
      geom_bar(show.legend = F) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Length of Isolation and Social Distancing of Players") +
      xlab("Length of Time of Isolation and/or Social Distancing") +
      ylab("Count") +
      ylim(NA,450) +
      scale_fill_manual(values=c("#343a40",
                                 "#8f5443",
                                 "#daa879",
                                 "#ccb2aa",
                                 "#78c2ad", 
                                 "#f3969a",
                                 "#56cc9d",
                                 "#6cc3d5",
                                 "#ffce67",
                                 "#ff7851",
                                 "#f396c6")) +
      scale_x_discrete(labels = c("1 day","3 days","4 days","5 days","6 days","7 days","More than\naweek","More than\n2 weeks","More than\n3 weeks","More than\na month","No\nself-isolation\nor social\ndistancing")) +
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  },bg = "transparent")
  
  output$hist_b3 <- renderPlot({
    ggplot(acnh, aes(x = B3,fill = B3)) +
      geom_bar(show.legend = F) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Level of COVID-19 Concern") +
      xlab("Level of Concern") +
      ylab("Count") +
      ylim(NA,350) +
      scale_x_discrete(labels = c("Not at all","A little","Worried","Very worried")) +
      scale_fill_manual(values=c("#78c2ad", 
                                 "#f3969a",
                                 "#56cc9d",
                                 "#6cc3d5")) +
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  },bg = "transparent")
  
  output$hist_age <- renderPlot({
    ggplot(acnh, aes(x = A5, y = after_stat(count), fill = after_stat(count))) +
      geom_bar(show.legend = F) +
      scale_fill_gradient(low = "#6cc3d5",high = "#56cc9d") +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
      labs(title = "Age of Participants") +
      xlab("Age (In Years)") +
      ylab("Count") +
      ylim(NA,65) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme_minimal(base_size = 20) +
      theme(axis.title = element_text(size = 20))
  },bg = "transparent")
  
  output$cortablebehaviorfeeling <- DT::renderDataTable({
    datatable(caption = 'In the original survey, players were asked to rate how often they performed certain tasks (for example fishing or catching bugs), 
    with lower value responses representing doing an action less often.  Additionally, 
              they were asked to rate their feelings when playing the game, with higher scores indicating a stronger feeling. The data seems to indicate that players
              feel more happy and content when fishing or catching bugs, and more bored and bad when terraforming or going on mystery island tours.  However, these correlations are rather weak.',
              style = "bootstrap",
              round(cor(cor_1,
                        cor_2),2),options = list(info = FALSE, searching = FALSE, paging = FALSE,columnDefs = list(list(className = 'dt-center', targets = 0:4))))
    
  })
  
  output$corconenviron <- DT::renderDataTable({
    datatable(caption = 'Players were asked to rate their level of COVID-19 concern, with "Not at all" being the lowest value and "Very worried" being the highest value.  Additionally,
              players were asked to answer eight 5-point Likert scale environmental concern questions (where 1 represents strong disagreement and 5 represents strong agreement).  This data implies that
              players with higher levels of COVID-19 concern tend to feel like we as humans are on the path to ecological destruction and that humans have negative impacts 
              on nature.  Like the previous correlations, these are also weak.',
              style = "bootstrap",
              round(cor(CORB3,
                        cor_3),2),options = list(info = FALSE, searching = FALSE, paging = FALSE,columnDefs = list(list(className = 'dt-center', targets = 0:7))))
    
  })
  
  output$corconbehavior <- DT::renderDataTable({
    datatable(caption = 'This table examines the correlations between the previously mentioned "Level of COVID-19 Concern" as well as player tasks.  This data was interesting to me 
              because I did not expect there to be any sort of relationship (despite the weakness) between COVID concern and mystery island tours, but it is the highest value on the table.  Perhaps 
              a possible explanation for this could be that players with high levels of concern tended to make up for traveling in real life with traveling in the game.',
              style = "bootstrap",
              round(cor(CORB3,
                        cor_1),2),options = list(info = FALSE, searching = FALSE, paging = FALSE,columnDefs = list(list(className = 'dt-center', targets = 0:4))))
  
  })

  output$corconfeeling <- DT::renderDataTable({
    datatable(caption = 'Finally, this table examines the correlations between the previously mentioned "Level of COVID-19 Concern" as well as player emotional state.  At first these results seemed surprising,
     but after thinking about it, they do make sense.  If you have a high level of COVID concern, you are probably still feeling bad or bored, even while playing a game.',
              style = "bootstrap",
              round(cor(CORB3,
                        cor_2),2),options = list(info = FALSE, searching = FALSE, paging = FALSE,columnDefs = list(list(className = 'dt-center', targets = 0:3))))

  })
  }
  
shinyApp(ui, server)