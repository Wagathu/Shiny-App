

source("Global.R")
ui <- fluidPage(tabsetPanel
                (
                  tabPanel("Kenya Overview",
                           br(),
                           br(),
                           fluidRow(
                             column(6, tags$image(
                               src = "Flag.jpeg",
                               height = "350px",
                               width = "450px"
                             )
                             ),
                             column(6,
                                    p(h4(strong("History of Kenya"))),
                                    p(("The independent Republic of Kenya was formed in 1963.
                                             This page shows the various demographic charachteristics 
                                             and the way they change over time."
                                       
                                    )),
                                    br(),
                                    
                                    p(h4(strong("Brief Pre - Colonial History"))),
                                    p(("Around 2000 BC, Cushitic-speaking people from northern Africa settled 
                                             in the part of East Africa that is now Kenya. By the 1st Century AD,
                                             the Kenyan coast was frequented by Arab traders, who due to Kenya's 
                                             proximity to the Arabian Peninsula, established Arab and Persian colonies there.
                                             The Nilotic and Bantu people also moved into the region during 
                                             the first millennium AD. and settled inland. Evolving from a mixture of Bantu 
                                             and Arabic, the Swahili language then developed as a lingua franca for trade 
                                             between the different peoples. When the Portuguese arrived in 1498, 
                                             the Arab dominance on the coast was clipped, as the Port of Mombasa became 
                                             an important resupply stop for ships bound for the Far East. The Portuguese 
                                             gave way in turn to Islamic control under the Imam of Oman in the 1600s until 
                                             another European influence came along, this time from the United Kingdom 
                                             during the 19th century."))
                             )),
                           
                           hr(),
                           
                           
                           fluidRow(
                             column(12,
                                    
                                    p(h4(strong("Colonial History"))),
                                    p(("The roots of the colonial history of Kenya go back to the Berlin 
                               Conference in 1885, when East Africa was first divided into territories
                               of influence by the European powers. The British Government founded the 
                               East African Protectorate in 1895 and soon after, opened the fertile highlands
                               to white settlers. Even before it was officially declared a British colony 
                               in 1920, these settlers were allowed a voice in government, while the
                               Africans and the Asians were banned from direct political participation 
                               until 1944. During this period thousands of Indians were brought into 
                               Kenya to work on building the Kenya Uganda Railway Line and subsequently
                               settled there, whilst inviting many of their kith and kin who were mainly
                               traders from India to join them.")),
                                    br()
                             )
                           )   ),
                  tabPanel("Kenya Sceneries and Counties",
                           br(),
                           br(),
                           fluidRow(
                             panel(status = "primary",
                                   column(4,
                                          p(strong(
                                            "There are 47 Counties in Kenya as shown in the map
                                     below"
                                          )),
                                          br(),
                                          tags$image(
                                            src = "counties.jpeg",
                                            height = "350px",
                                            width = "350px"
                                          ),
                                          br(),
                                          br(),
                                          
                                          p(
                                            strong(
                                              "The 47 counties have different population due to different
                             geographical and economic factors. The plots below 
                             shows the different population in head counts."
                                            )
                                          ),
                                          br(),
                                          
                                          br(),
                                          
                                          
                                   ),
                                   
                                   br(),
                                   br(),
                                   br(),
                                   column(1),
                                   column(7,
                                          sidebarLayout( 
                                            p("Kenya's capital and largest city is Nairobi, while its oldest,
                                   currently second largest city, and first capital is the coastal 
                                   city of Mombasa. Kenya has considerable land area devoted to wildlife habitats, 
                                   including the Masai Mara, where blue wildebeest and other bovids 
                                   participate in a large-scale annual migration. 
                                   More than one million wildebeest and 200,000 zebras 
                                   participate in the migration across the Mara River. The Big Five
                                   game animals of Africa, that is the lion, leopard, buffalo, rhinoceros,
                                  and elephant, can be found in Kenya and in the Masai Mara in particular.
                                  A significant population of other wild animals, reptiles, 
                                  and birds can be found in the national parks and game reserves 
                                                         in the country"),
                                            mainPanel(slickROutput("slickr")
                                            )
                                          )),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   hr(),
                                   br()
                                   
                             ),
                             fluidRow(
                               
                               column(12,
                                      panel(heading = h2(strong("Population of the counties")), status = "primary",
                                            sidebarLayout(
                                              sidebarPanel(    
                                                actionButton( "countyd","Generate Plot", class = "btn-success")
                                              ),
                                              mainPanel(plotlyOutput("p1"))))
                               )
                             ),
                             br(),
                             hr(),
                             br(),   
                             fluidRow(
                               column(12,
                                      panel(status = "primary", heading = h2(strong("Population of the counties According to
                                                                                Gender")),     
                                            sidebarLayout(
                                              
                                              sidebarPanel (width = 3,
                                                            
                                                            selectInput(
                                                              inputId = "counties",
                                                              label = "Select your demographic variable", 
                                                              choices = names(counties[,2:4])  
                                                            ),
                                                            actionButton("Plot", "Generate Plot", class = "btn-success"),
                                                            
                                                            br(),
                                                            br()
                                              ),
                                              
                                              mainPanel(plotlyOutput("pl1")))))
                               
                             )
                           ) ),
                  
                  tabPanel("Kenya Population Demographics",
                           fluidRow(
                             p(h4(("The Population of Kenya as well as other population charachteristics
                                       have been changing as shown in the plots below"))),
                             column(6,
                                    br(),
                                    panel(heading = p(strong("Population")), status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "pop","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p2")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Fertility Rate")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Fertility","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p4")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Life Expectancy")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Life","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p6")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Per Capita Income")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Capita","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p8")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Unemployment Rate")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Unemployment","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p10")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("CO2 Emmision")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "CO","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p12"))))
                                    
                                    
                             ),
                             
                             column(6,
                                    br(),
                                    panel(heading = p(strong("Population Growth Rate")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "growth","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p3")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Mortality Rate")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Mortality","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p5")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Gross Domestic Product")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Gdp","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p7")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("Infant Mortality Rate")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Infant","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p9")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("% of GDP spent on Education")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Education","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p11")))),
                                    br(),
                                    hr(),
                                    br(),
                                    panel(heading = p(strong("% of Population with Electricity Access")),status = "primary",
                                          sidebarLayout(
                                            sidebarPanel(
                                              actionButton( "Electricity","Generate Plot", class = "btn-success")
                                              
                                            ),
                                            mainPanel(plotlyOutput("p13"))))
                                    
                             )
                           )
                  ),
                  tabPanel("Summary",
                           p(strong("Population of Counties is as follows according to census 2019")),
                           br(),
                           fluidRow(dataTableOutput("tbl"))
                  )
                )
                
)        

server <- function(input, output) {
  
  is_mobile_device <- reactive(isTRUE(input$is_mobile_device))
  
  output$tbl <- renderDataTable({
    df <- counties
    df
  })
  
  output$slickr <- renderSlickR({
    
    slickR(imgs,
           height = "300px",
           width = "3300px",
    )+ settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 2000)
  }) 
  
  observeEvent( input$countyd,{
    
    output$p1 <- renderPlotly({
      p1 <- counties |>
        ggplot(aes(x = County)) + 
        geom_col(aes(y = Total) ,fill = "royalblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = .6), axis.ticks = element_line()) +
        labs(title = "Kenya Population by Counties") +
        xlab("County") +
        ylab("Head Count")
      ggplotly(p1)
      
    })
  })
  
  observeEvent(input$Plot,{
    output$pl1 <- renderPlotly({
      pl1 <-counties |> 
        ggplot(aes(x = County)) +
        geom_col(aes(y= .data[[input$counties]]), fill = "navyblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = .6), axis.ticks = element_line()) +
        labs(title = "Kenya Population by Counties") +
        xlab("County") +
        ylab("Specified Gender Population")
      ggplotly(pl1)
    })
  })
  
  observeEvent(input$pop,{
    
    output$p2 <- renderPlotly({
      p2 <- population |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = Population), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Kenya Population Since 1950") +
        xlab("Year") +
        ylab("Head Count")
      ggplotly(p2)
    })
  })
  
  observeEvent(input$growth,{
    output$p3 <- renderPlotly({
      p3 <- population |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Growth Rate`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Kenya Population Growth Rate Since 1950") +
        xlab("Year") +
        ylab("Growth Rate in %")
      ggplotly(p3)
      
    })
  })
  
  observeEvent(input$Fertility, {
    output$p4 <- renderPlotly({
      
      p4 <- Fertility |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Fertility Rate`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Kenya Ferility Rate Since 1950") +
        xlab("Year") +
        ylab("Ferility Rate in %")
      ggplotly(p4)
      
    })  
    
  })
  
  observeEvent(input$Mortality,{
    output$p5 <- renderPlotly({
      p5 <- Mortality |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Death Rate`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Kenya Mortality Rate Since 1950") +
        xlab("Year") +
        ylab("Mortality Rate in %")
      ggplotly(p5)
    })
    
    
  })
  
  observeEvent(input$Life,{
    output$p6 <- renderPlotly({
      p6 <- Expectancy |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Life Expectancy`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Life Expectnacy Since 1950") +
        xlab("Year") +
        ylab("Life Expectancy in %")
      ggplotly(p6)
    })
  })
  
  observeEvent(input$Gdp,{
    output$p7 <- renderPlotly({
      p7 <- Gdp |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `GDP(Billions)`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "GDP(Billions) Since 1960") +
        xlab("Year") +
        ylab("GDP(Billions)")
      ggplotly(p7)
      
    })
    
  })
  
  observeEvent(input$Capita,{
    output$p8 <- renderPlotly({
      p8 <- Gdp |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Per Capita`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Per Capita income Since 1960") +
        xlab("Year") +
        ylab("Per Capita")
      ggplotly(p8)
      
    })
  })
  
  observeEvent(input$Infant,{
    output$p9 <- renderPlotly({
      p9 <- infantM |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Infant Mortality Rate`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Infant Mortality Since 1950") +
        xlab("Year") +
        ylab("Infant Mortality")
      ggplotly(p9)
    })
  })
  
  observeEvent(input$Unemployment,{
    output$p10 <- renderPlotly({
      p10 <- Unemployment |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Unemployment Rate (%)`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Unemployment Rate Since 1991") +
        xlab("Year") +
        ylab("Unemployment Rate in %")
      ggplotly(p10)
    })
  })
  
  observeEvent(input$Education,{
    output$p11 <- renderPlotly({
      p11 <- Education |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Education Spending (% of GDP)`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "Education Spending (% of GDP) Since 1982") +
        xlab("Year") +
        ylab("Spending")
      ggplotly(p11)
      
    })
  })
  
  observeEvent(input$CO,{
    output$p12 <- renderPlotly({
      p12 <- Co2 |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `Kilotons of Co2`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "CO2 Emmision(in Kilotons) In Kenya Since 1960") +
        xlab("Year") +
        ylab("CO2")
      ggplotly(p12)
    })
  })
  
  observeEvent(input$Electricity,{
    output$p13 <- renderPlotly({
      p13 <- Electricity |>
        ggplot(aes(x = Year)) + 
        geom_line(aes(y = `% of Population`), col = "royalblue", size = .7) +
        theme_minimal() +
        labs(title = "% of Population with Electricity Access In Kenya Since 1993") +
        xlab("Year") +
        ylab("CO2")
      ggplotly(p13)
    })
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
