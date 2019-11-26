library(shiny)
library(tidyverse)
#load(file = "Funcionarios.Rdata")

 library(rvest)
 url <- "http://ldap.ine.pt/query_sem_autent.php"
 pagina <- read_html(url)
 funcionarios<-as.data.frame(pagina %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = T)) %>% 
   select(X2,X3,X4) %>% 
   rename (Nome=X2, UO=X3,Local=X4) %>%
   slice(18:n())

ui <- fluidPage(
    
    # Application title
    titlePanel("Distribuição de funcionários por localização"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("local",
                        "Local :",
                        choices = funcionarios %>% distinct(Local),
                        selected = 1
                        ),
            plotOutput("pie")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            textInput("pesquisa", "Pesquisa", ""),
            tableOutput("tabela")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pie<- renderPlot({funcionarios %>% ggplot(aes(x = 1, fill = as.factor(Local))) +
                                    geom_bar() +
                                    coord_polar(theta = "y")+
                                    labs(fill="Local", title = "Distribuicao Geral")+
                                    theme_void()
    })
    output$distPlot <- renderPlot({
        funcionarios %>% filter(Local==input$local)%>% ggplot() + 
            geom_bar(aes(x=UO, fill=UO), color= "white",show.legend = F)+ coord_flip() +theme_minimal()
                        # geom_text(aes(label=), vjust=2,size=5,color="white")
        
    })
    output$tabela<- renderTable(funcionarios %>% filter(Local==input$local)%>% filter(str_detect(Nome,input$pesquisa)) %>% select(Nome,UO) %>% arrange(UO))
}

# Run the application 
shinyApp(ui = ui, server = server)
