library(shiny)
library(shinydashboard)
library(leaflet)
library(wordcloud2)
library(RColorBrewer)
library(threejs)
library(dplyr)

# find best way to store data in cloud and download it when executing app to guarantee updated data
datapath <- Sys.getenv("retrieved_data_path")
worldAIdata <- readRDS(datapath)

#subset and group counts per country
countryFreqs <- worldAIdata %>%
  select(country_lat, country_long, country)%>%
  filter(!is.na(country)) %>%
  group_by(across(c("country_lat", "country_long")))%>%
  summarise(freq = n())

worldAIdata$lat <- as.numeric(worldAIdata$lat)
worldAIdata$long <- as.numeric(worldAIdata$long)

#make urls clickable, add html href
worldAIdata <- worldAIdata %>% 
  mutate(url = paste0("<a href='https://", url,"' target='_blank'>", url,"</a>"))
#generate logical vector to pass to datatable escape optiion to not escape only url col
escapeVec <- rep(TRUE, length(colnames(worldAIdata)))
escapeVec[8] <- FALSE

#setup word cloud
hashtags <- worldAIdata$hashtags
hashtags <- hashtags[which(hashtags != "na")]
hashtags <- as.character(
    do.call("c",
            sapply(hashtags, strsplit, ",", USE.NAMES = F))
)
#cleanup hashtags
#TODO: find way to support other types of characters (kanji, arabic, etc)
hashtags <- gsub("<U.*>", NA,hashtags)
hashtags <- na.exclude(hashtags)

hashtags <- table(hashtags)
hashtags <- data.frame(hashtags)
hashtags <- hashtags[order(hashtags$Freq,decreasing = T),]

#create twitter icon to use as marker in the 2D map
tIcon <- makeIcon(iconUrl = "https://www.twitter.com/favicon.ico",
                  iconWidth = 10,
                  iconHeight = 10)

cols <- brewer.pal(3,"YlOrRd")
pal <- colorRampPalette(cols)

set.seed(42)
################
# Define UI
ui <- dashboardPage(

    dashboardHeader(
        # Application title
        title = "WorldAI"
    ),

    dashboardSidebar(
        
        sidebarMenu(
            id = "tabs",
            menuItem("AI in the world", tabName = "home", icon = icon("globe")),
            menuItem("Related hashtags", icon = icon("project-diagram"), tabName = "wordcloudtab"),
            menuItem("Other curiousities", icon = icon("user-astronaut"), tabName = "curiousities")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                "home",
                h1('United World of AI', style="text-align: center; color: #2986cc; font-family: Helvetica, sans-serif;"),
                hr(style = "border-bottom: 3px solid #073763;"),
                fluidRow(
                    column(
                      width = 6,
                      box(
                        title = "Where do you AI",
                        background = "light-blue",
                        leafletOutput("aimap", height = 650),
                        width = "100%"
                        )
                    ),
                    column(
                      width = 6,
                      box(
                        title = "How much do you AI",
                        background = "light-blue",
                        globeOutput("globe", height = 650),
                        width = "100%"
                      )
                    )
                ),
                hr(style = "border-bottom: 1px solid #073763;"),
                p('Note: using geocoder with open street map to retrieve locations. Some posts do not contain valid names or the ones indicated were not able to lead to valid countries. Invalid locations are set as Antarctida by default, but some locations may be misplaced.'),
                hr(style = "border-bottom: 1px solid #073763;"),
                h3('Data', style="text-align: center; color: #2986cc; font-family: Helvetica, sans-serif;"),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      div(DT::dataTableOutput("table_input"), style = "font-size:70%"),
                      width = "100%"
                    )
                  )
                )
            ),
            
            tabItem(
                "wordcloudtab",
                fluidRow(
                    box(
                        title = "Most related hashtags",
                        background = "light-blue",
                        wordcloud2Output("wordcloud", height = "750px"),
                        width = "100%"
                    )
                )
            ),
            
            tabItem(
                "curiousities",
                fluidRow(
                  
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$aimap <- renderLeaflet({
        leaflet(worldAIdata) %>%
            addTiles() %>%
            #addCircleMarkers(
            #    popup = paste("@", worldAIdata$screen_name,"\n", "<a href='https://", worldAIdata$url, "'>", worldAIdata$url, "</a>", sep = ""),
            #    stroke = TRUE, fillOpacity = 0.4, radius = 3
            #) %>%
            addMarkers(
              popup = paste("@", worldAIdata$screen_name,"\n", "<a href='https://", worldAIdata$url, "'>", worldAIdata$url, "</a>", sep = ""),
              icon = tIcon
              ) %>%
            setView(0, 0, zoom = 2)
    })
    
    output$wordcloud <- renderWordcloud2({
        wordcloud2(hashtags, size = 0.6, color = "random-dark")
    })
    
    output$globe <- renderGlobe({
      globejs(lat = countryFreqs$country_lat,
              long = countryFreqs$country_long,
              value = countryFreqs$freq,
              color = pal(10)[as.numeric(cut(countryFreqs$freq,breaks = 10))],
              bg = "#0d0d0d",
              atmosphere = TRUE,
              emissive = "#030210",
              rotationlat = 0.503598776,
              rotationlong = -1.941052068
              )
    })
    
    # Table interface
    output$table_input = DT::renderDataTable({
      DT::datatable(worldAIdata, selection='single', rownames=FALSE, filter="top",
                    options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE
                    ),
                    escape = escapeVec
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
