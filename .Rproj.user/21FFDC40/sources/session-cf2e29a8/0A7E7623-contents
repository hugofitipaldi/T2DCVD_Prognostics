library(tidyverse)
library(shiny)
library(plotly)
library(shinythemes)
library(igraph)
library(visNetwork)
library(ggflags)
library(maps)
library(sp)
library(maptools)
library(leaflet)
library(leaflet.extras)
library(rgeos)
library(gt)
library(shinydashboard)
library(shinybusy)

# Pre-processing -----------------------------------------------------------------------
# Histograms

total_cov <- readr::read_csv("../data/cova_total.csv")
traditional_cov <- readr::read_csv("../data/cova_traditional.csv")

total_cov <-  total_cov %>%
  dplyr::select(total_cov, Frequency) %>%
  dplyr::mutate(group = "Total (Traditional & Non-traditional)", color_fill = "lightblue",  color_line = "darkblue",
         xaxis_title =  "Number of Total Adjusted Covariates")

traditional_cov <-  traditional_cov %>%
  dplyr::select(total_cov, Frequency) %>%
  dplyr::mutate(group = "Traditional", color_fill = "red",  color_line = "darkred",
         xaxis_title =  "Number of Traditional Adjusted Covariates")

names(total_cov) <- c("count_cov", "n" , "group", "color_fill", "color_line", "xaxis_title")
names(traditional_cov) <- c("count_cov", "n" , "group", "color_fill", "color_line", "xaxis_title")

cov_df <- rbind(total_cov, traditional_cov)

covariates.options <- unique(cov_df$group)

# Network
result_test <- readr::read_csv("../data/network_data.csv")

bigram_graph <- result_test %>%
  igraph::graph_from_data_frame()

# Affiliations

affiliations <- readr::read_csv("../data/affiliations.csv")

affiliations_map <- affiliations
world_aff <- maps::map("world", fill=TRUE, plot=FALSE)

affiliations_map[affiliations_map$country_of_affiliation == "United Kingdom",]$country_of_affiliation <- "UK"
affiliations_map[affiliations_map$country_of_affiliation == "United States",]$country_of_affiliation <- "USA"

world_aff_map <- map2SpatialPolygons(world_aff, sub(":.*$", "", world_aff$names))
world_aff_map <- SpatialPolygonsDataFrame(world_aff_map,
                                          data.frame(country=names(world_aff_map),
                                                     stringsAsFactors=FALSE),
                                          FALSE)

cnt_aff <- affiliations_map$country_of_affiliation
target_aff <- subset(world_aff_map, country %in% cnt_aff)

affiliations_map$popup_info <- paste0("<b>",affiliations_map$country_of_affiliation,"</b>", "<br/>", round(affiliations_map$sum_country, 2), "%")
affiliations_map$quants <- ntile(affiliations_map$sum_country, 8)
affiliations_map2 <- merge(target_aff, affiliations_map, by.x = "country", by.y = "country_of_affiliation", duplicateGeoms = TRUE)
affiliations_map$sum_country
risk.bins <- c(0, 0.5, 1, 2, 3, 5, 8, 12, 17, 18)

pal <- colorBin(palette = c("#56B1F7", "#72B8DB", "#4A9FC4", "#3887B1", "#2D6F96","#24577A","#1B3F5F", "#132B43"),
                bins=risk.bins, na.color = 'lightgrey')

# Gender

gender_coauthors <- readr::read_csv("../data/All_authors_genderAPI_groups.csv")

gender_coauthors[gender_coauthors$gender == "female",]$gender <- "Female"
gender_coauthors[gender_coauthors$gender == "male",]$gender <- "Male"

pie_palette <- c("#1B9E77", "#D95F02")
study.options <- unique(gender_coauthors$BM_GN_RS)

# Origin participants

origin_df <- readr::read_csv("../data/origins_participants.csv")

origins_map <- origin_df
world_ori <- map("world", fill=TRUE, plot=FALSE)

origins_map <- origins_map %>%
  filter(!(Origin %in% c("Asia (not-specified)",
                         "Europe (not-specified)",
                         "Latin America (not-specified)",
                         "South America (not-specified)")))

origins_map[origins_map$Origin == "United Kingdom",]$Origin <- "UK"
origins_map[origins_map$Origin == "United States",]$Origin <- "USA"

world_ori_map <- map2SpatialPolygons(world_ori, sub(":.*$", "", world_ori$names))
world_ori_map <- SpatialPolygonsDataFrame(world_ori_map,
                                          data.frame(country=names(world_ori_map),
                                                     stringsAsFactors=FALSE),
                                          FALSE)

cnt_ori <- origins_map$Origin
target_ori <- subset(world_ori_map, country %in% cnt_ori)

origins_map$popup_info <- paste0("<b>",origins_map$country_of_affiliation,"</b>", "<br/>", round(origins_map$Proportion, 2), "%")
origins_map$quants <- ntile(origins_map$Proportion, 8)
origins_map2 <- merge(target_ori, origins_map, by.x = "country", by.y = "Origin", duplicateGeoms = TRUE)

# ancestry participants

ancestry_df <- readr::read_csv("../data/ancestry.csv")

# Traffic light
traffic_data <- readr::read_csv("../data/BM_GN_RS_qualitySCORESbyStudy.csv")

traffic_data <- traffic_data %>%
  dplyr::mutate_all(~ case_when(
    . == "hig" ~ "low",
    . == "low" ~ "hig",
    . == "med" ~ "med",
    TRUE ~ as.character(.)
  ))

new_metadata416 <- readr::read_csv("../data/new_metadata416.csv")

traffic_data <- traffic_data %>%
  merge(new_metadata416, by.x = "studynum", by.y = "studyNum")

traffic_data_BM <- traffic_data %>%
  filter(bm_GN_RS == "BM") %>%
  select(study_id, Author_Year, PointQual_Represent012,
         PointQual_Selection012,	PointQual_Exposure012,
         PointQual_OutcDef012,	PointQual_FUPduration012,
         PointQual_FUPlost012,	PointQual_TOTALcov012,
         PointQual_TOTALtrad012, BiasRiskTotal) # D-M

traffic_data_GN <- traffic_data %>%
  filter(bm_GN_RS == "GN") %>%
  select(study_id, Author_Year, PointQual_Represent012,
         PointQual_Selection012,	PointQual_Exposure012,
         PointQual_OutcDef012,	PointQual_FUPduration012,
         PointQual_FUPlost012, BiasRiskTotal) # D-M -K & L

traffic_data_RS <- traffic_data %>%
  filter(bm_GN_RS == "RS") %>%
  select(study_id, Author_Year, PointQual_Represent012,
         PointQual_Selection012,	PointQual_Exposure012,
         PointQual_OutcDef012,	PointQual_FUPduration012,
         PointQual_FUPlost012, BiasRiskTotal) # D-M -K & L

traffic_light_GN <- traffic_data_GN %>%
  dplyr::mutate_all(~ case_when(
    . == "hig" ~ "red_sign",
    . == "low" ~ "green_sign",
    . == "med" ~ "yellow_sign",
    TRUE ~ as.character(.)
  ))

traffic_light_GN[traffic_light_GN$BiasRiskTotal == "High",]$BiasRiskTotal <- "red_sign"
traffic_light_GN[traffic_light_GN$BiasRiskTotal == "Low",]$BiasRiskTotal <- "green_sign"
traffic_light_GN[traffic_light_GN$BiasRiskTotal == "Med.",]$BiasRiskTotal <- "yellow_sign"

names(traffic_light_GN) <- c("Study_ID", "Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall")

GN_gt <-traffic_light_GN %>%
  select("Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall") %>%
  gt() %>%
  cols_align(
    align = "center"
  ) %>%
  tab_options(
    data_row.padding = px(2)
  ) %>%
  text_transform(
    locations = cells_body(c(D1, D2, D3,
                             D4, D5, D6,
                             Overall)),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("../images/", .x, ".png"),
        height = 25
      ))
    }) %>%
  tab_options(
    column_labels.background.color = "#585d63",
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "#585d63",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "#f9f9fb",
    table_body.hlines.color = "#f9f9fb",
    table_body.hlines.width = px(0.5),
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "#585d63",
    table.border.bottom.width = px(5),
    row.striping.background_color = "#f9f9fb",
    data_row.padding = px(3)) %>%
  cols_width(starts_with("D") ~ px(70),
             starts_with("O") ~ px(80),
             Study ~ px(100)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#585d63"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Study)) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgray"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Overall)) %>%
  opt_interactive(use_search = TRUE, use_compact_mode = TRUE, use_page_size_select = TRUE)

traffic_light_RS <- traffic_data_RS %>%
  dplyr::mutate_all(~ case_when(
    . == "hig" ~ "red_sign",
    . == "low" ~ "green_sign",
    . == "med" ~ "yellow_sign",
    TRUE ~ as.character(.)
  ))

traffic_light_RS[traffic_light_RS$BiasRiskTotal == "High",]$BiasRiskTotal <- "red_sign"
traffic_light_RS[traffic_light_RS$BiasRiskTotal == "Low",]$BiasRiskTotal <- "green_sign"
traffic_light_RS[traffic_light_RS$BiasRiskTotal == "Med.",]$BiasRiskTotal <- "yellow_sign"

names(traffic_light_RS) <- c("Study_ID", "Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall")

RS_gt <- traffic_light_RS %>%
  select("Study", "D1", "D2", "D3", "D4", "D5", "D6", "Overall") %>%
  gt() %>%
  cols_align(
    align = "center"
  ) %>%
  tab_options(
    data_row.padding = px(2)
  ) %>%
  text_transform(
    locations = cells_body(c(D1, D2, D3,
                             D4, D5, D6,
                             Overall)),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("../images/", .x, ".png"),
        height = 25
      ))
    }) %>%
  tab_options(
    column_labels.background.color = "#585d63",
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "#585d63",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "#f9f9fb",
    table_body.hlines.color = "#f9f9fb",
    table_body.hlines.width = px(0.5),
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "#585d63",
    table.border.bottom.width = px(5),
    row.striping.background_color = "#f9f9fb",
    data_row.padding = px(3)) %>%
  cols_width(starts_with("D") ~ px(70),
             starts_with("O") ~ px(80),
             Study ~ px(100)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#585d63"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Study)) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgray"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Overall)) %>%
  opt_interactive(use_search = TRUE, use_compact_mode = TRUE, use_page_size_select = TRUE)

traffic_light_BM <- traffic_data_BM %>%
  dplyr::mutate_all(~ case_when(
    . == "hig" ~ "red_sign",
    . == "low" ~ "green_sign",
    . == "med" ~ "yellow_sign",
    TRUE ~ as.character(.)
  ))

traffic_light_BM[traffic_light_BM$BiasRiskTotal == "High",]$BiasRiskTotal <- "red_sign"
traffic_light_BM[traffic_light_BM$BiasRiskTotal == "Low",]$BiasRiskTotal <- "green_sign"
traffic_light_BM[traffic_light_BM$BiasRiskTotal == "Med.",]$BiasRiskTotal <- "yellow_sign"

names(traffic_light_BM) <- c("Study_ID", "Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "Overall")

BM_gt <- traffic_light_BM %>%
  select("Study", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "Overall") %>%
  arrange(Study) %>%
  gt() %>%
  cols_align(
    align = "center"
  ) %>%
  tab_options(
    data_row.padding = px(2)
  ) %>%
  text_transform(
    locations = cells_body(c(D1, D2, D3,
                             D4, D5, D6,
                             D7, D8,
                             Overall)),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("../images/", .x, ".png"),
        height = 25
      ))
    }) %>%
  tab_options(
    column_labels.background.color = "#585d63",
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "#585d63",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "#f9f9fb",
    table_body.hlines.color = "#f9f9fb",
    table_body.hlines.width = px(0.5),
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "#585d63",
    table.border.bottom.width = px(5),
    row.striping.background_color = "#f9f9fb",
    data_row.padding = px(3)) %>%
  cols_width(starts_with("D") ~ px(70),
             starts_with("O") ~ px(80),
             Study ~ px(100)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#585d63"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Study)) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgray"),
      cell_text(color = "white")
    ),
    locations = cells_body(columns = Overall)) %>%
  opt_interactive(use_search = TRUE, use_compact_mode = TRUE, use_page_size_select = TRUE)


# ui -----------------------------------------------------------------------


ui <- bootstrapPage('', tags$style(HTML("
      .box {
        background-color: #f8f9fa;
        border-radius: 0.3rem;
        padding: 1rem;
        margin: 0 1rem 1rem 0;
        box-shadow: 0 1px 1px rgba(0,0,0,.05);
      }
      .box-x {
        height: 300px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
    ")),
                    navbarPage("Ahmad et al. 2023",
                               theme = shinytheme("sandstone"),
                               tabPanel("Introduction",
                                        fluidRow(
                                          box(
                                            h1("Precision Prognostics for CVD in T2D: Interactive Results"),
                                            column(
                                              width = 12,
                                              align = "center",
                                              a(actionButton("go_to_preprint", "Click here to access our pre-print"),
                                                href="https://www.medrxiv.org/content/10.1101/2023.04.26.23289177v1")
                                              ),
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            class = "box-x")
                                          ),
                                        fluidRow(
                                          box(title = "Directions", status = "info", solidHeader = TRUE, width = 6,
                                              tags$p("This interactive web app displays the results from our recent systematic review and meta-analysis on precision prognostics for cardiovascular disease in type 2 diabetes. Explore the findings and visualize the data by hovering over our tabs on the top of the page and using the interactive plots provided.")
                                              ),
                                          box(title = "Web-app maintainer", status = "primary", solidHeader = TRUE, width = 6,
                                              tags$a(href="https://github.com/hugofitipaldi", "Hugo Fitipaldi"))
                                          )
                                        ),
                               tabPanel("Adjusted covariates",
                                        tabsetPanel(
                                          type = "tabs",
                                          tabPanel("Histogram",
                                                   pageWithSidebar(
                                                     headerPanel('Histograms'),
                                                     sidebarPanel("Histogram showing the count of studies per (A) number of adjusted covariates and (B) number of traditional CVD risk factors adjusted for in the analyses.",
                                                                  radioButtons("histogram_type", "Covariates type:",
                                                                               covariates.options)),
                                                     mainPanel(plotlyOutput('histogram.plot')))
                                                   ),
                                          tabPanel("Network",
                                                   pageWithSidebar(
                                                     headerPanel('Network'),
                                                     sidebarPanel("The network figure represents the connections of adjusted covariates in 416 studies. The nodes in the figure correspond to covariates. The more centrally located a node, the more important its role as a factor in the network."),
                                                     mainPanel(visNetworkOutput("mynetwork", height="600px"))
                                                     )
                                                   )
                                          )
                                        ),
                               tabPanel("Metadata",
                                        tabsetPanel(
                                          type = "tabs",
                                          tabPanel("Affiliations - Countries",
                                                   pageWithSidebar(
                                                     headerPanel('Affiliations'),
                                                     sidebarPanel(
                                                       tags$p("Top 20 countries of affiliation of authors for biomarker, genetics and risk score studies. The data used for this visualization was obtained using the R package ",
                                                              tags$a(href="https://github.com/hugofitipaldi/affiliation", "affiliation"),
                                                              ". The final proportions of ancestries were calculated for each unique study and then aggregated as described in detail ",
                                                              tags$a(href="https://pubmed.ncbi.nlm.nih.gov/36190496/.", "here."))
                                                       ),
                                                     mainPanel(
                                                       fluidRow(
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Barplot",
                                                                    plotOutput('affiliation.plot', height="600px")
                                                                    ),
                                                           tabPanel("Map",
                                                                    leafletOutput(outputId = "map.aff", height = "600px"))
                                                           )
                                                         )
                                                       )
                                                     )
                                                   ),
                                          tabPanel("Gender Co-authors",
                                                   pageWithSidebar(
                                                     headerPanel('Gender'),
                                                     sidebarPanel(
                                                       tags$p("Gender distribution of authors for biomarker (BM), genetics (GN) and risk score (RS) studies. Mor information on the gender inference strategy utilized in this analysis can be found ",
                                                              tags$a(href= "https://pubmed.ncbi.nlm.nih.gov/36190496/.", "here.")),
                                                       radioButtons("study_type", "Study type:",
                                                                    study.options)),
                                                     mainPanel(
                                                       fluidRow(plotlyOutput('gender.plot'))
                                                       )
                                                     )
                                                   ),
                                          tabPanel("Origin - participants",
                                                   pageWithSidebar(
                                                     headerPanel('Origin - participants'),
                                                     sidebarPanel("Top 20 countries of origin of the study populations in the included studies."),
                                                     mainPanel(
                                                       fluidRow(
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Barplot",
                                                                    plotOutput('origin.plot', height="600px")
                                                                    ),
                                                           tabPanel("Map",
                                                                    leafletOutput(outputId = "map.ori", height = "600px")
                                                                    )
                                                           )
                                                         )
                                                       )
                                                     )
                                                   ),
                                          tabPanel("Ancestry - participants",
                                                   pageWithSidebar(
                                                     headerPanel('Ancestry - participants'),
                                                     sidebarPanel(
                                                       tags$p('This plot shows the percentage of participants by ancestry for the included studies in this analysis. The y-axis displays the reported ethnicities, including European, East Asian, Other, South Asian, Hispanic or Latin American, Native American, African American or Afro-Caribbean, Asian (unspecified), and Aboriginal Australian, while the x-axis shows the percentage of participants in each category. The data used for this visualization was obtained from PubMed and PubMed Central through manual curation and by applying text mining functions developed using R software version 4.1.2. The final proportions of ancestries were calculated for each unique study and then aggregated as described in detail ',
                                                              tags$a(href="https://pubmed.ncbi.nlm.nih.gov/36190496/.", "here.")
                                                              )
                                                       ),
                                                     mainPanel(
                                                       fluidRow(plotlyOutput('ancestry.plot', height="600px"))
                                                       )
                                                     )
                                                   )
                                          )
                                        ),
                               tabPanel("Quality Scores",
                                        add_busy_spinner(spin = "fading-circle", color = "white"),
                                        pageWithSidebar(
                                          headerPanel('Traffic light'),
                                          sidebarPanel("Quality assessment (risk of bias traffic light plot) for biomarkers (BM), genetic markers (GN), and risk scores (RS).",
                                                       radioButtons("study_type2", "Study type:",
                                                                    study.options),
                                                       tags$p(tags$li("D1 - Representativeness Bias"),
                                                              tags$li("D2 - Selection Bias"),
                                                              tags$li("D3 - Exposure Bias"),
                                                              tags$li("D4 - Outcome Bias"),
                                                              tags$li("D5- Duration of follow-up Bias"),
                                                              tags$li("D6 - Lost to follow-up Bias"),
                                                              tags$li("D7: Confounding Bias (Total number of covariates)"),
                                                              tags$li("D8: Confounding Bias (Traditional risk factors)")
                                                              ),
                                                       tags$p("Risk of Bias:"),
                                                       tags$p(imageOutput("color_leg"))
                                                       ),
                                          mainPanel(gt_output(outputId = "gn.table"))
                                          )
                                        )
                               )
                    )




# server -----------------------------------------------------------------------

server <- function(input, output) {

  output$histogram.plot <- renderPlotly(

    cov_df %>%
      filter(group == input$histogram_type) %>%
      plot_ly(x = ~as.factor(count_cov), y = ~n, type = 'bar',
              marker = list(color = ~color_fill,
                            line = list(color = ~color_line,
                                        width = 1.5))) %>%
      layout(yaxis = list(title = 'Count of studies'),
             xaxis = list(title = as.character(unique(filter(cov_df, group == input$histogram_type)[6]))))
  )



  output$mynetwork <- renderVisNetwork({
    visIgraph(bigram_graph) %>%
      visNodes(size = 20) %>%
      visEdges(width = "n") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),
                 nodesIdSelection = T)
  })

  output$affiliation.plot <- renderPlot({

    affiliations %>%
      filter(rank <=20) %>%
      ggplot(aes(x = reorder(Country_Rank, sum_country), y = sum_country)) +
      geom_bar(stat="identity", fill="dimgray") +
      labs( x = "", y = "") +
      theme_linedraw() +
      geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
      scale_y_continuous(expand = c(0.02, 1.2)) +
      geom_text(aes(reorder(Country_Rank, sum_country), sum_country + 0.7, label = paste0(sprintf("%2.2f", sum_country), "%")),
                position = position_dodge(width = 1)) +
      coord_flip() +
      theme(axis.text.x= element_text(colour = "black",  size = 10),
            axis.text.y = element_text(colour = "black",  size = 12),
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 16))

  })

  output$map.aff <- renderLeaflet({
    leaflet() %>%
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = affiliations_map2,
                  color = 'black',
                  fillColor =  ~pal(sum_country),
                  popup = affiliations_map2$popup_info,
                  smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
                  highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      ) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)

  })

  output$gender.plot <- renderPlotly({
    gender_coauthors %>%
      filter(BM_GN_RS == input$study_type) %>%
      plot_ly(labels = ~gender,
              values = ~prop * 100,
              type = 'pie',
              marker = list(colors = pie_palette, line = list(color = '#FFFFFF', width = 1)),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', gender,
                                 '</br>', round(prop * 100, 2), "%")) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  })

  output$origin.plot <- renderPlot({

    origin_df %>%
      filter(rank <=20) %>%
      ggplot(aes(x = reorder(Country_Rank, Proportion), y = Proportion)) +
      geom_bar(stat="identity", fill="dimgray") +
      labs(x = "", y = "",
           caption = "") +
      theme_linedraw() +
      geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
      scale_y_continuous(expand = c(0.02, 1.2)) +
      geom_text(aes(reorder(Country_Rank, Proportion), Proportion + 0.7, label = paste0(sprintf("%2.2f", Proportion), "%")),
                position = position_dodge(width = 1)) +
      coord_flip() +
      theme(axis.text.x= element_text(colour = "black",  size = 10),
            axis.text.y = element_text(colour = "black",  size = 12),
            plot.subtitle = element_text(size = 14),
            plot.title = element_text(size = 18, face = "bold"),
            strip.text = element_text(size = 16))

  })

  output$map.ori <- renderLeaflet({
    leaflet() %>%
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = origins_map2,
                  color = 'black',
                  fillColor =  ~pal(Proportion),
                  popup = origins_map2$popup_info,
                  smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
                  highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      ) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)

  })

  ancestry_pallete <- c("#a87a41","#d64053","#8bb533","#55c1a1","#4fb958","#bd6173","#a259c7","#c281c1","#3d8658","#d04798")

  output$ancestry.plot <- renderPlotly({

    ancestry_df %>%
      plot_ly(x = ~reorder(Ancestry, desc(Proportion)), y = ~Proportion, color = ~Ancestry, type = "bar",colors = ancestry_pallete,
              hoverinfo = 'text',
              hovertext = ~paste('</br> Ancestry: ', Ancestry,
                                 '</br>', round(Proportion, 2), "%"))%>%
      layout(xaxis = list(title = ""),
             yaxis = list (title = "% of participants",
                           #range = c(0, 100),
                           #ticktext = list("0","10%", "20%", "30%", "40%", "50%", "60%", "70%","80%"),
                           #tickvals = list(0,10,20,30,40,50,60,70,80),
                           tickmode = "array"))

  })

  # Traffic light
  output$gn.table <- render_gt({
    if (input$study_type2 == "BM") {
      expr = BM_gt
    } else if (input$study_type2 == "RS") {
      expr = RS_gt
    } else if (input$study_type2 == "GN") {
      expr = GN_gt
    }
  })


  output$color_leg <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'color_leg_low', '.png')))
    list(
      src = filename,
      style = "margin-left: auto; margin-right: auto; padding-top: 5px; padding-bottom: 5px;",
      height = "10%"
    )
  }, deleteFile = FALSE)




}

shinyApp(ui, server)


