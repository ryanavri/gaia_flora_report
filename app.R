library(shiny)
library(bslib)
library(data.validator)
library(DT)
library(dplyr)
library(flextable)
library(ggplot2)
library(gridlayout)
library(kableExtra)
library(rmarkdown)
library(shinybusy)
library(shinydashboard)
library(tidyverse)
library(treemap)
library(vegan)
library(rredlist)
library(rcites)
library(assertr)
library(BIOMASS)
library(tidyquant)
library(ggdist)
library(ggpubr)
library(pastecs)
library(qqplotr)
library(moments)
library(patchwork)


#data source
load("source/datavalidation.RData")


ui <- page_navbar(
  title = "Flora and Carbon Rapid Analysis",
  selected = "Validation",
  collapsible = TRUE,
  theme = bslib::bs_theme(preset = "minty"),
  nav_panel(
    title = "Validation",
    grid_container(
      layout = c(
        "num_chicks area1"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "165px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "num_chicks",
        card_header("Input"),
        card_body(
          fileInput(inputId = "fileflora", label = "Choose Flora File",
                    multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          fileInput(inputId = "filelulc", label = "Choose Stratum File",
                    multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
          actionButton(inputId = "florastart", label = "Validate"),
          selectInput("alometric", "Select Region", 
                      choices = c("Sumatera-Kalimantan" = 0.167, "Other" = 0.151, "Papua" = 0.206)),
          checkboxInput(
            inputId = "myCheckboxInput",
            label = "IUCN & CITES",
            value = FALSE
          ),
          actionButton(inputId = "floracalculate", label = "Calculate"),
          downloadButton("downloadReport")
        )
      ),
      grid_card(
        area = "area1",
        card_body(
          grid_container(
            layout = c(
              "area0 area1",
              "area2 area3"
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "1fr",
              "1fr"
            ),
            gap_size = "10px",
            grid_card(
              area = "area0",
              full_screen = TRUE,
              card_header("Mandatory Column"),
              card_body(
                uiOutput(width = "100%", outputId = "sayur")
              )
            ),
            grid_card(
              area = "area1",
              full_screen = TRUE,
              card_header("Your Data"),
              card_body(
                uiOutput(width = "100%", outputId = "datasetHead")
              )
            ),
            grid_card(
              area = "area2",
              full_screen = TRUE,
              card_header("Validation Report"),
              card_body(uiOutput(outputId = "validation"))
            ),
            grid_card(
              area = "area3",
              full_screen = TRUE,
              card_header("List of unique Species"),
              card_body(uiOutput(outputId = "uspecies", width = "100%"))
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Exploratory",
    grid_container(
      layout = c(
        "area0 area1",
        "area2 area2"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1.07fr",
        "0.93fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header("Raincloud plot of DBH"),
        card_body(plotOutput(outputId = "raincloudbh", width = "100%"))
      ),
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("Raincloud plot of Total Height"),
        card_body(plotOutput(outputId = "raincloudtt", width = "100%"))
      ),
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("Linear model between girth and tree height"),
        card_body(plotOutput(outputId = "lmddbhtt", width = "100%"))
      )
    )
  ),
  nav_panel(
    title = "Summary",
    grid_container(
      layout = c(
        "area0 area1",
        "area2 area3"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1.07fr",
        "0.93fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header("Richness index"),
        card_body(DTOutput(outputId = "spindex"))
      ),
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("Importance value"),
        card_body(DTOutput(outputId = "IV"))
      ),
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("Weight Density and Biomass"),
        card_body(DTOutput(outputId = "biomasswd"))
      ),
      grid_card(
        area = "area3",
        full_screen = TRUE,
        card_header("Mean Weighted Carbon Stock"),
        card_body(DTOutput(outputId = "meanagc"))
      )
    )
  ),
  nav_panel(
    title = "Plots",
    grid_container(
      layout = c(
        "area0 area1",
        "area2 area3"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "area0",
        full_screen = TRUE,
        card_header("Species Richness"),
        card_body(plotOutput(outputId = "richnessplot"))
      ),
      grid_card(
        area = "area1",
        full_screen = TRUE,
        card_header("Cluster between transect"),
        card_body(plotOutput(outputId = "cluster"))
      ),
      grid_card(
        area = "area2",
        full_screen = TRUE,
        card_header("AGC distribution on each class"),
        card_body(plotOutput(outputId = "meanagclass"))
      ),
      grid_card(
        area = "area3",
        full_screen = TRUE,
        card_header("AGC distribution across plot"),
        card_body(plotOutput(outputId = "meanagcplot"))
      )
    )
  )
)

server <- function(input, output, session) {

  output$sayur <- renderUI({
    sayur %>%
      head() %>%
      regulartable() %>% 
      autofit() %>%
      htmltools_value()
  })
  
  datanya <- reactive({
    req(input$fileflora)
    read.csv(file = input$fileflora$datapath,
             na.strings = ".", 
             sep = ",",
             header = TRUE)               
  })
  
  dataset <- reactive({
    req(datanya())
    datanya() %>%
      mutate(Scientific.Name = trimws(Scientific.Name))
  })
  
  datalulcnya <- reactive({
    req(input$filelulc)
    read.csv(file = input$filelulc$datapath,
             na.strings = ".", 
             sep = ",",
             header = TRUE)               
  })
  
  datalulc <- reactive({
    req(datalulcnya())
    datalulcnya() %>%
      mutate(LULC = trimws(LULC))
  })
  

  
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", ".docx", sep = "")
    },
    
    content = function(file) {
      
      show_modal_spinner(
        spin = "orbit",
        color = "#112446",
        text = "Tenang, app-nya jalan kok. Seruput kopi dan ngudud dulu~")
      
      src <- normalizePath("biodive_basic_v2.0.Rmd")
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "biodive_basic_v2.0.Rmd", overwrite = TRUE)
      
      out <- render("biodive_basic_v2.0.Rmd", word_document(), params = list(data = input$fileflora$datapath))
      file.rename(out, file)
      
      remove_modal_spinner()
    })

  observeEvent(input$florastart, {
    
    # Data validation----
    output$validation <- renderUI({
      report <- data_validation_report() 
      data.validator::validate(dataset(), description = "avifauna dataset validation") %>%
        validate_cols(predicate = not_na, Landscape:Site, description = "No missing values") %>%
        validate_cols(predicate = not_na, Transect:Plot.ID, description = "No missing values") %>%
        validate_cols(predicate = not_na, Scientific.Name:Taxon.Rank, description = "No missing values") %>%
        validate_cols(predicate = not_na, Class:ID.Pohon, description = "No missing values") %>%
        validate_cols(in_set(c("Species", "Genus", "Family", "Ordo")), Taxon.Rank, description = "Correct Taxon Rank category") %>%
        add_results(report)
      
      render_semantic_report_ui(get_results(report = report))
    })
    
    
    # Render yourdata----
    output$datasetHead <- renderUI({
      req(dataset())  # Ensure the dataset is available
      dataset()%>%
        head() %>%
        regulartable() %>% 
        autofit() %>%
        htmltools_value()
    })
    
    
    # Render unique species to check typos----
    output$uspecies <- renderUI({
      req(dataset()) 
      dataset() %>%
        distinct(Scientific.Name) %>%  # Extract unique Scientific.Names
        arrange(Scientific.Name) %>%
        regulartable() %>% 
        autofit() %>%
        htmltools_value()
    })
    
    calculate_agb <- function(alometric) {
      clean$agb <- (alometric * clean$DBH^2.560 * clean$meanWD^0.889)
      return(clean)
    }
    
    
    #Eda for DBH
    output$raincloudbh <- renderPlot({
      req(dataset())
      dataset() %>%
        ggplot(aes(x = factor(Class, levels = c("C", "B", "A")), y = DBH, fill=factor(Class))) +
        
        ggdist::stat_halfeye(
          adjust = 0.5,
          justification = -.2,
          .width = 0,
          point_colour = NA
        ) + 
        
        geom_boxplot(
          width =.12,
          outlier.colour = NA,
          alpha = .5
        ) +  
        
        ggdist::stat_dots(
          side = "left",
          justification = 1.1,
          binwidth = .25
        ) + 
        
        scale_fill_tq() + 
        theme_tq() +
        labs(
          title = "Raincloud Plot of DBH",
          subtitle = "",
          x = "Class",
          y = "DBH (cm)",
          fill = "Class"
        ) + 
        coord_flip()
    })
    
    #Eda for total height
    output$raincloudtt <- renderPlot({
      req(dataset())
      dataset() %>%
        ggplot(aes(x = factor(Class, levels = c("C", "B", "A")), y = TT, fill=factor(Class))) +
        
        ggdist::stat_halfeye(
          adjust = 0.5,
          justification = -.2,
          .width = 0,
          point_colour = NA
        ) + 
        
        geom_boxplot(
          width =.12,
          outlier.colour = NA,
          alpha = .5
        ) +  
        
        ggdist::stat_dots(
          side = "left",
          justification = 1.1,
          binwidth = .25
        ) + 
        
        scale_fill_tq() + 
        theme_tq() +
        labs(
          title = "Raincloud Plot of Tree Height",
          subtitle = "",
          x = "Class",
          y = "Height (m)",
          fill = "Class"
        ) + 
        coord_flip()
    })
    
    # Eda for relationship between heigth and girth
    output$lmddbhtt <- renderPlot({
      req(dataset())

      raw <- dataset()
      # Calculate R-squared value
      fit <- lm(TT ~ DBH, raw)
      r_squared <- summary(fit)$r.squared
      
      raw %>%
        ggplot(aes(x = DBH, y = TT)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) + 
        scale_fill_tq() + 
        theme_tq() +
        geom_label(aes(x = max(DBH), y = max(TT), label = paste("R² =", round(r_squared, 2))), 
                     hjust = 1, vjust = 1)
    })
    
    
  })
  
  observeEvent(input$floracalculate, {
    
    clean_data <- reactive({
      
      raw <- dataset()
      
      raw <- raw %>%
        separate(Scientific.Name, into = c("Genus", "Species"), sep = " ", remove = FALSE)
      
      wd <- getWoodDensity(
        genus = raw$Genus,
        species = raw$Species
      )
      
      wd <- wd %>%
        mutate(Scientific.Name = paste(genus, species, sep = " ")) %>%
        select(Scientific.Name, meanWD) %>%
        distinct(Scientific.Name, .keep_all = TRUE)
      
      clean <- raw %>%
        inner_join(., wd, by = "Scientific.Name")
      
      # Function to calculate agb based on the selected option
      calculate_agb <- function(option) {
        clean$`Biomass (kg/tree)` <- (option * clean$DBH^2.560 * clean$meanWD^0.889)
        return(clean)
      }
      
      # Retrieve the numeric value based on the selected region from UI
      selected_option <- as.numeric(input$alometric)  # Use input$alometric directly as numeric
      
      # Calculate agb column based on the selected option from UI
      clean_with_agb <- calculate_agb(selected_option)
      
      # Add AGB(ton/ha) calculation to the existing dataframe
      cleaned <- clean_with_agb %>% 
        mutate(
          'AGB(ton/ha)' = case_when(
            Class == "A" ~ (4 * `Biomass (kg/tree)`) / 1000,
            Class == "B" ~ (25 * `Biomass (kg/tree)`) / 1000,
            Class == "C" ~ (100 * `Biomass (kg/tree)`) / 1000,
            TRUE ~ 0  # Return 0 for other cases, if necessary
          )
        ) %>% select(-Genus, -Species)
      return(cleaned)
    })
    
    
    # Render DataTable using the cleaned data with agb column
    output$biomasswd <- renderDT({
      req(clean_data())
      datatable(clean_data(), extensions = "Buttons", 
                options = list(
                  paging = TRUE,
                  scrollX = TRUE, 
                  searching = TRUE,
                  ordering = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf'),
                  pageLength = 10, 
                  lengthMenu = c(3, 5, 10)
                ))
    })
    

  #tabel ringkasan index----
  output$spindex <- renderDT({
    req(dataset())
    data <- dataset() %>%
      filter(Transect != "") %>% 
      filter(Taxon.Rank == "Species" | Taxon.Rank == "Genus") %>%
      count(Scientific.Name, Transect) %>%
      group_by(Transect) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), ~round(., 2)))
    datatable(data, extensions = "Buttons", 
              options = list(paging = TRUE,
                             scrollX=TRUE, 
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             pageLength=10, 
                             lengthMenu=c(3,5,10) ))
  })
  
  #estimate IV----
  output$IV <- renderDT({  
    req(dataset())
    sayur <- dataset()
    sayur$Class <- as.factor(sayur$Class)
    #Menghitung indeks nilai penting
    impset <- sayur %>% 
      group_by(Transect, Class, Scientific.Name) %>% 
      summarise(count = n(),
                basal = sum(0.7854*(DBH/100)^2)) %>% #konversi dbh (cm) ke basal area dalam meter persegi
      ungroup() %>% 
      as.data.frame
    
    imp <- importancevalue.comp(impset, site='Transect', species='Scientific.Name', count='count', 
                                basal='basal', factor='Class') 
    
    selected_column <- 'importance.value'  # Change this to the column name you want to select
    
    # Create an empty data frame to store the results
    result_df <- data.frame()
    
    # Iterate through each element in the list
    for (i in 2:length(imp)) {
      # Extract the species names and the selected column data
      species <- rownames(imp[[i]])
      values <- imp[[i]][, selected_column]
      
      # Create a temporary data frame for the current LOTP and sort by the selected column
      temp_df <- data.frame(
        Class = rep(names(imp)[i], length(species)),
        Species = species,
        Importancevalue = values
      )
      temp_df <- temp_df[order(-temp_df$Importancevalue), ]  # Sort by importancevalue
      
      # Take the top 5 species for the current LOTP
      top_5 <- temp_df[1:min(10, nrow(temp_df)), ]
      
      # Append the top 5 species data to the result dataframe
      result_df <- rbind(result_df, top_5)
      result_df$Importancevalue <- round(result_df$Importancevalue, 2)
      rownames(result_df) <- NULL
    }
    
    datatable(result_df, extensions = "Buttons", 
              options = list(paging = TRUE,
                             scrollX=TRUE, 
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             pageLength=10, 
                             lengthMenu=c(3,5,10) ))
    
  })
    
  # biomass and wd
  output$meanagc <- renderDT({  
    req(clean_data())
    req(datalulc())
    sayur <- clean_data()
    lulc <- datalulc()
    
    csayur <- inner_join(sayur, lulc, by="Plot.ID")
    
    unique_lulc <- unique(csayur$LULC)
    
    result_list <- lapply(unique_lulc, function(lulc_value) {
      csayur %>%
        filter(LULC == lulc_value) %>%
        group_by(Plot.ID) %>%
        summarise(total_AGB = sum(`AGB(ton/ha)`)) %>%
        stat.desc(basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95) %>%
        t() %>%
        as.data.frame() %>%
        slice(2) %>%
        mutate(
          'Mean.Cstock(tc/ha)' = mean,
          SE = SE.mean,
          'Precision (%)' = (CI.mean / mean) * 100,
          '95% CI' = CI.mean,
          'Low 95% CI' = mean - CI.mean,
          'Up 95% CI' = mean + CI.mean
        ) %>%
        select('Mean.Cstock(tc/ha)', SE, 'Precision (%)', '95% CI', 'Low 95% CI', 'Up 95% CI') %>%
        mutate(across(1:last_col(), ~round(., 2)))
    })
    
    result_combined <- do.call(rbind, result_list)
    rownames(result_combined) <- unique_lulc
    result_combined
     
    datatable(result_combined, extensions = "Buttons", 
              options = list(paging = TRUE,
                             scrollX=TRUE, 
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             pageLength=10, 
                             lengthMenu=c(3,5,10) ))
    
  })
  
  # make cluster----
  output$cluster <- renderPlot({
    req(dataset())
    rawsp <- dataset() %>% 
      filter(Transect != "") %>% 
      filter(Taxon.Rank == "Species" | Taxon.Rank == "Genus")
    data_matrix <-table(rawsp$Transect, rawsp$Scientific.Name)
    data_matrix %>%
      vegdist(method = "bray") %>%
      hclust(method = "average") -> hc_transect
    plot(hc_transect, xlab = "", ylab = "Dissimilarity", sub = "Transect", hang = -1)
  })
  
  
  # richeness index plot
  output$richnessplot <- renderPlot({
    req(dataset())
    dataset() %>%
      filter(Transect != "") %>% 
      filter(Taxon.Rank == "Species" | Taxon.Rank == "Genus") %>%
      count(Scientific.Name, Transect) %>%
      group_by(Transect) %>%
      summarize(Richness = n_distinct(Scientific.Name),
                Abundance = sum(n),
                Shannon = -sum(prop.table(n) * log(prop.table(n))),
                Margalef = (n_distinct(Scientific.Name) - 1) / log(sum(n)),
                Evenness = (-sum(prop.table(n) * log(prop.table(n))))/log(length(n)),
                Simpson = sum(prop.table(n)^2)) %>%
      mutate(across(4:last_col(), ~round(., 2))) %>%
      select(c(Transect,Shannon,Margalef)) %>% 
      pivot_longer(-Transect, names_to = "Richness Index", values_to = "values") %>%
      ggplot(aes(fill=`Richness Index`, y=values, x=Transect)) + 
      geom_col(position="dodge", width = 0.8) + 
      geom_text(aes(label = round(values,2)), 
                position = position_dodge(0.8), vjust = -0.5, hjust = 0.5) + 
      theme_bw()
  })

  #mean agc on each class
  output$meanagclass <- renderPlot({
  
    req(clean_data())
    req(datalulc())
    sayur <- clean_data()
    lulc <- datalulc()
    
    csayur <- inner_join(sayur, lulc, by="Plot.ID")
    
    dis.plot <- csayur %>% 
      group_by(LULC, Plot.ID) %>% 
      summarise(AGB=sum(`AGB(ton/ha)`))
    
    Bp.se <- dis.plot %>%
      group_by(LULC) %>%
      summarise( 
        n=n(),
        mean=mean(AGB),
        sd=sd(AGB)
      ) %>%
      mutate( se=sd/sqrt(n))  %>%
      mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
    
  ggplot(Bp.se) +
      geom_bar( aes(x=LULC, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
      geom_errorbar( aes(x=LULC, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) + 
      ylab("Above Ground Carbon (tonnes C/ha)") + xlab(NULL) +
      theme_bw()
    
  })
  
  #mean agc across plot
  output$meanagcplot <- renderPlot({
  
    req(clean_data())
    req(datalulc())
    sayur <- clean_data()
    lulc <- datalulc()
    
    csayur <- inner_join(sayur, lulc, by="Plot.ID")
    
    dis.plot <- csayur %>% 
      group_by(LULC, Plot.ID) %>% 
      summarise(AGB=sum(`AGB(ton/ha)`))
    
    ggplot(dis.plot, aes(fct_reorder(Plot.ID, AGB),AGB))+
      geom_col(aes(fill = AGB), show.legend = FALSE) +
      labs(x="Plot ID", y="Above Ground Carbon (tonnes C/ha)") +
      geom_hline(yintercept = 100, lty=2) +
      geom_hline(yintercept = 180, lty=4) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_gradient(low = "yellow", high = "green")
  })
  
  
})

}

shinyApp(ui, server)


