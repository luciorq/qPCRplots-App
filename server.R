#
# This is the server logic of a Shiny web application.
library(shiny)
#library(xlsx)
library(tidyverse)
#library(readxl)
#library(scales)
#library(stringr)
#library(ggpubr)

# Define server logic required by the app
server <- function(input, output, session) {
  #This function is responsible for loading in the first selected file
  filedata1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    df <- readxl::read_excel(paste(inFile$datapath, ext, sep="."), sheet="Results")
    for (i in 1:nrow(df)){
      if(is.na(df[i,1])){ 
        next 
      }
      if(df[i,1] == "Well"){
        skip_num <- i
      }
    }
    df1 <- readxl::read_excel(paste(inFile$datapath, ext, sep="."), sheet="Results", skip=skip_num)
    for(i in 1:length(names(df1))){
      if (names(df1)[i] == "Cт"){
        names(df1)[i] <- "CT"
      }
    }
    df1
  })
  #This function is responsible for loading in the second selected file
  filedata2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    ext <- tools::file_ext(inFile2$name)
    file.rename(inFile2$datapath,
                paste(inFile2$datapath, ext, sep="."))
    df <- readxl::read_excel(paste(inFile2$datapath, ext, sep="."), sheet="Results")
    for (i in 1:nrow(df)){
      if(is.na(df[i,1])){ 
        next 
      }
      if(df[i,1] == "Well"){
        skip_num <- i
      }
    }
    df2 <- readxl::read_excel(paste(inFile2$datapath, ext, sep="."), sheet="Results", skip=skip_num)
    for(i in 1:length(names(df2))){
      if (names(df2)[i] == "Cт"){
        names(df2)[i] <- "CT"
      }
    }
    df2
  })
  #This function is responsible for loading in the metadata file
  metadata_table <- reactive({
    inFile3 <- input$file_metadata
    if (is.null(inFile3))
      return(NULL)
    ext <- tools::file_ext(inFile3$name)
    file.rename(inFile3$datapath,
                paste(inFile3$datapath, ext, sep="."))
    df_metadata <- readxl::read_excel(paste(inFile3$datapath, ext, sep="."))
    df_metadata
  })
  # The following set of functions populate the gene list selectors
  GeneList <- reactive({
    df1 <- filedata1()
    df2 <- filedata2()
    df1 <- df1[,"Target Name"]
    df2 <- df2[,"Target Name"]
    excel_table = rbind(df1, df2)
    gene_list <- unique(excel_table$`Target Name`)
    gene_list
  })
  observe({
    updateSelectInput(session, "gene1", choices = GeneList(), selected = " ")
  })
  observe({
    updateSelectInput(session, "gene2", choices = GeneList(), selected = " ")
  })
  
  # This function gets the names for the target gene and the constitutive reference
  Gene1 <- reactive({ input$gene1 })
  Gene2 <- reactive({ input$gene2 })
  my_range1_upper <- reactive({ input$range1[2] })
  my_range1_lower <- reactive({ input$range1[1] })
  my_range2_upper <- reactive({ input$range2[2] })
  my_range2_lower <- reactive({ input$range2[1] })
  Threshold <- reactive({ input$threshold })
  
  # This function loads the excel files and update the values based on the range sliders
  CtTable <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    #Loading and concatenating both Files
    df1 <- filedata1()
    df2 <- filedata2()
    df1 <- df1[, c("Sample Name","Target Name", "CT", "Tm1")]
    df2 <- df2[, c("Sample Name","Target Name", "CT", "Tm1")]
    excel_table = rbind(df1, df2)
    
    if (is.null(excel_table))
      return(NULL)
    
    excel_table <- excel_table[complete.cases(excel_table),]
    
   # for( i in 1:length(excel_table$`Sample Name`)){
   #   excel_table$`Sample Name`[i] <- gsub("Sample ", "", excel_table$`Sample Name`[i])
   #   excel_table$`Sample Name`[i] <- gsub("sample ", "", excel_table$`Sample Name`[i])
   #   excel_table$`Sample Name`[i] <- gsub("sample", "", excel_table$`Sample Name`[i])
   #   excel_table$`Sample Name`[i] <- gsub("Sample", "", excel_table$`Sample Name`[i])
   #   if ( nchar(excel_table$`Sample Name`[i]) == 1 ){
   #     excel_table$`Sample Name`[i] <- paste0("0", excel_table$`Sample Name`[i])
   #   }
   # }
    table <- data_frame(name = excel_table$`Target Name`,
                        value = excel_table$CT , 
                        sample = excel_table$`Sample Name`,
                        Tm = excel_table$Tm1)
    table <- table %>% mutate(value = replace(value,value=="Undetermined",NA)) %>%
      mutate(value=as.numeric(value))
    table
  })
  ## Target Gene
  output$CT1 <- renderTable({
    table <- CtTable1()
    table
  })
  CtTable1 <- reactive({
    table <- CtTable()
    gene1_df <- table[table$name == Gene1(),]
    gene1_df$value[gene1_df$Tm > my_range1_upper()] <- NA
    gene1_df$value[gene1_df$Tm < my_range1_lower()] <- NA
    table <- gene1_df
    mean_Ct_vector <- tapply(as.numeric(table$value), table$sample, mean, na.rm=TRUE)
    tm_min_vector <- tapply(as.numeric(table$Tm), table$sample, min, na.rm = TRUE)
    tm_max_vector <- tapply(as.numeric(table$Tm), table$sample, max, na.rm = TRUE)
    tm_range_vector <- paste(round(tm_min_vector,digits = 1),"-",round(tm_max_vector, digits = 1))
    table = data_frame(Sample = Gene1(),
                       sample_ID = as.character(names(mean_Ct_vector)),
                       value = as.numeric(mean_Ct_vector),
                       Tm_range = tm_range_vector)
    table <-  table %>% arrange(Sample)
    table
  })
  ## Reference Gene
  output$CT2 <- renderTable({
    table <- CtTable2()
    table
  })
  CtTable2 <- reactive({  
    table <- CtTable()
    gene2_df <- table[table$name == Gene2(),]
    gene2_df$value[gene2_df$Tm > my_range2_upper()] <- NA
    gene2_df$value[gene2_df$Tm < my_range2_lower()] <- NA
    table <- gene2_df
    mean_Ct_vector <- tapply(as.numeric(table$value), table$sample, mean, na.rm=TRUE)
    tm_min_vector <- tapply(as.numeric(table$Tm), table$sample, min, na.rm = TRUE)
    tm_max_vector <- tapply(as.numeric(table$Tm), table$sample, max, na.rm = TRUE)
    tm_range_vector <- paste(round(tm_min_vector,digits = 1),"-",round(tm_max_vector, digits = 1))
    table = data_frame(Sample = Gene2(),
                       sample_ID = as.character(names(mean_Ct_vector)),
                       value = as.numeric(mean_Ct_vector),
                       Tm_range = tm_range_vector)
    table <-  table %>% arrange(Sample)
    table
  })
  
  # This function calculates the deltaCt values of the genes
  deltaCtTable <- reactive({
    Ct_table <- rbind(CtTable1(),CtTable2())
    if (is.null(Ct_table)){
      return(NULL)
    }
    target_df <- Ct_table[Ct_table$Sample == Gene1(),]
    reference_df <- Ct_table[Ct_table$Sample == Gene2(),]
    deltaCT_vector <- c()
    if(nrow(target_df) > 0 ){
      for (i in 1:nrow(target_df)){
        deltaCT <- 2^(-(target_df[i,"value"]-reference_df[i,"value"]))
        deltaCT_vector <- c(deltaCT_vector, deltaCT)
      }
    }
    deltaCT_df <- target_df[,c(1,2)]
    deltaCT_df <- deltaCT_df %>% mutate("deltaCT" = as.numeric(deltaCT_vector))
    deltaCT_df
  })
  # This function is used to render the Table with deltaCT values
  output$deltaCT <- renderTable({
    deltaCtTable()
  },digits = -2)
  
  ## This function is used to render the Plot
  output$plot_1 <- renderPlot({
    plotInput()
  },width =  400 )
  # This function proccess and plot the data
  plotInput <- reactive({
    threshold = Threshold()
    threshold <- as.numeric(threshold)
    colorblind_pallete <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    table <- deltaCtTable()
    if (is.null(table)){
      return(NULL)
    }
    metadata_df <- metadata_table()
    if (is.null(metadata_df)){
      return(NULL)
    } else {
      metadata_df <- metadata_df %>%
        dplyr::rename(sample_ID = Sample)
      table <- table %>%
        left_join(metadata_df, by = "sample_ID") 
    }
     
    
    names(table)[1:3] <- c("Sample", "Number", "value")
    ## Se os dados estiverem em log
    if(TRUE %in% (table$value < 0)){
      table$value <- sapply(table$value, function(x){10**x})
    }
    ## Removes missing values from the data
    table <- na.omit(table)
    table$Positive <- ifelse(table$value > threshold,"Positive", "Negative")
    
    if ("mock" %in% table$Sample){table$Sample <- relevel(table$Sample, "mock")}
    if ("Mock" %in% table$Sample){table$Sample <- relevel(table$Sample, "Mock")}
    
    max_normal = 100
    table_positive <- subset(table, value > threshold)
    table_negative <- subset(table, value <= threshold)
    
    #dilutions <- unique(table_positive[,1])
    
    #fig1 = ggplot(data=table,aes(x=Sample,y=value)) + 
    #  stat_boxplot(data=subset(table, value > threshold),aes(x=Sample,y=value), geom = "errorbar", width = 0.7, alpha = 0.4 ) +
    #  geom_boxplot(data =subset(table, value > threshold) , notch = TRUE, outlier.size = 0, notchwidth = 0.6, alpha = 0.3, colour = "black",aes(x=Sample ,y=value) ) +
    #  geom_beeswarm(size=1.3, cex=1.5, priority='ascending', aes(color = Positive )) +
    #  scale_y_log10(breaks = scales::trans_breaks("log10",function(x) 10^x),
    #                labels= scales::trans_format("log10", scales::math_format(10^.x))) +
    #  theme_bw(base_size=16) + theme(axis.text.x = element_text(angle=45,hjust=1), legend.title = element_blank()) +
    #  xlab("Sample") + ylab("RNA levels") +
    #  scale_colour_manual(values = ifelse( table$value > threshold, colorblind_pallete[1], colorblind_pallete[2] ) )
    
    fig_1 <- ggpubr::ggboxplot(data = table_positive, x = "Sample", y = "value",
                               color = "Group", add = "jitter",
                               #shape = "Group",
                               notch = input$plot_1_notch,
                               palette = "npg" , ## npg -> nature publishing group
                               yscale = "log10")
    
   # groups_vector <- unique(table$Group)
   # if (length(groups_vector) > 1) {
   #   my_comparisons <- list( c(groups_vector[1], groups_vector[2]))
   #   if (length(groups_vector) > 2){
   #     for (i in 3:length(groups_vector)) {
   #       my_comparisons[[i-1]] <- c(groups_vector[1],groups_vector[i])
   #     }
   #   }
   #   fig_1 + stat_compare_means(comparisons = my_comparisons) # Add pairwise comparisons p-value
   #     #stat_compare_means()    # label.y = 50               # Add global p-value
   # }
    fig_1
    
  })
  
  ## This function is used to save the plot as a PDF file
  output$download_plot_1 <- downloadHandler(
    filename = function() { paste(input$gene1, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "pdf")
    }
  )
  
  ## Function to download deltaCT excel table
  output$download_deltaCT_table <- downloadHandler(
    filename = function() { "delta_Ct_table.xlsx" },
    content = function(file) {
      tempFile <- tempfile(fileext = ".xlsx")
      xlsx::write.xlsx(deltaCtTable(), tempFile)
      file.rename(tempFile, file)
    })
}

server