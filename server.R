function(input, output, session){
  
  # update dataset based on selection from ui
  aggregate <- reactive({
    readRDS(paste("datasets/", input$dataset_multi, sep=""))
    readRDS(paste("datasets/", input$dataset_single, sep=""))
    readRDS(paste("datasets/", input$dataset_markset, sep=""))
    readRDS(paste("datasets/", input$dataset_multifea, sep=""))
    readRDS(paste("datasets/", input$dataset_cluster, sep=""))
    readRDS(paste("datasets/", input$dataset_sepfea, sep=""))
    readRDS(paste("datasets/", input$dataset_sepcat, sep=""))
    readRDS(paste("datasets/", input$dataset_marktab, sep=""))
    readRDS(paste("datasets/", input$dataset_download, sep=""))
  })
  
  # update values based on input from ui
  outVar_single = reactive({
    mydata = switch(input$subset_single, 
                   'Genes' = rownames(genes),
                   'Numeric Metadata' = meta_nums,
                   "PCs" = pcs
    )
    mydata
  })
  
  # update values based on input from ui
  outVar_double = reactive({
    mydata = switch(input$subset_multi, 
                    'Genes' = rownames(genes),
                    'Numeric Metadata' = meta_nums,
                    "PCs" = pcs
    )
    mydata
  })
  
  # update values based on input from ui
  outVar_seperated = reactive({
    mydata = switch(input$subset_seperated, 
                    'Genes' = rownames(genes),
                    'Numeric Metadata' = meta_nums,
                    "PCs" = pcs
    )
    mydata
  })

  getResChoices = reactive({
    mydata = levels(eval(call("$", aggregate(), input$identity_table)))
    mydata
  })
  
  # Reduction Type for the Single Marker Plot
  observe({
    updateSelectInput(session, "reduction_single", choices = reductions)
  })
  
  # Reduction Type for the Double Marker Plot
  observe({
    updateSelectInput(session, "reduction_multi", choices = reductions)
  })
  
  # Primary numeric value in the multiple marker plot
  observe({
    updateSelectizeInput(session, "multi_numeric", choices = outVar_double(), server = TRUE)
  })
  
  
  # Only numeric input for the single marker plot
  observe({
    updateSelectInput(session, "numeric_single", choices = outVar_single())
  })
  
  # Cluster Tree identity
  observe({
    updateSelectInput(session, "identity_tree", choices = meta_cats)
  })
  
  # Seperated Identity
  observe({
    updateSelectInput(session, "identity_seperated", choices = meta_cats)
  })
  
  # Seperated Numeric
  observe({
    updateSelectizeInput(session, "numeric_seperated", choices = outVar_seperated(), server = TRUE)
  })
  
  # Seperated Reduction
  observe({
    updateSelectInput(session, "reduction_seperated", choices = reductions)
  })

  # Seperated categroical Identity
  observe({
    updateSelectInput(session, "identity_seperated_cateogrical", choices = meta_cats)
  })
  
  # Seperated categorical identity2
  observe({
    updateSelectInput(session, "identity2_seperated_categorical", choices = meta_cats)
  })
  
  # Seperated categorical Reduction
  observe({
    updateSelectInput(session, "reduction_seperated_categorical", choices = reductions)
  })
  
  
  # Numeric input list for the marker set
  observe({
    updateSelectizeInput(session, "numeric_b", choices = rownames(genes), server = TRUE)
  })
  
  
  # Multiple Feature Plot
  observe({
    updateSelectizeInput(session, "multiple_feature_list", choices = rownames(genes), server = TRUE)
  })
  
  
  # Table Identity
  observe({
    updateSelectInput(session, "identity_table", choices = meta_cats)
  })
  
  
  # Table Marker
  observe({
    updateSelectInput(session, "compare_table", choices = getResChoices())
  })
  
  # Table Compare
  observe({
    updateSelectInput(session, "markers_table", choices = getResChoices())
  })
  

  # Documentation
  output$markdown <- renderUI({
    includeMarkdown("README.md")
  }) 
  
  # Marker Plot Single
  output$MarkerGenePlotSingle <- renderPlotly({
    temp_aggregate <- aggregate()
    p <- FeaturePlot(temp_aggregate, c(input$numeric_single), reduction=input$reduction_single) +
      theme_minimal()
    ggplotly(p)
  })
  
  multi_marker_gene_plot_list <- reactive({
    result <- list()
    for (i in seq_along(input$multi_numeric)) {
      temp_aggregate <- aggregate()
      result[[i]] = ggplotly(FeaturePlot(temp_aggregate, input$multi_numeric[[i]], blend=FALSE, reduction=input$reduction_multi) + 
                               theme_minimal())
    }
    result
  })
  
  # Marker Plot multiple
  output$MarkerGenePlotMulti <- renderUI({
    plot_output_list <- lapply(seq_along(multi_marker_gene_plot_list()), function(i) {
      div(style="display: inline-block", plotlyOutput(outputId = paste("multi_marker_gene_plot", i, sep = "_"))) # make placeholder name plot_i for each plot
    })
    do.call(tagList, plot_output_list) # combines plotlyOutputs
    
  })
  
  # Render each multi gene plot's graph
  observe({
    lapply(seq_along(multi_marker_gene_plot_list()), function(i) {
      output[[paste("multi_marker_gene_plot", i, sep = "_")]] <- renderPlotly({
        multi_marker_gene_plot_list()[[i]]
      })
    })
  })
  
  # Single Feature Categorical Feature Plot
  output$CategoricalPlotSingle <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$categorical_single
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(object = temp_aggregate, group.by=input$categorical_single, pt.size=0.5, reduction = input$reduction_single, label = T, repel = TRUE) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Multi Feature Categorical Feature Plot
  output$CategoricalPlotMulti <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$categorical_multi
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(object = temp_aggregate, pt.size=0.5, reduction = input$reduction_multi, label = T, repel = TRUE) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Single Feature Violin Plot
  output$ViolinPlotSingle <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$categorical_single
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- VlnPlot(object = temp_aggregate, features = c(input$numeric_single), pt.size = 0.05) + 
      theme_minimal()
    ggplotly(p)
  })
  
  # Multiple marker violin plot
  multi_marker_vln_plot_list <- reactive({
    result <- list()
    for (i in seq_along(input$multi_numeric)) {
      temp_aggregate <- aggregate()
      Idents(temp_aggregate) <- input$categorical_multi
      order <- sort(levels(temp_aggregate))
      levels(temp_aggregate) <- order
      result[[i]] = ggplotly(VlnPlot(object = temp_aggregate, features = input$multi_numeric[[i]], pt.size = 0.05) + 
                               theme_minimal())
    }
    result
  })
  
  output$ViolinPlotMulti <- renderUI({
    plot_output_list <- lapply(seq_along(multi_marker_vln_plot_list()), function(i) {
      div(style="display: inline-block", plotlyOutput(outputId = paste("multi_marker_vln_plot", i, sep = "_"))) # make placeholder name plot_i for each plot
    })
    do.call(tagList, plot_output_list) # combines plotlyOutputs
    
  })
  
  # Render each feature's graph
  observe({
    lapply(seq_along(multi_marker_vln_plot_list()), function(i) {
      output[[paste("multi_marker_vln_plot", i, sep = "_")]] <- renderPlotly({
        multi_marker_vln_plot_list()[[i]]
      })
    })
  })
  
  
  # Cluster Tree Plot
  output$ClusterTree <- renderPlot({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_tree
    temp_aggregate <- BuildClusterTree(
      temp_aggregate, dims = use.pcs)
    PlotClusterTree(temp_aggregate) 
  })
  
  # create list of plotly plots for each feature in multifeaturelist
  multi_feature_plot_list <- reactive({
    result <- list()
    for (i in seq_along(input$multiple_feature_list)) {
      temp_aggregate <- aggregate()
      result[[i]] = ggplotly(FeaturePlot(temp_aggregate, input$multiple_feature_list[[i]], blend=FALSE, reduction=input$multiple_feature_reduction) + 
                                 theme_minimal())
    }
    result
  })
  
  # Multiple Feature Plot
  output$MultipleFeaturePlot <- renderUI({
    plot_output_list <- lapply(seq_along(multi_feature_plot_list()), function(i) {
      div(style="display: inline-block", plotlyOutput(outputId = paste("multi_feature_plot", i, sep = "_"))) # make placeholder name plot_i for each plot
    })
    do.call(tagList, plot_output_list) # combines plotlyOutputs
  })
  
  # Render each feature's graph
  observe({
    lapply(seq_along(multi_feature_plot_list()), function(i) {
      output[[paste("multi_feature_plot", i, sep = "_")]] <- renderPlotly({
        multi_feature_plot_list()[[i]]
      })
    })
  })
  
  # Multiple Feature Categorical Plot
  output$MultipleFeatureCategoricalPlot <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$multiple_feature_categorical_plot
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(object = temp_aggregate, group.by=input$multiple_feature_categorical_plot, pt.size=0.5, reduction = input$multiple_feature_reduction, label = T, repel = TRUE) + 
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Seperated Identity Categorical Plot
  output$SeperatedIdentityCategorical <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_seperated_categorical
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(temp_aggregate, reduction=input$reduction_seperated_categorical, split.by = mysplitbydefault, ncol=4) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Seperated Identity 2 Categorical Plot
  output$SeperatedIdentity2Categorical <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity2_seperated_categorical
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(temp_aggregate, reduction=input$reduction_seperated_categorical, split.by = mysplitbydefault, ncol=4) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Seperated Categorical table
  output$SeperatedCountsCategorical <- renderPlotly({
    temp_aggregate <- aggregate()
    length_data = as.data.frame(prop.table(table(eval(call('$', temp_aggregate[[]], input$identity_seperated_categorical)), 
                                                 eval(call('$', temp_aggregate[[]], input$identity2_seperated_categorical))),1))
    colnames(length_data) = c(input$identity_seperated_categorical, input$identity2_seperated_categorical, 'Freq')
    mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")
    p <- ggplot(length_data, aes_string(x=input$identity_seperated_categorical, y=input$identity2_seperated_categorical, fill='Freq')) + 
      geom_tile() + 
      scale_fill_gradientn(colours = mycol) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Seperated Feature Plot
  output$SeperatedFeature <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_seperated
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- FeaturePlot(temp_aggregate, c(input$numeric_seperated), reduction = input$reduction_seperated, split.by = input$identity_seperated2, ncol=4) + 
      theme_minimal()
    ggplotly(p)
  })
  
  # Separated Dim Plot
  output$SeperatedDim <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_seperated
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- DimPlot(temp_aggregate, reduction=input$reduction_seperated, split.by = input$identity_seperated2, ncol=4) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Separated Violin Plot
  output$SeperatedViolin <- renderPlotly({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_seperated
    order <- sort(levels(temp_aggregate))
    levels(temp_aggregate) <- order
    p <- VlnPlot(temp_aggregate, c(input$numeric_seperated), group.by = input$identity_seperated, split.by = input$identity_seperated2, ncol=4) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  # Separated Counts table
  output$SeperatedCounts <- renderTable({
    temp_aggregate <- aggregate()
    marker = c(input$numeric_seperated)
    Idents(temp_aggregate) <- input$identity_seperated
    
    if(input$subset_seperated == 'Numeric Metadata'){
      nm <- data.frame(matrix(unlist(eval(call('$', temp_aggregate, marker[1]))), nrow=length(eval(call('$', temp_aggregate, marker[1]))), byrow=T))
      colnames(nm) = marker
      rownames(nm) = labels(eval(call('$', temp_aggregate, marker[1])))
      widedat <- nm
    }
    else{widedat <- FetchData(temp_aggregate, marker)}
    
    widedat$Cluster <- Idents(temp_aggregate)
    widedat[[mysplitbydefault]] = eval(call("$", temp_aggregate, input$identity_seperated2))
    widedat$final = paste(widedat[[mysplitbydefault]], widedat$Cluster, sep="_")
    final_object = (temp_aggregate(widedat[, 1:2], list(widedat$final), mean)[1:2])
    lab_list = widedat[[mysplitbydefault]]
    identities = widedat$Cluster
    
    num_list = widedat[[marker]]
    
    # df needs to be fixed
    tmp_df = data.frame(identities, num_list, lab_list)
    df = as.data.frame(pivot_wider(temp_aggregate(tmp_df[2], list(tmp_df$identities, tmp_df$lab_list), mean), names_from = Group.2, values_from = num_list))
    df[is.na(df)] <- 0
    rownames(df) = df$Group.1
    drops <- c("Group.1")
    df = df[ , !(names(df) %in% drops)]
    
    df_p = as.data.frame.matrix(prop.table((table(eval(call("$", temp_aggregate, input$identity_seperated)), eval(call("$", temp_aggregate, input$identity_seperated2)))),2))
    df_p=df_p/colSums(df_p)

    merged_final = as.data.frame.matrix(merge(df, df_p, by.x = 'row.names', by.y = 'row.names', suffixes = c(".AvgExpression",".Proportion")))
    merged_final
  }, width = "100%", colnames=TRUE, rownames=TRUE, digits=4)
  
  
  # Marker Table
  output$markers <- renderTable({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$identity_table
    if (as.logical(length(c(input$compare_table)))){FindMarkers(temp_aggregate, ident.1=input$markers_table, ident.2=input$compare_table)}
    else {FindMarkers(temp_aggregate, ident.1=input$markers_table)}
  }, rownames = TRUE, colnames = TRUE, width = "100%", digits=-5)
  
  # Marker Set Plot
  output$MarkerSet <- renderPlot({
    temp_aggregate <- aggregate()
    Idents(temp_aggregate) <- input$categorical_b
    markers = input$numeric_b
    expr.cutoff = 1
    widedat <- FetchData(temp_aggregate, markers)
    widedat$Cluster <- Idents(temp_aggregate)
    longdat <- gather(widedat, key = "Gene", value = "Expression", -Cluster)
    longdat$Is.Expressed <- ifelse(longdat$Expression > expr.cutoff, 1, 0)
    longdat$Cluster <- factor(longdat$Cluster)
    longdat$Gene <- factor(longdat$Gene, levels = markers)
    
    # Need to summarize into average expression, pct expressed (which is also an average)
    plotdat <- group_by(longdat, Gene, Cluster) %>% summarize(`Percentage of Expressed Cells` = mean(Is.Expressed), `Mean Expression` = mean(Expression))
    ggplot(plotdat, aes(x = Gene, y = Cluster)) +
      geom_point(aes(size = `Percentage of Expressed Cells`, col = `Mean Expression`)) +
      labs(size = "Percentage\nof Expressed\nCells", col = "Mean\nExpression", x = NULL) +
      scale_color_gradient(low = "grey", high = "slateblue4") + theme_grey(base_size = 15) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    # }, height = 1000, width = 900 )
  }, height = 1000)
  
  observeEvent(input$download_link, {
    link <- links_mapping[input$dataset_download]
    shinyjs::runjs(paste0('window.open("', link, '", "_blank");'))
  })
  
  
  # Potential to do, add DimPlot or HeatMap
}
