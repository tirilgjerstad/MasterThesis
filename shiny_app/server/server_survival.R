
# -------------- OUTPUT SURVIVAL --------------


merge_surv_data = reactive({

  req(data_clinical())
  req(clustering())
  req(input$surv_id)
  req(input$surv_time)
  req(input$surv_event)
  req(input$surv_cluster)
  req(input$k_groups)

  subset_vector = c(input$surv_id, input$surv_time, input$surv_event)
  
  subset_clinical = subset(data_clinical(), select = subset_vector)
  
  # Remove all rows with NA values 
  subset_clinical = subset_clinical[complete.cases(subset_clinical), ]
  
  names(subset_clinical) = c('id', 'survival_time', 'event_status')
  
  
  # extract groups 
  
  if (input$surv_cluster == 'NMF_cluster') {
    grouping = data.frame(cluster = cutree(clustering()$tree_col, k = input$k_groups))
    
    # adding id as a column connected to the cluster label 
    id = row.names(grouping) 
    grouping = cbind(id, grouping) 
  } else {
    ss_vec = c('id', input$surv_cluster)
    grouping = subset(data_clinical(), select = ss_vec)
    names(grouping) = c('id', 'cluster')
  }
  
  
  table = merge(subset_clinical, grouping, by = 'id')
  if (nrow(table) == 0) {table = NULL}
  table
})


survfit_km = reactive({
  
  req(merge_surv_data())
  
  survfit2(Surv(survival_time, event_status) ~ cluster, data = merge_surv_data())
})

output$select_surv_id = renderUI({
  
  req(data_clinical())
  
  ids = list()
  
  for (cname in names(data_clinical())) {
    
    colx = data_clinical()[[cname]]
    if (length(unique(colx[!is.na(colx)])) == nrow(data_clinical())) {
      ids = append(ids, cname)
    }
  }
  
  selectizeInput(inputId = "surv_id", 
                 label = "Select the identification column:", 
                 choices = ids, multiple = TRUE, options = list(maxItems = 1))
})

output$select_surv_time = renderUI({
  
  req(data_clinical())
  
  selectizeInput(inputId = "surv_time", label = "Select the time variable:", 
                 choices = names(data_clinical() %>% dplyr::select(where(is.numeric))), 
                 multiple = TRUE, options = list(maxItems = 1))
})

output$select_surv_event = renderUI({
  
  req(data_clinical())
  
  binary_factors = list()
  
  for (cname in names(data_clinical())) {
    
    colx = data_clinical()[[cname]]
    
    if (all(colx %in% c(0, 1))) {
      binary_factors = append(binary_factors, cname)
    }
  }
  
  
  selectizeInput(inputId = "surv_event", label = "Select the event variable:", 
                 choices = binary_factors, multiple = TRUE, options = list(maxItems = 1))
})


output$select_cluster = renderUI({
  req(data_clinical())
  
  clinical_cluster = append('NMF_cluster', names(data_clinical()))
  selectizeInput(inputId = 'surv_cluster', label = "Select the cluster variable:", 
                 choices = clinical_cluster, multiple = TRUE, options = list(maxItems = 1))
})

# Merged datasets with the chosen columns 
output$dataMerge = DT::renderDataTable({
  req(merge_surv_data())
  
  DT::datatable({
    merge_surv_data()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 pageLength=10, scrollX = TRUE, searching = FALSE,
                 dom = 'Bfrtip',
                 buttons = list(
                   list(
                     extend = 'copy',
                     text = '<i class="fa-solid fa-copy fa-xl"></i>',
                     titleAttr = 'Copy'
                   ),
                   list(
                     extend = 'excel',
                     text = '<i class="fa-solid fa-file-excel fa-xl"></i>',
                     titleAttr = 'download Excel',
                     filename = 'data_table_survival'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'data_table_survival'
                     
                   )))
  )
}) #end merge data 

# Show full and subset of data set 
output$table1 <- DT::renderDataTable(server = FALSE,{
  
  req(data_clinical())
  
  datatable(data_clinical(), extensions = c('Select', 'Scroller', 'FixedColumns'), 
            selection = list(mode = 'multiple', target = "column"), 
            options = list(ordering = FALSE, searching = FALSE, pageLength = 10, 
                           deferRender = TRUE, scrollY = 300, scroller = TRUE,
                           scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
})
output$table2 <- DT::renderDataTable(server = FALSE,{
  
  req(data_clinical())
  req(input$table1_columns_selected)
  
  subset = data_clinical()[, input$table1_columns_selected, drop = F]
  subset = subset[complete.cases(subset), ]
  
  datatable(subset, extensions = c('Select', 'Scroller', 'FixedColumns'), selection = list(target = "column"), 
            options = list(select = TRUE, ordering = FALSE, searching = FALSE, pageLength = 10, 
                           deferRender = TRUE, scrollY = 300, scroller = TRUE,
                           scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
})


# Kaplan meier 
output$nmf_clustered_data = renderPlot({

  req(survfit_km())
  req(merge_surv_data())

  
  pp = survfit_km() %>% 
    ggsurvfit() +
    labs(
      x =  "\n Survival Time (Months) ",
      y = "Survival Probabilities \n",
      title = "Survival Times Of \n Cancer Patients \n"
    ) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
          legend.title = element_text(face="bold", size = 10))
  if (input$censor) {pp = pp + add_censor_mark()}
  if (input$pval)  {pp = pp + add_pvalue("annotation")}
  if (input$confidence) {pp = pp + add_confidence_interval()}
  if (input$risktable) {pp = pp + add_risktable(times = seq(0, max(merge_surv_data()$survival_time), 50), risktable_group='risktable_stats')}
  if (input$quantile) {pp = pp + add_quantile()}
  if (input$legend) {pp = pp + add_legend_title("Groups ")}
  if (input$risksym & input$risktable) {pp = pp + add_risktable_strata_symbol()}
  pp
  
  
  
}) # end nmf_clustered_data

reactive_survplot = reactive({
  req(survfit_km())
  req(merge_surv_data())
  
  
  pp = survfit_km() %>% 
    ggsurvfit() +
    labs(
      x =  "\n Survival Time (Months) ",
      y = "Survival Probabilities \n",
      title = "Survival Times Of \n Cancer Patients \n"
    ) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
          axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
          legend.title = element_text(face="bold", size = 10))
  if (input$censor) {pp = pp + add_censor_mark()}
  if (input$pval)  {pp = pp + add_pvalue("annotation")}
  if (input$confidence) {pp = pp + add_confidence_interval()}
  if (input$risktable) {pp = pp + add_risktable(times = seq(0, max(merge_surv_data()$survival_time), 50), risktable_group='risktable_stats')}
  if (input$quantile) {pp = pp + add_quantile()}
  if (input$legend) {pp = pp + add_legend_title("Groups ")}
  if (input$risksym & input$risktable) {pp = pp + add_risktable_strata_symbol()}
  pp
})

output$nmf_clustered_data = renderPlot({
  reactive_survplot()
})

output$download_surv <- downloadHandler(
  filename = function() {
    "surv_plot.png"
  },
  content = function(file) {
    ggsave(file, plot = reactive_survplot(), device = "png", height = 6, width = 10)
  }
)


# Test statistics
output$summary_y_km <- renderPrint({
  
  req(merge_surv_data())
  

  Y_km = Surv(time = merge_surv_data()$survival_time, event = merge_surv_data()$event_status)
  
  Y_km %>% 
    summary()
})

output$summary_survfit_km <- renderPrint({
  req(survfit_km())
  survfit_km() %>% 
    summary()
})

output$survdiff_km = renderPrint({

  req(merge_surv_data())
  survdiff(Surv(survival_time, event_status) ~ cluster, data = merge_surv_data())
})

