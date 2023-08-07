# --------- OUTPUT CONTINOUS RESPONSE ---------

output$select_cont_id = renderUI({
  
  
  req(data_clinical())
  
  ids = list()
  
  for (cname in names(data_clinical())) {
    
    colx = data_clinical()[[cname]]
    if (length(unique(colx[!is.na(colx)])) == nrow(data_clinical())) {
      ids = append(ids, cname)
    }
  }
  
  selectizeInput(inputId = "cont_id", 
                 label = "Select the identification column:", 
                 choices = ids, multiple = TRUE, options = list(maxItems = 1))
})

output$select_cont_response = renderUI({
  
  req(data_clinical())

  
  selectizeInput(inputId = "cont_response", label = "Select the response variable:", 
                 choices = names(data_clinical() %>% dplyr::select(where(is.numeric))), 
                 multiple = TRUE, options = list(maxItems = 1))
})

output$select_cont_predictor = renderUI({
  
  req(E_hat())
  
  
  selectizeInput(inputId = "cont_pred", label = "Invesigate one spesific signature-predictor:", 
                 choices =  names(as.data.frame(E_hat())),
                 multiple = TRUE, options = list(maxItems = 1))
})

subset_id_response_cont = reactive({

  req(data_clinical())
  req(E_hat())
  
  req(input$cont_id)
  req(input$cont_response)
  
  
  E = as.data.frame(E_hat())
  id = row.names(E)
  E = cbind(id, E)
  
  
  subset_vector = c(input$cont_id, input$cont_response)
  subset_clinical = subset(data_clinical(), select = subset_vector)
  
  # Remove all rows with NA values 
  subset_clinical = subset_clinical[complete.cases(subset_clinical), ]
  
  names(subset_clinical) = c('id', 'response')
  table = merge(subset_clinical, E, by = 'id') 
  if(nrow(table) == 0) {table = NULL}
  table 
}) 

merge_id_response_cluster_cont = reactive({
  
  
  req(data_clinical())
  req(E_hat())
  
  req(input$cont_id)
  req(input$cont_response)
  req(input$k_groups)
  
  
  E = as.data.frame(E_hat())
  id = row.names(E)
  E = cbind(id, E)
  
  subset_vector = c(input$cont_id, input$cont_response)
  subset_clinical = subset(data_clinical(), select = subset_vector)
  
  # Remove all rows with NA values 
  subset_clinical = subset_clinical[complete.cases(subset_clinical), ]
  names(subset_clinical) = c('id', 'response')
  
  grouping = data.frame(cluster = cutree(clustering()$tree_col, k = input$k_groups))
  
  id = row.names(grouping) 
  grouping = cbind(id, grouping)
  
  
  table = merge(subset_clinical, grouping, by = 'id')
  if (nrow(table) == 0) {table = NULL}
  table 
  
  
})


linear_model = reactive({
  
  req(subset_id_response_cont())
  req(input$cont_pred)
  
  formula_lm = as.formula(paste0('response ~ ', input$cont_pred))
  lm(formula_lm, data = subset_id_response_cont())
  
})

output$binary_E = DT::renderDataTable({
  
  req(E_hat())
  
  DT::datatable({
    E_hat()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 searching = FALSE, pageLength=10, scrollX = TRUE,
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
                     filename = 'E_hat'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'E_hat'
                     
                   )))
  
  )
})

output$binary_clinical = DT::renderDataTable({
  
  req(data_clinical())
  
  DT::datatable({
    data_clinical()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 searching = FALSE, pageLength=10, scrollX = TRUE,
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
                     filename = 'clincial'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'clinical'
                     
                   )))
  
  )
})


reactive_ggbox = reactive({

  req(merge_id_response_cluster_cont())

  ggplot(merge_id_response_cluster_cont(), aes(group = as.factor(cluster), x= as.factor(cluster), y = response, fill = as.factor(cluster))) + 
    geom_boxplot() + 
    labs(x = 'Cluster', y = 'Response', fill = "Clusters") + 
    ggtitle(paste('Boxplot of Response by Cluster', input$cont_response)) +
    theme(axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 20))
  
})


output$ggbox = renderPlot({
  reactive_ggbox()
})

output$download_box = downloadHandler(
  filename = function() {
    "nmf_continuous_boxplot.png"
  },
  content = function(file) {
    ggsave(file, plot = reactive_ggbox(), device = "png", height = 6, width = 10)
  }
)


# Merged datasets with the chosen columns 

output$out_merge_glm = DT::renderDataTable({
  
  req(subset_id_response_cont())
  
  DT::datatable({
    subset_id_response_cont()
  },
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 pageLength=10, searching = FALSE, scrollX = TRUE,
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
                     filename = 'data_table_continuous'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'data_table_continuous'
                     
                   )))
  )
})

output$out_merge_id_response_cluster = DT::renderDataTable({
  
  req(merge_id_response_cluster_cont())
  
  DT::datatable({
    merge_id_response_cluster_cont()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),
                 pageLength=10, searching = FALSE, scrollX = TRUE,
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
                     filename = 'data_table_continuous'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'data_tabe_continuous'
                     
                   )))
  )
})

output$summary_linear_model = renderPrint({

  req(linear_model())
  
  linear_model() %>%
    summary()
})


predict_stuff_cont = reactive({
  
  req(E_hat())
  req(subset_id_response_cont())
  
  E = as.data.frame(E_hat())
  
  result_matrix = matrix(NA, nrow = length(names(E)), ncol = 2,
                         dimnames = list(names(E), c('p-value', 'std error')))
  
  
  for (i in 1:length(names(E))) {
    
    signature = names(E)[i]
    
    lm_formula = as.formula(paste0('response ~ ' , signature))
    mod = lm(lm_formula, data = subset_id_response_cont())
    
    
    smry = summary(mod)
    cof = smry['coefficients']
    p_val = signif(smry[['coefficients']][2,4], 4)
    std_er = signif(smry[['coefficients']][2,2], 4)
    
    result_matrix[i,1] = p_val
    result_matrix[i,2] = std_er
    
  }
  
  result_matrix
  
  
})


create_summaries = reactive({
  
  req(subset_id_response_cont())
  req(E_hat())
  
  E = as.data.frame(E_hat())
  summaries_string = list()
  
  for (i in 1:length(names(E))) {
    
    signature = names(E)[i]
    lm_formula = as.formula(paste0('response ~ ' , signature))
    mod = lm(lm_formula, data = subset_id_response_cont())
    
    
    smry = summary(mod)
    summaries_string = append(summaries_string, capture.output(smry))
    
  }
  
  as.character(summaries_string)
  
})

output$out_result_matrix_cont = renderTable ({
  
  req(predict_stuff_cont())
  
  predict_stuff_cont()
}
, rownames = TRUE, digits = 4)


output$result_summaries = renderPrint({
  
  req(create_summaries())
  
  create_summaries()
  
})

output$aov_test <- renderPrint({

  req(merge_id_response_cluster_cont())
  
  
  aov(response ~ cluster, var.equal = TRUE, data = merge_id_response_cluster_cont())
  
  
})


output$t_test <- renderPrint({

  req(merge_id_response_cluster_cont())
  
  t.test(response ~ cluster, var.equal = TRUE, data = merge_id_response_cluster_cont())
  
  
})

output$wilcox_test <- renderPrint({

  req(merge_id_response_cluster_cont())
  req(input$k_groups == 2)
  
  wilcox.test(response ~ cluster, conf.int = TRUE, data = merge_id_response_cluster_cont())
})

output$cor_test1 = renderPrint({
  
  req(merge_id_response_cluster_cont())
  req(input$k_groups == 2)
  
  cor.test(merge_id_response_cluster_cont()$response, merge_id_response_cluster_cont()$cluster,
           conf.int = TRUE, method = 'pearson')
})

output$cor_test2 = renderPrint({

  req(merge_id_response_cluster_cont())
  req(input$k_groups == 2)
  
  cor.test(merge_id_response_cluster_cont()$response, merge_id_response_cluster_cont()$cluster,
           conf.int = TRUE, method = 'spearman')
})


