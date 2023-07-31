# -------- OUTPUT CATEGORICAL RESPONSE --------

output$select_cat_id = renderUI({
  
  req(data_clinical())
  
  ids = list()
  
  for (cname in names(data_clinical())) {
    
    colx = data_clinical()[[cname]]
    if (length(unique(colx[!is.na(colx)])) == nrow(data_clinical())) {
      ids = append(ids, cname)
    }
  }
  
  selectizeInput(inputId = "cat_id", 
                 label = "Select the identification column:", 
                 choices = ids, multiple = TRUE, options = list(maxItems = 1))
})

output$select_cat_response = renderUI({
  
  req(data_clinical())
  
  binary_factors = list()
  discrete_factors = list()
  
  for (cname in names(data_clinical())) {
    
    colx = data_clinical()[[cname]]
    
    if (length(unique(colx[!is.na(colx)])) > 2) {
      discrete_factors = append(discrete_factors, cname)
    }
    else if (length(unique(colx[!is.na(colx)])) == 2) {
      binary_factors = append(binary_factors, cname)
    }}
  
  
  if (input$cond_response == 'binary') {factors = binary_factors}
  else {factors = discrete_factors}
  
  
  selectizeInput(inputId = "cat_response", label = "Select the response variable:", 
                 choices = factors, multiple = TRUE, options = list(maxItems = 1))
})

output$select_cat_predictor = renderUI({
  
  req(E_hat())
  
  
  selectizeInput(inputId = "cat_pred", label = "Invesigate one spesific signature-predictor:", 
                 choices =  names(as.data.frame(E_hat())),
                 multiple = TRUE, options = list(maxItems = 1))
})

subset_id_response_cat = reactive({
  
  req(E_hat())
  req(data_clinical())
  
  req(input$cat_id)
  req(input$cat_response)
  
  
  E = as.data.frame(E_hat())
  id = row.names(E)
  E = cbind(id, E)
  
  
  subset_vector = c(input$cat_id, input$cat_response)
  subset_clinical = subset(data_clinical(), select = subset_vector)
  
  # Remove all rows with NA values 
  subset_clinical = subset_clinical[complete.cases(subset_clinical), ]
  
  names(subset_clinical) = c('id', 'response')
  table = merge(subset_clinical, E, by = 'id') 
  
  if (nrow(table) == 0) {table = NULL}
  table 
}) 

merge_id_response_cluster_cat = reactive({
  
  req(E_hat())
  req(data_clinical())
  
  req(input$cat_id)
  req(input$cat_response)
  
  E = as.data.frame(E_hat())
  id = row.names(E)
  E = cbind(id, E)
  
  subset_vector = c(input$cat_id, input$cat_response)
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

predict_stuff_cat = reactive({
  
  req(subset_id_response_cat())
  req(E_hat())
  
  E = as.data.frame(E_hat())
  
  result_matrix = matrix(NA, nrow = length(names(E)), ncol = 2,
                         dimnames = list(names(E), c('p-value', 'std error')))
  
  for (i in 1:length(names(E))) {
    
    signature = names(E)[i]
    
    formula_glm = as.formula(paste0('factor(response) ~ ', signature))
    mod = glm(formula_glm, data = subset_id_response_cat(), family = binomial)
    
    smry = summary(mod)
    cof = smry['coefficients']
    p_val = signif(smry[['coefficients']][2,4], 4)
    std_er = signif(smry[['coefficients']][2,2], 4)
    
    result_matrix[i,1] = p_val
    result_matrix[i,2] = std_er
    
  }
  
  result_matrix
  
  
})


gen_linear_model = reactive({
  
  req(subset_id_response_cat())
  req(input$cat_pred)
    
  formula_glm = as.formula(paste0('factor(response) ~ ', input$cat_pred))
  glm(formula_glm, data = subset_id_response_cat(), family = binomial)
  
  
})

output$out_result_matrix_cat = renderTable ({
  req(predict_stuff_cat())
  predict_stuff_cat()
}
, rownames = TRUE, digits = 4)

output$summary_gen_linear_model = renderPrint({
  req(gen_linear_model())
  gen_linear_model() %>%
    summary()
})


output$cateorical_table = renderTable({
  
  req(merge_id_response_cluster_cat())
  
  t = table(merge_id_response_cluster_cat()[, c('response', 'cluster')])
  as.data.frame.matrix(t, dnn = c('uu', 'nn'))
  
}, rownames = TRUE)


output$out_merge_id_response_cluster_cat = DT::renderDataTable({
  
  req(merge_id_response_cluster_cat())
  
  
  DT::datatable({
    merge_id_response_cluster_cat()
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
                     filename = 'data_table_categorical'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'data_table_categorical'
                     
                   )))
  )
})

output$cat_table = renderTable({
  
  req(merge_id_response_cluster_cat())
  table(merge_id_response_cluster_cat()[, c('response', 'cluster')], deparse.level = 0)
  
})

output$xtabs = renderPrint({
  
  req(merge_id_response_cluster_cat())
  
  (xtabs( ~ response + cluster, data = merge_id_response_cluster_cat()))
  
})

output$test_gt = render_gt({
  
  req(merge_id_response_cluster_cat())
  
  t = table(merge_id_response_cluster_cat()[, c('response', 'cluster')])
  
  dt = gt(as.data.frame.matrix(t), rownames_to_stub = TRUE)
  dt = tab_stubhead(dt, label = "response")
  dt = tab_spanner(dt, label = "cluster", columns = everything())
  dt = tab_header(dt, title = 'Result table')
  
  dt
  
})

output$fisher_test = renderPrint({
  req(merge_id_response_cluster_cat())
  fisher.test(table(merge_id_response_cluster_cat()[, c('response', 'cluster')]))
})


output$create_summaries_cat = renderPrint({
  
  req(subset_id_response_cat())
  req(E_hat())
  
  E = as.data.frame(E_hat())
  summaries_string = list()
  
  for (i in 1:length(names(E))) {
    
    signature = names(E)[i]
    
    
    formula_glm = as.formula(paste0('factor(response) ~ ', signature))
    mod = glm(formula_glm, data = subset_id_response_cat(), family = binomial)
    
    smry = summary(mod)
    summaries_string = append(summaries_string, capture.output(smry))
    
  }
  
  as.character(summaries_string)
  
})