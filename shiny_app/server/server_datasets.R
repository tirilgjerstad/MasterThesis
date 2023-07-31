

# ------------- OUTPUT DATASETS ---------------


# input M 
output$data_output_M = DT::renderDataTable({
  
  req(data_M())
  
  DT::datatable({
    data_M()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
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
                     filename = 'mut_matrix'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'mut_matrix'
                     
                   )))
  )
}) # end data_output_M

output$structure_M <- renderPrint({
  req(data_M())
  as.data.frame(data_M()) %>% 
    str()
})

output$summary_M <- renderPrint({
  req(data_M())
  data_M() %>% 
    summary()
})


# input clinical 
output$data_output_clinical = DT::renderDataTable({
  
  req(data_clinical())
  
  DT::datatable({
    data_clinical()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
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
                     filename = 'clinical'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'clinical'
                     
                   )))
  )
}) # end data_output_clinical

output$structure_clinical <- renderPrint({
  req(data_clinical())
  data_clinical() %>% 
    str()
})

output$summary_clinical <- renderPrint({
  req(data_clinical())
  data_clinical() %>% 
    summary()
})

# random M 
output$output_random_M = DT::renderDataTable({
  DT::datatable({
    random_M()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),

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
                     filename = 'synthetc_M'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'synthetc_M'
                     
                   )))
  )
})

output$structure_random_M <- renderPrint({
  as.data.frame(random_M()) %>% 
    str()
})

output$summary_random_M <- renderPrint({
  random_M() %>% 
    summary()
})


# random clinical
output$output_random_clinical = DT::renderDataTable({
  DT::datatable({
    random_clinical()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
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
                     filename = 'synthetc_clinical'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'synthetc_clinical'
                     
                   ))
                 )
  )
})

output$structure_random_clinical <- renderPrint({
  random_clinical() %>% 
    str()
})

output$summary_random_clinical <- renderPrint({
  random_clinical() %>% 
    summary()
})



output$select_subset_cols = renderUI({
  
  req(data_clinical())
  
  if (input$choose_upload == 'upload') {
    data = input_clinical()
  }
  
  else if (input$choose_upload == 'random') {
    data = random_clinical()
  }
  
  selectizeInput(inputId = "subset_cols", 
                 label = "Select the colums to keep:", 
                 choices = names(data), 
                 multiple = TRUE,
                 selected = names(data_clinical()),
                 options = list(minItems = 1))
})



output$nr_of_rows = renderUI({
  
  req(data_clinical())
  
  if (input$choose_upload == 'upload') {
    data = input_clinical()
  }
  
  else if (input$choose_upload == 'random') {
    data = random_clinical()
  }
  
  sliderInput(inputId = "subset_rows", label = "Select the number of rows to keep:",
              #label = p("Select the number of rows to keep",
              #           bsButton("q1", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
              step = 1, 
              value = nrow(data_clinical()), min = 1, max = nrow(data))
})




output$nr_of_rows_M = renderUI({
  
  
  req(data_M())
  
  if (input$choose_upload == 'upload') {
    req(input_M())
    data = as.data.frame(input_M())
  }
  
  else if (input$choose_upload == 'random') {
    data = random_M()
  }
  
  sliderInput(inputId = "subset_rows_M", label = "Select the number of rows to keep:",
              step = 1, value = nrow(data_M()), min = 1, max = nrow(data))
  
})



output$out_ss_clinical = DT::renderDataTable({
  
  req(ss_clinical())
  
  DT::datatable({
    ss_clinical()
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
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
                     filename = 'subset_clinical'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'subset_clinical'
                     
                   )))
  )
})

output$test_subset_M = DT::renderDataTable({
  
  req(data_M())
  req(input$subset_rows_M)
  
  data = as.data.frame(data_M())
  
  DT::datatable({
    #data %>% sample_n(input$subset_rows_M)
    data %>% dplyr::slice_head(n = input$subset_rows_M)
  },
  extensions = 'Buttons',
  options = list(lengthMenu=list(c(5, 10, 15, 20, 100, -1),c('5', '10', '15','20', '100', 'all')),pageLength=10,
                 scrollX = TRUE, searching = FALSE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
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
                     filename = 'subset_M'
                     
                   ),
                   list(
                     extend = 'csv',
                     text = '<i class="fa-solid fa-file-csv fa-xl"></i>',
                     titleAttr = 'download CSV',
                     filename = 'subset_M'
                     
                   )))
  )
})


