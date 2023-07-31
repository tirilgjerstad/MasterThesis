

## Shiny UI component for the Dashboard

dashboardPage(
  dashboardHeader(title="Analysing mutational data", titleWidth = 250,
                  leftUi = tagList(
                    
                    dropdownBlock(
                      id = 'nmf_opions', 
                      title = 'NMF options',
                      icon = icon('sliders'),
                      badgeStatus = NULL,
                      selectInput('method', label = 'NMF method', selected = 'brunet',
                                  choices = c('brunet', 'lee', 'snmf/l', 'snmf/r', 'nsNMF')),
                      
                      sliderInput('rank', label = 'Number of signatures (rank)', min = 2, max = 10, value = 5),

                      numericInput('seed', label = 'Select random seed', min = NA, max = NA, value = 1234),
                      numericInput('beta', label = 'Parameter: \U1D6FD (SNMF(L/R)', min = 0, max = 1, value = 0.0001, step = 0.01),
                      numericInput('eta', label = 'Parameter: \U03B7 (SNMF(R/L)', min = 0, max = 1, value = 0.1, step = 0.1),
                      
                      bsTooltip("beta", 
                                "\U1D6FD: regularisation parameter for sparsity control in ‘SNMF/R’and ‘SNMF/L’.",
                                "right", options = list(container = "body")),
                      
                      
                      bsTooltip('eta', 
                                '\U03B7: parameter to suppress the L2-norm in ‘SNMF/R’ and ‘SNMF/L’', 
                                placement = "right", trigger = "hover", options = list(container = "body"))
            
                                              

                      ),
                    
                    dropdownBlock(
                      id = 'cluster_options', 
                      title = 'Clustering options',
                      icon = icon('circle-nodes'),
                      badgeStatus = NULL,
                      sliderInput('k_groups', label = 'Number of clusters', min = 2, max = 10, value = 2),
                      selectInput('dist_met', label = 'Distance metric', 
                                  choices = c("correlation",  "euclidean", "manhattan"),
                                  selected = 'correlation'),
                      
                      selectInput('clust_met', label = 'Clustering method', 
                                  choices = c('complete', 'average', 'single'), selected = 'complete'),
                      selectInput('scale', label = 'Center and scale values by', 
                                  choices = c('row', 'column', 'none'), selected = 'column')
                      

                    )
                    )
                  ),  #end dashboard header 
  
  
  dashboardSidebar(width = 250, 
    sidebarMenu(id = "sidebar",
                
                menuItem('Front Page', tabName='welcome', icon = icon('info-circle')),

                
                menuItem('Datasets', tabName = 'about_data', icon = icon('database'), startExpanded = TRUE, 
                         menuSubItem('Upload data', tabName = 'upload', icon = icon('upload'), selected = FALSE),
                         menuSubItem('Subset data', tabName = 'subset', icon = icon('table'))
                         ),
                menuItem('Matrix Factorization', tabName = 'NMF', icon = icon('calculator'),
                         menuSubItem('NMF Visualization', tabName = 'nmf1', icon = icon('gears')),
                         menuSubItem('NMF Clustering', tabName = 'nmf2', icon = icon('circle-nodes'))),
                
                
                menuItem("Clinical Analysis", tabName = 'analysis', icon = icon('magnifying-glass-chart'),
                         
                         menuSubItem('Categorical response', tabName = 'categorical', icon = icon('chart-simple')),
                         conditionalPanel("input.sidebar == 'categorical'", 
                                          selectInput('cond_response', label = "Select type of response", 
                                                      choices = c('binary', 'non-binary'), 
                                                      selected = 'binary')),
                         menuSubItem('Continuous response', tabName = 'continuous', icon = icon('chart-line')),
                         menuSubItem("Survival response", tabName = "survival", icon = icon('timeline'))




  ))),
  
  dashboardBody(
    
    style = "height: 90vh; overflow-y: auto;",
    useShinyjs(),

    
    
    tabItems(

      tabItem(tabName='welcome',
              h3("A simple application for visualizing and analyzing mutation signatures"),
              p("This application has been developed by Tiril Gjerstad as part of a master's project (spring 2023). 
                It is a prototype intended for further development as needed. 
                The source code is available on Git via the following link: git.no."),
              br(),
              
              p(strong('Go to', em("Upload data"), 'to get started: ')),
              actionButton('switchtab', 'Upload data'),
              
              br(),
              br(),

             box(
               title = 'Introduction',
               id = 'test',
               width = 12, 
               icon = icon('circle-info'),
               status = 'primary', 
               solidHeader = FALSE, 
               collapsible = TRUE,
               collapsed = FALSE,
               p('Non-negative Matrix Factorization (NMF) is acomputational technique used to extract mutation signatures from genetic data. 
              Mutation signatures are characteristic patterns of DNA alterations that arise due to specific mutational processes, such as DNA damage and repair mechanisms, 
              environmental exposures, or underlying biological processes.'),
              p('NMF has gained popularity in recent years for its ability to extract underlying patterns from high-dimensional datasets, 
                NMF can be applied to analyze mutational catalogs derived from cancer genomes, 
                identifying distinct mutational patterns associated with different mutational processes.'),
              p('Visualization plays a crucial role in this process because it helps researchers gain insights into the patterns and structure of the data. 
              By visualizing the results of NMF analysis, scientists can effectively interpret and validate the extracted mutation signatures. 
              Visualization tools can represent mutation signatures allowing researchers to identify the contribution 
              of each mutation type to a given signature. 
              Interactive visualizations can facilitate the exploration of complex datasets, enabling researchers to detect potential outliers or novel 
              patterns that may be of biological significance.'),
              
              p('This application allows you to explore mutation datasets, providing an overview of the data and enabling the user to perform simple analyses. 
                The application supports running NMF on a mutation matrix, with options to choose specific NMF methods and other inputs.
                
                Additionally, the application allows the user to conduct some basic clinical analyses, such as using NMF clustering to predict clinical response. 
                This capability can be valuable in understanding the relationship between mutational patterns and clinical outcomes, potentially providing insights 
                into treatment responses and personalized medicine approaches.')

             ),
             
             box(
               title = 'Dashboard Header',
               id = 'test',
               width = 12, 
               icon = icon('plus'),
               status = 'primary', 
               solidHeader = FALSE, 
               collapsible = TRUE,
               collapsed = TRUE,
               
               fluidRow(
                 column(width = 7, 
                        h4('NMF options'),
                        p('The NMF options are easily accessible from the top menu bar, allowing users to conveniently explore various inputs for NMF. 
                          Users can choose between different NMF methods, select the rank/number of signatures, and set or modify the random seed for reproducibility. 
                          Additionally, they have the flexibility to adjust the regularization parameters eta (\U03B7) and beta (\U1D6FD).')
                        ),
                 column(width = 5,
                        h4('Clustering options'),
                        p('The Clustering options are conveniently accessible from the top menu bar, allowing users to easily explore different clustering techniques and 
                          adjust the number of clusters.'))
    
               )),
  
               
               

               
               box(
                 title = 'Dashboard Sidebar',
                 id = 'test',
                 width = 12, 
                 icon = icon('plus'),
                 status = 'primary', 
                 solidHeader = FALSE, 
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 fluidRow(
                   column(width = 3, 
                          h4('Datasets'),
                          h5(strong('Upload data')),
                          p('To perform analyses, the first step is to upload the dataset/datasets to explore. 
                           If the user only want to explore the functionality of the application, it is possible to select a synthetic dataset.'),
                          
                          h5(strong('Subset data')),
                          p('The user can easily subset the datasets by selecting a subset of columns/rows for better clarity and organization.')), 
                          
                   column(width = 6,
                          h4('Matrix Factorization'),
                          h5(strong('NMF visualization')),
                          
                          p('The user can visually explore the mutation matrix (M) and the product matrces E and S (obtained through NMF) 
                            through heatmaps in an interactive manner. The user can apply clustering according to their preferences. 
                            All the results, including tables and heatmaps, can be easily downloaded. By interacting with the application 
                            and updating the input arguments, the plots automatically refresh and display the results based on the new settings.'),
                          

                          h5(strong('NMF Clustering')),
                          p('NMF clustering groups the samples based on the signature that has the strongest expression in their mutation patterns. 
                            An overview of this clustering is represented as a heatmap. Furthermore, users have the option to visualize the 
                            distribution of signatures through a histogram, and view the results as tables.')
                          ),
                   
                   column(width = 3, 
                          h4('Clinical analysis'),
                          p('Clinical analyses require the user to upload a clinical dataset containing IDs that match the IDs present in the 
                            mutation matrix. The user can perform some simple clinical analyses to investigate whether NMF clustering corresponds
                            to clinical variables. There are three different types of responses to investigate: categorical response, 
                            continuous response, and survival response. In all cases, the test statistics will be available.'

                         ))))),

              
              

                


                
       
 
      
      ## Tab item upload data 
      tabItem(tabName = "upload",
              
              br(),
              
              div(style = 'margin-left: 20px',
              
              prettyRadioButtons(inputId = "choose_upload" , 
                                 label ='Upload your own datasets, or explore the functionality by using synthetic datasets:' , 
                                 inline = TRUE,
                                 choiceNames = c('Upload data', 'Explore with synthetic data'), 
                                 choiceValues = c('upload', 'random'), selected = 'upload')),
              

              br(),
              

              
              
              box(
                title = 'Upload data',
                id = 'upload_box',
                width = 12, 
                icon = icon('upload'),
                status = 'primary', 
                solidHeader = FALSE, 
                collapsible = TRUE,
                collapsed = TRUE,
                

                
                fluidRow(
                  column(width = 6,
                         fileInput("mutation_data", label = "Upload the mutation matrix file:",
                                   accept = c('.txt', '.csv', '.xlsx')), 
                         

                         radioButtons('row_col_M', label = "Upload with mutation categories on row or column?", 
                                      choices = c('Column', 'Row'), selected = 'Column'),
                         
                         bsPopover("row_col_M", 'Help',
                                   'Mutation categories and ids must be included in the file as first row and first column. Choose column if the mutation categories are the first column in your file, choose row if the mutation signatures are the first row in your file. Make sure mutation categories ends up as columnames when working with the data.',
                                   "bottom", options = list(container = "body")),

                         
                         radioButtons('sep_M', label = 'Separator', 
                                      choices = list('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t', 'Space' = ''), 
                                      selected = '')
                  ),
                  column(width = 6, 
                         fileInput("clinical_data", label = "Upload the clinical file:",
                                   accept = c('.txt', '.csv', '.xlsx')), 


                         radioButtons('row_col_C', label = "Upload with features on row or column?", 
                                      choices = c('Column', 'Row'), selected = 'Column'),
                         
                         bsPopover("row_col_C", 'Help',
                                   'Name of the features must be included in the file. Either as firt row or first column. Choose column if the features are the first column in your file, choose row if the features are the first row in your file. Make sure the features ends up as columnnames when working with the data.',
                                   "bottom", options = list(container = "body")),
                         
                         radioButtons('sep_C', label = 'Separator', 
                                      choices = list('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t', 'Space' = ''), 
                                      selected = '')

                         
                         
                  )
                )
                
              ),
              box(
                title = 'Mutational data',
                id = 'mut_box',
                width = 12, 
                icon = icon('dna'),
                status = 'primary', 
                solidHeader = FALSE, 
                collapsible = TRUE,
                collapsed = TRUE,
                
                tabBox(id = "t01", width = 12,
                       
                       tabPanel("Data", icon = icon("table"),
                                DT::dataTableOutput("data_output_M")),
                       
                       tabPanel("Structure", icon=icon("uncharted"),
                                verbatimTextOutput("structure_M")),
                       
                       tabPanel("Summary Stats", 
                                verbatimTextOutput("summary_M"), icon=icon("chart-pie")))
              ),
              
              box(
                title = 'Clinical data',
                id = 'clinical_box',
                width = 12, 
                icon = icon('house-chimney-medical'),
                status = 'primary', 
                solidHeader = FALSE, 
                collapsible = TRUE,
                collapsed = TRUE,
                
                tabBox(id = "t02", width = 12,
                       
                       tabPanel("Data", icon = icon("table"),
                                DT::dataTableOutput("data_output_clinical")),
                       
                       tabPanel("Structure", icon=icon("uncharted"),
                                verbatimTextOutput("structure_clinical")),
                       
                       tabPanel("Summary Stats", 
                                verbatimTextOutput("summary_clinical"), icon=icon("chart-pie")))
              )

      ), # end tabItem "upload" 
      
      
      tabItem(tabName = 'subset',
              tabBox(id = 'tabbox_subset_clinical', width = 12, 
                tabPanel('Subset Clinical Dataset', value = 'sub_c',
                  prettyRadioButtons(inputId = "choose_subset" , 
                                     label ='Use the full dataset, or choose to just use the selected rows and columns: ' , 
                                     inline = TRUE,
                                     choiceNames = c('Full dataset', 'Subset dataset'), 
                                     choiceValues = c('full', 'subset'), selected = 'full'),
                  

                  
                  uiOutput("select_subset_cols"),
                  
                  uiOutput('nr_of_rows'),
                  DT::dataTableOutput("out_ss_clinical")
                  
                ),
                tabPanel('Subset mutation matrix M', value = 'sub_m',
                         prettyRadioButtons(inputId = "choose_subset_M" , 
                                            label ='Use the full dataset, or choose to just use the selected rows and columns: ' , 
                                            inline = TRUE,
                                            choiceNames = c('Full dataset', 'Subset dataset'), 
                                            choiceValues = c('full', 'subset'), selected = 'full'),
                         
                         uiOutput('nr_of_rows_M'),
                         DT::dataTableOutput("test_subset_M")
                )
              )
            ), # end tabitem subset 
      

      tabItem(tabName = 'nmf1', 
              

              
              tabBox(id = 't3.2', width = 12,
                  tabPanel("Original Mutation Matrix M",
                           
                           br(),
                           br(),
                           
                           dropdown(
                             inputId = 'M1', label = 'Plot options',
                             checkboxInput('norm_M', label = "Normalize rows (x/sum)", value = TRUE),
                             checkboxInput("scale_M1", label = 'Scale and center the rows (ids)', value = FALSE),
                             numericInput('fs_row_m1', label = 'Change fontsize ids', min = 1, max = 15, value = 10),
                             numericInput('fs_col_m1', label = 'Change fontsize mutation categories', 
                                          min = 1, max = 15, value = 8),

                             icon = icon('gear'), style = 'jelly', size = 'xs'
                             
                           ),
                           
                           withSpinner(plotlyOutput("raw_heatmap_M")),
                           br(),
                           br(),
                           
                           dropdown(
                             inputId = 'M2', label = 'Plot options',
                             icon = icon('gear'), style = 'jelly', color = 'primary', size = 'xs',
                             
                             checkboxInput("scale_M2", label = 'Scale and center the rows (ids)', value = FALSE),
                             
                             
                             radioButtons('cluster_M', label = 'Cluster M by: ', 
                                          choiceNames = c('rows (ids)', 'columns (mutation categories)', 'both'),
                                          choiceValues = c('row', 'column', 'both')),
                             
                             checkboxInput('dendro_m', label = 'Show dendrogram', value = TRUE),
                             
                             checkboxInput('tick_row_m', label = 'Show x-labels (mutation categories)', value = FALSE),
                             checkboxInput('tick_col_m', label = 'Show y-labels (ids)', value = FALSE),
                             
                             
                             numericInput('fs_row_m2', label = 'Change fontsize row', min = 1, max = 15, value = 10),
                             numericInput('fs_col_m2', label = 'Change fontsize column', min = 1, max = 15, value = 8)
                             
                           ),
                           bsTooltip('M2', 'Plotting options', placement = 'right', trigger = 'hover'),
                           

                           withSpinner(plotlyOutput("cluster_heatmap_M")),

                           br(),
                           br(),
                           DT::dataTableOutput("dataM")
                  ),
                           

                  tabPanel("Exposure Matrix E",

                           br(),
                           br(),

                           dropdown(
                             inputId = 'E1', label = 'Plot options',
                             icon = icon('gear'), style = 'jelly', color = 'primary', size = 'xs',
                             
                             checkboxInput('norm_E', label = "Normalize columns (x/sum)", value = TRUE),
                             
                             numericInput('fs_row_e1', label = 'Change fontsize row', min = 1, max = 15, value = 10),
                             numericInput('fs_col_e1', label = 'Change fontsize column', min = 1, max = 15, value = 8)
                             
                             
                           ),
                           
                          withSpinner(plotlyOutput("raw_heatmap_E")),
                          br(),
                          br(),
                          
                          dropdown(
                            inputId = 'E2', label = 'Plot options',
                            icon = icon('gear'), style = 'jelly', color = 'primary', size = 'xs',
                            
                            radioButtons('cluster_E', label = 'Cluster E by: ', 
                                         choiceNames = c('rows (mutation signatues)', 'columns (ids)', 'both'),
                                         choiceValues = c('row', 'column', 'both'), selected = 'column'),
                            
                            checkboxInput('dendro_e', label = 'Show dendrogram', value = TRUE),
                            
                            checkboxInput('tick_row_e', label = 'Show x-labels (ids)', value = FALSE),
                            checkboxInput('tick_col_e', label = 'Show y-labels (signatures)', value = TRUE),
                            
                            
                            numericInput('fs_row_e2', label = 'Change fontsize row', min = 1, max = 15, value = 10),
                            numericInput('fs_col_e2', label = 'Change fontsize column', min = 1, max = 15, value = 8)
                            
                          ),
                          
                          withSpinner(plotlyOutput('cluster_heatmap_E')),
                          br(),
                          br(),
                          DT::dataTableOutput("dataE")


                  ), 
                  
                  tabPanel("Signature Matrix S",

                           br(),
                           br(),
                           
                           dropdown(
                             inputId = 'S1', label = 'Plot options',
                             icon = icon('gear'), style = 'jelly', color = 'primary', size = 'xs',
                             
                             checkboxInput('norm_S', label = "Normalize columns (x/sum)", value = TRUE),
                             
                             numericInput('fs_row_s1', label = 'Change fontsize row', min = 1, max = 15, value = 10),
                             numericInput('fs_col_s1', label = 'Change fontsize column', min = 1, max = 15, value = 8)


                           ),
                           
                           withSpinner(plotlyOutput("NMFplot_S")),
                           br(),
                           br(),
                           
                           dropdown(
                             inputId = 'S2', label = 'Plot options',
                             icon = icon('gear'), style = 'jelly', color = 'primary', size = 'xs',
                             
                             radioButtons('cluster_S', label = 'Cluster S by: ', 
                                          choiceNames = c('rows (mutation signatues)', 'columns (mutation categories)', 'both'),
                                          choiceValues = c('row', 'column', 'both'), selected = 'both'),
                             
                             checkboxInput('dendro_s', label = 'Show dendrogram', value = TRUE),
                             
                             checkboxInput('tick_row_s', label = 'Show x-labels (mutation categories) ', value = FALSE),
                             checkboxInput('tick_col_s', label = 'Show y-labels (mutation signatures)', value = FALSE),

                             numericInput('fs_row_s2', label = 'Change fontsize row', min = 1, max = 15, value = 10),
                             numericInput('fs_col_s2', label = 'Change fontsize column', min = 1, max = 15, value = 8)
                             
                           ),
                           

                           withSpinner(plotlyOutput("NMFplot_S2")),
                           br(),
                           br(),
                           DT::dataTableOutput("dataS")
                  )
                  
                  


                  
                  
      )), # end tabItem nmf1 
      
      tabItem(tabName = 'nmf2', 
              tabBox(id = 't-nmf', width = 12,
                     tabPanel('HeatMaps',
                     
                     br(),
                     br(),
                     
                    fluidRow(
                      column(width = 10, withSpinner(plotlyOutput('heatmap_nmf_clust'))),
                      column(width = 2, div(class = 'pull-right', 
                                            dropdown(
                                              inputId = 'T1', label = 'Show table', right = TRUE, 
                                              icon = icon('table'), style = 'jelly', color = 'primary', size = 'xs',
                                              div(style = 'max-height: 300px; overflow-y: scroll',
                                                  uiOutput('nmf_cluster_ids'))
                                            )))),
                    fluidRow(
                      br(), 
                      br(),
                      column(width = 10, withSpinner(plotlyOutput('heatmap_nmf_clust2'))))),

                    
                    tabPanel('Histogram',
                    
                    br(),
                    br(),

                    
                    fluidRow(
                      column(width = 10, withSpinner(plotOutput('E_cluster_hist'))),
                      column(width = 2, div(class = 'pull-right',
                                            dropdown(
                                              inputId = 'T2', label = 'Show table', right = TRUE, 
                                              icon = icon('table'), style = 'jelly', color = 'primary', size = 'xs',
                                              div(style = 'max-height: 300px; overflow-y: scroll',
                                                  uiOutput('nmf_cluster_table')))
                                ))),
                    downloadButton("download_hist", "Download histogram")
                    
                    
                     
                     ))), # end tabitem nmf2
      
      
      tabItem(tabName = 'categorical', 
              tabBox(id="t4", width = 12, 
                     
                     
                     tabPanel("Binary", icon=icon("circle-info"),
                              
                              fluidRow(
                                column(width = 5,
                                       uiOutput("select_cat_id"),
                                       uiOutput("select_cat_response")),
                                column(width = 4, 
                                       uiOutput("out_result_matrix_cat")),
                                column(width = 4,
                                       gt_output('test_gt'))),
                              
                              
                              
                              br(),
                              br(),
                              
                              fluidRow(
                                column(width = 5,
                                       uiOutput("select_cat_predictor")
                                )),
                              fluidRow(
                                column(width = 12,
                                       verbatimTextOutput("summary_gen_linear_model")
                                ))
                              
                              
                              
                     ),
                     
                     tabPanel('Test statistics', icon = icon('uncharted'),
                              verbatimTextOutput('fisher_test')
                              
                              ),
                     
                     tabPanel('All summaries output', icon = icon('uncharted'), 
                              verbatimTextOutput("create_summaries_cat")
                              
                     ),
                     
                     tabPanel('Data set', icon = icon('database'),
                              DT::dataTableOutput("out_merge_id_response_cluster_cat")
                              
                     )
                     
              )), # end tabItem 'categorical'
      
      tabItem(tabName = 'continuous', 
              tabBox(id="t4", width = 12, 
                     
                     
                     tabPanel("Continous response", icon=icon("circle-info"),
                              
                              fluidRow(
                                column(width = 5,
                                       uiOutput("select_cont_id"),
                                       uiOutput("select_cont_response")),
                                column(width = 4, 
                                       uiOutput("out_result_matrix_cont"))),
                              br(),
                              br(),
                              fluidRow(
                                column(width = 5,
                                       uiOutput("select_cont_predictor"))),
                              fluidRow(
                                column(width = 12,
                                       verbatimTextOutput("summary_linear_model"))),
                              

                              

                                
                              
                     ),
                     
                     
                     tabPanel('Test statistics', icon = icon('uncharted'),
                              
                              conditionalPanel(
                                condition = "input.k_groups == 2",
                                
                                verbatimTextOutput("t_test"),
                                verbatimTextOutput("wilcox_test"),
                                verbatimTextOutput("cor_test1"),
                                verbatimTextOutput("cor_test2")
                              ),
                              
                              conditionalPanel(
                                condition = "input.k_groups > 2",
                                verbatimTextOutput("aov_test")
                              )
                     ),
                     
                     tabPanel('Box plot', icon = icon('chart-column'),
                              
                              fluidRow(
                                column(width = 12,
                              
                                  fluidRow( 
                                    withSpinner(plotOutput("ggbox")))))
    
                    ),
                     


                    
                    tabPanel('All summaries output', icon = icon('uncharted'), 
                             verbatimTextOutput("result_summaries")
                             
                    ),
                     
                     tabPanel('Data set', icon = icon('database'),
                              
                              DT::dataTableOutput("out_merge_id_response_cluster")
                              
                     )
                     
              )), # end tabitem 'continious'
      

      
      tabItem(tabName = "survival",
    
              tabBox(id="t4", width = 12, 
                     tabPanel("Prepare dataset", icon=icon("circle-info"),

                              uiOutput("select_surv_id"),
                              uiOutput("select_surv_time"),
                              uiOutput("select_surv_event"),
                              uiOutput("select_cluster"),
                              br(),
                              DT::dataTableOutput('dataMerge')
                              
   
                     ),
                     tabPanel("Kaplan-Meier", icon = icon('timeline'),
                       fluidRow(
                         column(width = 4, 
                                
                                checkboxInput('censor', label = "Add Consor marks", value = TRUE),
                                checkboxInput('confidence', label = "Add Confidence interval", value = FALSE),
                                checkboxInput('legend', label = "Add Legend title", value = TRUE),
                                checkboxInput('pval', label = "Add P-value", value = TRUE), 
                                checkboxInput('quantile', label = "Add quantile", value = FALSE),
                                checkboxInput('risktable', label = "Add Risktable", value = FALSE),
                                checkboxInput('risksym', label = 'Add Risktable Strata symbol', value = FALSE)),
                         column(width = 8,
                              withSpinner(plotOutput("nmf_clustered_data")))),
                       
                       downloadButton("download_surv", "Download survival plot")
                     ),
                       
                     
                     tabPanel("Test statistics", icon=icon("uncharted"), 
                              verbatimTextOutput("summary_y_km"), 
                              verbatimTextOutput("summary_survfit_km"),
                              verbatimTextOutput("survdiff_km")

                              )

              )

      
      ) # end tabItem "survival"
   ) # end tabitems 
  )# end dashboard body
) # end dashboard page 
