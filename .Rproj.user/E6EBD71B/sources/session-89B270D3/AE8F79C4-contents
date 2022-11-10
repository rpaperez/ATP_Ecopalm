# Load packages -----------------------------------------------------------

packs <- c('shiny','datasets',"lubridate",'plotly')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

shinyUI(fluidPage(
  headerPanel("ECOPALM & ATP reserves database"),
  fluidRow(
    column(2,
           checkboxGroupInput("Project", label = h4("Select project(s)"), 
                              choices = list("ATP" = 'ATP', "Ecopalm" = 'Ecopalm'),
                              selected = NULL)
    ),
    column(2,
           checkboxGroupInput("checkGroup", label = h4("Select site(s)"), 
                              choices = list("KNDE" = 'KNDE', "PHLE" = 'PHLE', "BMLE" = 'BMLE'),
                              selected = NULL)
    )
  )
  ,
  tabsetPanel(
    tabPanel("Growth & Production",
             fluidRow(
               column(3,
                      selectInput(inputId="x_var", label = h4("Select x variable"), 
                                  choices = list('NbLeaf',"NewLeafDate","PruningLeafDate" ,"DryLeafDate","BrokenLeafDate","HarvestingBunchDate","SpatheOpeningDate",
                                                 'SpearNb',
                                                 "PetioleLength","PetioleFW","PetioleWaterContent","PetioleDW",
                                                 "RachisLength","RachisFW","RachisWaterContent","RachisDW",
                                                 "LeafletNbRigth","LeafletNbLeft","LeafletTotNb","LeafletFW","LeafletWaterContent","LeafletDW", 'LealfetLength','LeafletMaxWidth','LealfetArea','SLA',
                                                 "BunchFW","BunchDW",
                                                 "PeduncleFW","PeduncleWaterContent","PeduncleDW",
                                                 'SpikeletNb',"SpikeletFW","SpikeletWaterContent","SpikeletDW",
                                                 "ParthenoFruitNb","ParthenoFruitFW","ParthenoFruitWaterContent","ParthenoFruitDW",
                                                 "FruitNb","FruitFW","FruitWaterContent","FruitDW",
                                                 "PulpSampleFW","PulpSampleDW","NutSampleFW","NutSampleDW"),
                                  selected = NULL)
                      
                      
               ),
               column(3,
                      selectInput(inputId="y_var", label = h4("Select y variable"), 
                                  choices = list("PetioleLength","PetioleFW","PetioleWaterContent","PetioleDW",
                                                 "RachisLength","RachisFW","RachisWaterContent","RachisDW",
                                                 'SpearNb',
                                                 "LeafletNbRigth","LeafletNbLeft","LeafletTotNb","LeafletFW","LeafletWaterContent","LeafletDW",'LealfetLength','LeafletMaxWidth','LealfetArea','SLA',
                                                 "BunchFW","BunchDW",
                                                 "PeduncleFW","PeduncleWaterContent","PeduncleDW",
                                                 'SpikeletNb',"SpikeletFW","SpikeletWaterContent","SpikeletDW",
                                                 "ParthenoFruitNb","ParthenoFruitFW","ParthenoFruitWaterContent","ParthenoFruitDW",
                                                 "FruitNb","FruitFW","FruitWaterContent","FruitDW",
                                                 "PulpSampleFW","PulpSampleDW","NutSampleFW","NutSampleDW"),
                                  selected = NULL)
                      
                      
               ),
               column(2,
                      checkboxGroupInput("Outliers", label = h4(""), 
                                         choices = list("Remove outliers" = 'remove'),
                                         selected = NULL)
               ),
               column(2,
                      checkboxGroupInput("Average", label = h4(""), 
                                         choices = list("Average" = 'average'),
                                         selected = NULL)
               ),
               fluidRow(
                 column(12,
                        plotlyOutput('graph')
                 )
               )
             )
    ),
    tabPanel("Phenology",
             fluidRow(
               column(3,
                      selectInput(inputId="pheno_x_var", label = h4("Select x variable"), 
                                  choices = list('Months after planting'='NewLeafMAP','Date'='NewLeafMonth'),
                                  selected = NULL)
                      
                      
               ),
               column(3,
                      selectInput(inputId="pheno_y_var", label = h4("Select y variable"), 
                                  choices = list('# emmitted leaves (Rank 1)'= 'Leaves',
                                                 "# male inflo" = 'male_inflo',
                                                 "# female inflo" = 'female_inflo',
                                                 "# harvested bunches" = 'Bunches'
                                  ),
                                  selected = NULL)
                      
                      
               ),
               column(2,
                      checkboxGroupInput("AveragePheno", label = h4(""), 
                                         choices = list("Average" = 'average'),
                                         selected = NULL)
               ),
               fluidRow(
                 column(12,
                        plotlyOutput('graph_pheno')
                 )
               )
             )
    )
    
  )
))


