library(visNetwork)
shinyUI(fluidPage(
     wellPanel(fileInput('file1', 'Upload a CSV File that contains the following fields: from, to, weight, group and entry_message', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
     ),
	hr(),
    
	fluidRow(
		wellPanel(uiOutput("Box1")),
		tabsetPanel(
			tabPanel("Discussion Interactions",visNetworkOutput('visnetwork')),
			tabPanel("Community Detection",visNetworkOutput('visgroup'))
		)
	)))

