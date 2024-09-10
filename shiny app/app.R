#### ASSIGNMENT 3 TASK 1 ####
#' loading the required packages to properly work with the Shiny objects
library(shiny)
if (!require(shinyWidgets)) {
	install.packages("shinyWidgets")
}
if (!require(shinyjs)) {
	install.packages("shinyjs")
}

library(ggplot2)  # Visualization
library(dplyr)    # Manipulation of the data set

#' Load the data
Bournemouth <-
	read.csv("BOURNEMOUTH.csv", header = T, sep = ",")

#' Some adjustments must be made to the data set; in order to include 
#' visualization by the district, the postcode prefix must be singled out
Bournemouth$postcode_prefix <- 
	sapply(Bournemouth$postcode,function(x){strsplit(x,split=' ')[[1]][1]})

#' a few worries arise: 
#' a single unit from  postcode SO30 is observed but it is wrongly  placed in 
#' the Bournemouth county as it should belong to a different postcode area.
#' the levels BH12, BH14, BH22, BH23 of the postcode district show few units 
#' (9 in total) and compared to the remaining levels of the category (i.e. from 
#' BH1 to BH11) they do not bring enough information to the dataset.
#' 
#' These levels of the category will be discarded

Bournemouth <- 
	Bournemouth %>% filter(postcode_prefix %in% paste0(rep("BH",11),1:11))

Bournemouth <- 
	Bournemouth %>% mutate(year_0 = year - min(year) ,
												 propertytype_epc = as.factor(propertytype_epc))

model <- glm(
	price ~ tfarea * numberrooms + tfarea * propertytype_epc +
		CURRENT_ENERGY_EFFICIENCY + ENVIRONMENT_IMPACT_CURRENT + 
		postcode_prefix + year_0 ,
	family = Gamma(link = "log") ,
	data = Bournemouth
)

name_var <- c(
	"Price" = "price",
	"Floor Area" = "tfarea" ,
	"Number of Rooms" = "numberrooms" ,
	"Energy Efficiency Rating" = "CURRENT_ENERGY_EFFICIENCY" ,
	"Environmental Impact" = "ENVIRONMENT_IMPACT_CURRENT"
)
 
#' the chosen layout is a navigation bar with components for the different 
#' questions of the task

ui <- navbarPage(
	div("Bournemouth Estate Investigation",
			style = "color:darkblue; font-size:30px; font-family:strong"),
	           #general title for the navbar
	
	useShinyjs(),
	setBackgroundColor( c("lightyellow", "seashell") ,
											gradient = "radial",
											direction = c("bottom", "right")),   
	tabPanel(
		div("VISUALISATION",
				style = "font-size:13px, color:black"),
		
		titlePanel(div("HOUSE FEATURES RELATIONSHIPS",
										style = "font-family:times; color:#3900B3",
									 align = "center")),
		br(),
		plotOutput("var_plot"),
		hr(),
		wellPanel(
			fluidRow(
				column(6,
							 div(h4("VARIABLES TO DISPLAY"),
							 		style = "color:#5A27C6"),
							 selectInput(
							 	inputId = "var1",
							 	label = "Variable 1",
							 	choices = list(
							 		"Price" = "price",
							 		"Floor Area" = "tfarea" ,
							 		"Number of Rooms" = "numberrooms" ,
							 		"Energy Efficiency Rating" = "CURRENT_ENERGY_EFFICIENCY" ,
							 		"Environmental Impact" = "ENVIRONMENT_IMPACT_CURRENT" ),
							 	selected = "tfarea"
							 ),
							 selectInput(
							 	inputId = "var2",
							 	label = "Variable 2",
							 	choices = list(
							 		"Price" = "price" ,
							 		"Floor Area" = "tfarea" ,
							 		"Number of Rooms" = "numberrooms" ,
							 		"Energy Efficiency Rating" = "CURRENT_ENERGY_EFFICIENCY" ,
							 		"Environmental Impact" = "ENVIRONMENT_IMPACT_CURRENT" ),
							 	selected = "price"
							 ), 
							 helpText(span("Variable 1 is plotted horizontally,
							 				 while variable 2 refers to the vertical axis.",
							 				 "By selecting the same variable for both choices
							 				 a", em("density plot"), 
							 				 "will be shown instead of a", 
							 				 em("jitter plot.") 
							 				 ) )
				),
				column(
					4,
					offset = 2,
					br(),
					br(),
					radioGroupButtons(
						inputId = "county_or_district",
						label = "Group by",
						choices = list("COUNTY" = T,
													 "POSTCODE DISTRICT" = F),
						selected = T,
						individual = TRUE
					),
					disabled(
						selectizeInput(
							inputId = "postcode_pre",
							label = NULL,
							choices = paste0(rep("BH", 11), 1:11),
							selected = "BH1",
							width = "60%"
						)
					)
				)
			)#end of FluidRow
			)#end of the wellpanel
	) , #end of tabPanel 1
	
	tabPanel(
		div("ESTIMATES AND PREDICTIONS",
				style = "font-size:13px, color:black"),
		
		sidebarLayout(
			
			sidebarPanel(
				width = 3,
				numericInput(
					"area",
					label = "Floor Area",
					value = min(Bournemouth$tfarea)	) ,
				
				numericInput("nroom",
										 label = "Number of Rooms",
										 value = 1) ,
				
				selectInput(
					"prop_type",
					label = "Type of Property",
					choices = levels(Bournemouth$propertytype_epc)
				),
				
				numericInput("en_eff",
										 label = "Energy Efficiency Rating",
										 value = 1 ) ,
				
				numericInput("env_imp",
										 label = "Environmental Impact",
										 value = 1 ) ,
				
				selectizeInput(
					inputId = "pcode_mod",
					label = "Postcode District",
					choices = paste0(rep("BH", 11), 1:11),
					selected = NULL ) ,
				
				numericInput(
					inputId = "year",
					label = "Year of the Final Purchase",
					value = 2011
				)
				
			), 
			
			mainPanel(
				tabsetPanel(
					
					tabPanel(
						"ESTIMATION",
						div( h1("ESTIMATED PRICE AND PREDICTION"),
								 style = "font-family:times; color:#3900B3",
								 align = "center" ) ,
						br(),
						span(
							textOutput("estimate"),
							align = "adjust",
							style = "color:#83097D;
							 font-size:28px;
							 font-family:times"
						) ,
						br(),
						hr(),
						plotOutput("year.est"),
						br(),
						br()
					) ,
				
				tabPanel("EXTENSION",
								 div( h1("EFFECT OF MODIFICATIONS ON THE HOUSE"), 
								 		 style = "font-family:times; color:#3900B3",
								 		 align = "center" ),
								 fluidRow(
								 	column(3,
								 				 align = "right",
								 				 style = "margin-top:32px",
								 				 prettyRadioButtons(
								 				 	"nroom.logic",
								 				 	label = NULL,
								 				 	choices = list("Add" = T, "Reduce" = F),
								 				 	inline = T,
								 				 	bigger = T,
								 				 	plain = T,
								 				 	outline = T
								 				 ) ),
								 	column(
								 		3,
								 		align = "center",
								 		numericInput("nroom.ch",
								 								 "Rooms",
								 								 value = 0)
								 	),
								 	column(6,
								 				 align = "left",
								 				 style = "margin-top:30px",
								 				 helpText(
								 				 	"Difference in number of rooms with respect to 
                 				 	the value in the sidebar"))
								 ), br(),
								 fluidRow(
								 	column(3,
								 				 align = "right",
								 				 style = "margin-top:32px",
								 				 prettyRadioButtons(
								 				 	"area.logic",
								 				 	label = NULL,
								 				 	choices = list("Add" = T, "Reduce" = F),
								 				 	inline = T,
								 				 	bigger = T,
								 				 	plain = T,
								 				 	outline = T
								 				 )),
								 	column(
								 		3,
								 		align = "center",
								 		numericInput("area.ch",
								 								 "Floor Area",
								 								 value = 0)
								 	),
								 	column(6,
								 				 align = "left",
								 				 style = "margin-top:30px",
								 				 helpText(
								 				 	"Difference in squared meters with respect to 
                 				 	the value in the sidebar"))
								 ), br(),
								 fluidRow(
								 	column(
								 		3,
								 		align = "right",
								 		style = "margin-top:32px",
								 		prettyRadioButtons(
								 			"en_eff.logic",
								 			label = NULL,
								 			choices = list("Add" = T, "Reduce" = F),
								 			inline = T,
								 			bigger = T,
								 			plain = T,
								 			outline = T
								 		)
								 	),
								 	column(
								 		3,
								 		align = "center",
								 		numericInput("en_eff.ch",
								 								 "Energy Efficiency Rating",
								 								 value = 0)
								 	),
								 	column(
								 		6,
								 		align = "left",
								 		style = "margin-top:30px",
								 		helpText("Difference in the rating with respect to
                 				 	the value in the sidebar")
								 	)
								 ) ,
								 br(), br(),
								 span(
								 	textOutput("estimate2"),
								 	align = "adjust",
								 	style = "color:#83097D;
							 font-size:28px;
							 font-family:times"
								 ), 
							 br(),
							 span(
							 	textOutput("new_estimate"),
							 	align = "adjust",
							 	style = "color:#83097D;
							 font-size:28px;
							 font-family:times"
							 ),
							 span(
							 	textOutput("new_estimate2"),
							 	align = "adjust",
							 	style = "color:#83097D;
							 font-size:28px;
							 font-family:times"
							 )
				)# end of
				))

		)# end of Layout for TabPanel2
		
	)  # end of TabPanel2
)#end of ui

sign <- function(logic, add1, add2){
	if(logic){
		return(add1 + add2)
	}else{
		return(add1 - add2)
	}
}
server <- function(input, output) {
	
	observeEvent(
		input$county_or_district,
		{
			if(input$county_or_district){
				disable("postcode_pre")
			}else{
				enable("postcode_pre") }
		} )
	
	var_plotData <- reactive({
		if (input$county_or_district)
			return(Bournemouth)
		Bournemouth %>% filter(postcode_prefix == input$postcode_pre)
	})
	
	output$var_plot <- renderPlot({
		g <- ggplot(var_plotData())
		if (input$var1 == input$var2) {
			g +
				geom_density(
					aes(.data[[input$var1]]),
					color = "firebrick4",
					fill =  "peru",
					alpha = 0.4,
					linewidth = 0.5,
					linetype = 1
				) +
				xlab(names(name_var)[name_var == input$var1]) +
				theme_minimal() +
				theme(axis.title = element_text(
					face = "bold.italic",
					size = 18,
					colour = "dodgerblue4"
				),
				axis.text = element_text(size = 14)
				)
		} else{
			g +
				geom_jitter(aes(x = .data[[input$var1]] ,
												y = .data[[input$var2]])) +
				labs(x = names(name_var)[name_var == input$var1] ,
						 y = names(name_var)[name_var == input$var2]) +
				theme_minimal()+	
				theme(axis.title = element_text(
					face = "bold.italic",
					size = 18,
					colour = "dodgerblue4"
				),
				axis.text = element_text(size = 14)
				)
		}
	})
	
	predData <- reactive({
		newdata <- data.frame(
			tfarea = input$area ,
			numberrooms = input$nroom ,
			propertytype_epc = input$prop_type ,
			CURRENT_ENERGY_EFFICIENCY = input$en_eff ,
			ENVIRONMENT_IMPACT_CURRENT = input$env_imp ,
			postcode_prefix = input$pcode_mod ,
			year_0 = input$year - min(Bournemouth$year)
		)
	})
	
	pred <- reactive({
		validate(
			need(input$area != "" , "Fill in all the empty fields"),
			need(input$nroom != "" , "Fill in all the empty fields"),
			need(input$en_eff != "" , "Fill in all the empty fields"),
			need(input$env_imp != "" , "Fill in all the empty fields"),
			need(input$year != "" , "Fill in all the empty fields"),
			need(input$pcode_mod %in% paste0(rep("BH",11),1:11),
					 "Write an appropriate postcode district")
		)
		predict(model, predData(), type = "response")
	})
	
	pred.ch <- reactive({
		validate(
			need(
				input$area.logic == T | (input$area > input$area.ch),
				"the selected reduction of squared meters is more than the total floor area of the house"
			),
			need(
				input$nroom.logic == T | (input$nroom > input$nroom.ch),
				"the selected reduction of the number of rooms is more than the rooms in the house"
			),
			need(
				(input$en_eff.logic == T) | (input$en_eff > input$en_eff.ch),
				"the selected reduction of points is more than the actual energic efficiency rating"
			)
		)
		# 
		a <-  predData() %>% mutate(
			tfarea = sign(input$area.logic,
										tfarea,
										input$area.ch),
			numberrooms = sign(input$nroom.logic,
												 numberrooms,
												 input$nroom.ch),
			CURRENT_ENERGY_EFFICIENCY = sign(
				input$en_eff.logic,
				CURRENT_ENERGY_EFFICIENCY,
				input$en_eff.ch

			)
		)
		predict(model, a, type = "response")
	})
	
	output$estimate2 <- output$estimate <- renderText({
		paste(
			"The estimated price of the house with the inserted feature is ",
			round(pred(), 2),
			"£ (pounds)")

	})
	
	output$year.est <- renderPlot({
		newdata2 <- data.frame(
			tfarea = input$area ,
			numberrooms = input$nroom ,
			propertytype_epc = input$prop_type ,
			CURRENT_ENERGY_EFFICIENCY = input$en_eff ,
			ENVIRONMENT_IMPACT_CURRENT = input$env_imp , 
			postcode_prefix = input$pcode_mod ,
			year_0 = 0:14
		)
		validate(
			need(input$area != "" , "Fill in all the empty fields"),
			need(input$nroom != "" , "Fill in all the empty fields"),
			need(input$en_eff != "" , "Fill in all the empty fields"),
			need(input$env_imp != "" , "Fill in all the empty fields"),
			need(input$pcode_mod %in% paste0(rep("BH",11),1:11),
					 "Write an appropriate postcode district")
		)
		ggplot() + geom_point(aes( x = 2011:2025 ,
															y = predict(model, newdata2, type = "response") ),
													color = "deepskyblue3",
													size = 4) +
			labs(y = "estimated price (pounds)") + theme_minimal() + 
			geom_vline(xintercept = 2019,
								 colour = "orangered2",
								 linewidth = 1.6) +
			scale_x_discrete( "Year", limits = 2011:2025 ) +
			theme(axis.title = element_text(
				face = "bold.italic",
				size = 18,
				colour = "dodgerblue4"
			),
			axis.text = element_text(size = 14))
	})
	
	output$new_estimate <- renderText({
			paste(
				"Adjusting for the modifications, the new estimated price is ",
				round(pred.ch(), 2),
				"£ (pounds)."
			)
	})
	output$new_estimate2 <- renderText({
		paste(
			"The estimate has ",
			if_else(pred() > pred.ch(), "decreased", "increased"),
			" of",
			round(abs(pred() - pred.ch()), 2),
			"£ (pounds)."
		)
	})

}

shinyApp(ui = ui, server = server)