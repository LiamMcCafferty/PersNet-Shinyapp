#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Detatches all packages from current iteration of R, most packages interfere with this code
detach_all_packages <- function() {
	##########
	# Function: Detatches all attatched packages from current instance of R
	# Inputs: none, just call the function
	# Ouputs: none
	# Credit to mjaniec on stack overflow for function logic
	##########  
	basic.packages <- c("package:stats", "package:graphics", "package:grDevices",
											"package:utils", "package:datasets",
											"package:methods", "package:base")
	package.list <- search()[ifelse(unlist(gregexpr("package:",
																									search())) == 1, TRUE, FALSE)]
	package.list <- setdiff(package.list, basic.packages)
	if (length(package.list) > 0)  for (package in package.list) detach(package,
																																			character.only = TRUE)
}
detach_all_packages()
rm(list = ls())

library(shiny)
library(tidyverse)
library(igraph)
# library(statnet.common)
# library(network)


#Comment this out before running program
input_frame <- read.csv("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/Zero_Color Networks Work/raw_w_smalls.csv",
												stringsAsFactors = FALSE)
data_dict <- read.csv("~/Dropbox (Partners HealthCare)/BWH Backup - Liam/Zero_Color Networks Work/PersonalNetworksInstrumentDemo_DataDictionary_2020-03-13.csv",
											stringsAsFactors = FALSE) %>%
	rename("Variable_Field_Name" = "Variable...Field.Name",
				 "Form_Name" = "Form.Name",
				 "Section_Header" = "Section.Header",
				 "Field_Type" = "Field.Type",
				 "Field_Label" = "Field.Label",
				 "Choices_Calculations_or_Slider_Labels" =
				 	"Choices..Calculations..OR.Slider.Labels",
				 "Field_Note" = "Field.Note",
				 "Text_Validation_Type_OR_Show_Slider_Number" =
				 	"Text.Validation.Type.OR.Show.Slider.Number",
				 "Text_Validation_Min" = "Text.Validation.Min",
				 "Text_Validation_Max" = "Text.Validation.Max",
				 "Identifier_question" = "Identifier.",
				 "Branching_Logic" = "Branching.Logic..Show.field.only.if....",
				 "Required_Field_question" = "Required.Field.",
				 "Custom_Alignment" = "Custom.Alignment",
				 "Question_Number_surveys_only" = "Question.Number..surveys.only.",
				 "Matrix_Group_Name" = "Matrix.Group.Name",
				 "Matrix_Ranking?" = "Matrix.Ranking.",
				 "Field_Annotation" = "Field.Annotation")

#Appearently yes/no objects exist, to cut this off early, we will turn them
#  into radios and change their choices to a default of 0 = no, 1 = yes.
data_dict[data_dict$Field_Type == "yesno",]$Choices_Calculations_or_Slider_Labels <-
	"0, No | 1, Yes"
data_dict[data_dict$Field_Type == "yesno",]$Field_Type <-
	"radio"

input <- list()
input$survey_identifier <- "record_id"
input$instrument_select <- "personal_network_survey_for_clinical_research"
input$display_texttry <- FALSE
input$texttry <- "0,1,2,NA"

rm(list = ls())


# Define UI for data upload app ----
ui <- fluidPage(
	
	# App title ----
	titlePanel("Uploading Files"),
	
	# Sidebar layout with input and output definitions ----
	sidebarLayout(
		
		# Sidebar panel for inputs ----
		sidebarPanel(
			
			# Input: Select a file ----
			fileInput("file1", "Choose Raw Survey Output as CSV File",
								multiple = FALSE,
								accept = c("text/csv",
													 "text/comma-separated-values,text/plain",
													 ".csv")),
			
			# Adaption Check ----
			checkboxInput("adapt", "Are you using an adapted version of this survey?"),
			conditionalPanel(
				condition = "input.adapt == true",
				# Input: REDCap Data Dictionary ----
				fileInput("file2", "Choose REDCap Data Dictionary CSV File",
									multiple = FALSE,
									accept = c("text/csv",
														 "text/comma-separated-values,text/plain",
														 ".csv")),
				# Input: reveal texttry question ----
				checkboxInput("display_texttry",
											"Does your adapted survey contain associated values for ties different than: no tie (e.g. stranger), weak tie (e.g. in-between), and strong tie (e.g. especially close)?"),
				
			),
			
			
			
			# Horizontal line ----
			tags$hr(),
			
			# ID Check ----
			textInput(inputId = "survey_identifier",
								label = "What is the name of the ID which connects each record?",
								value = "record_id"),
			
			# Instrument Processing Selection ----
			conditionalPanel(
				condition = "input.adapt == true",
				# Input: REDCap Data Dictionary ----
				radioButtons("instrument_select",
										 "Which instrument would you like to process",
										 c("[instrument_1]"))
			),
			
			
			# Horizontal line ----
			tags$hr(),
			
			# Input: Select number of rows to display ----
			radioButtons("disp", "Display",
									 choices = c("Dataframe" = "dataframe",
									 						"Data Dictionary" = "data_dictionary",
									 						"Alter Frame" = "alter_frame",
									 						"Leveled Dataframe" = "leveled_dataframe",
									 						"Network Structure" = "network_structure",
									 						All = "all"),
									 selected = "dataframe"),
			
			downloadButton("downloadContents", "Download .csv"),
			downloadButton("downloadRDA", "Download .rda"),
			
			conditionalPanel(
				# Input: texttry for different ties -----
				condition = 'output.panelStatus',
				p("It appears your code contains values for ties which are different than our traditional setup of: No Tie < Weak Tie < Strong Tie. As we are unable to parse which is which please rank the given tie values from none (0), then from weakest to strongest in ascending order. If a a value is a 'Don't Know' or some sort of unknown value please enter either an NA or whatever tie level you feel best to impute these for. Unfortunately we don't yet have a method of NA imputation, so currently we assume all missing ties are 'no ties'"),
				textInput("texttry", "[Text Output]")
			)

		),
		
		# Main panel for displaying outputs ----
		mainPanel(
			
			# Output: Data file ----
			tableOutput("contents"),
			# plotOutput("plontents"),
			textOutput("panelStatus") #Currently this allows us to ping texttry,
			                          #  should figure out a better way to ping the
			                          #  observe event....
			
		)
		
	)
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
	
	# Importing Raw Data ----
	frame_make <- reactive({
		req(input$file1)
		
		# when reading semicolon separated files,
		# having a comma separator causes `read.csv` to error
		tryCatch(
			{
				input_dataset <- read.csv(input$file1$datapath,
													header = TRUE,
													sep = ",",
													quote = '"',
													stringsAsFactors = FALSE)
				return(input_dataset)
			},
			error = function(e) {
				# return a safeError if a parsing error occurs
				stop(safeError(e))
			}
		)
	})
	
	# Importing Data Dictionary ----
	data_dict_make <- reactive({
		req(input$file2)
		
		# when reading semicolon separated files,
		# having a comma separator causes `read.csv` to error
		tryCatch(
			{
				data_dict <- read.csv(input$file2$datapath,
													header = TRUE,
													sep = ",",
													quote = '"',
													stringsAsFactors = FALSE)
				# print(colnames(data_dict))
				
				#Due to strange characters contained w/in data dictionary we are renaming it
				#  for ease of programming. This is a potential break point for the code in
				#  the case that the names for the data dictionary are different (they
				#  shouldn't be)
				data_dict %>%
					rename("Variable_Field_Name" = "Variable...Field.Name",
								 "Form_Name" = "Form.Name",
								 "Section_Header" = "Section.Header",
								 "Field_Type" = "Field.Type",
								 "Field_Label" = "Field.Label",
								 "Choices_Calculations_or_Slider_Labels" =
								 	"Choices..Calculations..OR.Slider.Labels",
								 "Field_Note" = "Field.Note",
								 "Text_Validation_Type_OR_Show_Slider_Number" =
								 	"Text.Validation.Type.OR.Show.Slider.Number",
								 "Text_Validation_Min" = "Text.Validation.Min",
								 "Text_Validation_Max" = "Text.Validation.Max",
								 "Identifier_question" = "Identifier.",
								 "Branching_Logic" = "Branching.Logic..Show.field.only.if....",
								 "Required_Field_question" = "Required.Field.",
								 "Custom_Alignment" = "Custom.Alignment",
								 "Question_Number_surveys_only" = "Question.Number..surveys.only.",
								 "Matrix_Group_Name" = "Matrix.Group.Name",
								 "Matrix_Ranking?" = "Matrix.Ranking.",
								 "Field_Annotation" = "Field.Annotation") ->
					data_dict
				# print(colnames(data_dict))
				
				#Appearently yes/no objects exist, to cut this off early, we will turn them
				#  into radios and change their choices to a default of 0 = no, 1 = yes.
				data_dict[data_dict$Field_Type == "yesno",]$Choices_Calculations_or_Slider_Labels <-
					"0, No | 1, Yes"
				data_dict[data_dict$Field_Type == "yesno",]$Field_Type <-
					"radio"
				
				return(data_dict)
			},
			error = function(e) {
				# return a safeError if a parsing error occurs
				stop(safeError(e))
			}
		)
	})
	
	# Checking instrument selection ----
	observe({
		req(input$file2)
		#Imports data dictionary
		data_dict <- data_dict_make()
		#Forms are the unique instruments in case multiple instruments exist
		forms <- unique(data_dict$Form_Name)
		
		
		#Ask the user to select which instrument they want to process
		updateRadioButtons(session, "instrument_select",
											 label = "Which instrument would you like to process",
											 choices = forms,
											 selected = forms
		)
		
	})
	
	# Creating Level List ----
	create_level_list <- reactive({
		req(input$file2, input$instrument_select, input$survey_identifier)
		
		data_dict <- data_dict_make()
		
		#Checks if the inputted id input$survey_identifier is actually in the
		#  dataframe. If not this will pop up an error for the user to fix.
		validate(
			need(input$survey_identifier %in% data_dict$Variable_Field_Name,
						 paste("Input survey ID does not match,",
						 			input$survey_identifier, "not found"))
			)
		
		#Needed here:
		#   recognize if there are multiple instruments. (Maybe ask which one they want to process?)
		#   keep id locked to each instrument.
		#   
		data_dict %>%
			subset(Variable_Field_Name == input$survey_identifier |
						 	Form_Name == input$instrument_select) -> data_dict
		
		data_dict %>%
			subset(!grepl("^name_[0-9]+$|^tie[0-9]+$|^a_tie[0-9]+$", Variable_Field_Name) &
						 	Field_Type %in% c("radio", "checkbox", "dropdown")) %>%
			mutate(Variable_Field_Name_Mod = sub("^name[0-9]+","ALTER",Variable_Field_Name)) ->
			data_dict_levelsave
		
		data_dict_levelsave$Choices_Calculations_or_Slider_Labels %>%
			strsplit("\\|") %>%
			lapply(trimws) %>%
			lapply(sub, pattern = ",", replacement = ";;;") %>%
			sapply(function(x){
				strsplit(x, ";;;") %>% sapply("[[", 1) %>% as.integer() %>%
					"names<-"(strsplit(x, ";;;") %>% sapply("[[", 2))
			}) %>%
			"names<-"(data_dict_levelsave$Variable_Field_Name) ->
			level_list
		
		attributes(level_list)$var_type <- data_dict_levelsave$Field_Type
		attributes(level_list)$persnet_var_type <-
			ifelse(grepl("^ALTER", data_dict_levelsave$Variable_Field_Name_Mod),
						 "alter",
						 "single")
		
		return(level_list)
	})
	
	# Creating dataframe w/ leveled ego single/checkbox variables ----
	leveled_singles <- reactive({
		req(input$file1, input$instrument_select, input$survey_identifier)
		
		level_list <- create_level_list()
		input_frame <- frame_make()
		
		# Leveling single vectors
		for(i in 1:length(level_list)){
			
			if(!attr(level_list, "var_type")[i] %in% c("radio", "dropdown")) next
			
			input_frame[[names(level_list)[i]]] <-
				factor(input_frame[[names(level_list)[i]]],
							 levels = level_list[[i]],
							 labels = names(level_list[[i]]))
		}
		
		# Constructing levels for single checkboxes
		level_list %>% subset(attr(level_list, "var_type") == "checkbox" &
														attr(level_list, "persnet_var_type") == "single") ->
			level_list_checkbox
		
		if(length(level_list_checkbox) > 0){
			#Isolating list of all single checkbox variables
			level_list_checkbox %>% names() %>%
				lapply(function(x){
					paste0(x, "___", level_list_checkbox[[x]]) %>%
						"names<-"(names(level_list_checkbox[[x]]))
				}) %>% "names<-"(names(level_list_checkbox)) ->
				level_list_checkbox
			
			#Iterating through each checkbox variable, and using method previously used in
			#  cleandata2 to construct a grouped dataframe w/ data
			for(i in 1:length(level_list_checkbox)){
				input_frame[c(input$survey_identifier, level_list_checkbox[[i]])] %>%
					"colnames<-"(c("id", trimws(names(level_list_checkbox[[i]])))) %>%
					gather(value, count, -id) %>% filter(count == 1) %>% 
					arrange(id) %>% select(-count) %>%
					group_by(id) ->
					thing
				
				#Breaking apart single dataframe into multiple using slice() and making a
				#  list of them that we can then make long.
				thang <- list()
				
				for(j in 1:length(level_list_checkbox[[i]])){
					thang[[j]] <- thing %>% slice(j)
					names(thang[[j]]) <- c(input$survey_identifier,
																 paste0(names(level_list_checkbox)[i], j))
				}
				#Combining the dataframes recursively
				thang <- Reduce(function(...) merge(..., by=input$survey_identifier, all.x=TRUE), thang)
				
				#Removing columns w/ all NA's to clean up result
				thang <- thang[,is.na(thang) %>% apply(2,all) %>% "!"()]
				#Making each value into a factor
				thang[-1] <- lapply(thang[-1],
														factor,
														levels = trimws(names(level_list_checkbox[[i]])))
				#Joining constructed dataframe to the output dataframe
				input_frame <- left_join(input_frame, thang, by = input$survey_identifier)
			}
		}
		
		return(input_frame)
	})
	
	# Creates dataframe w/ tie values and their associated levels. ----
	#  Useful in case someone changes the number of weighted values for ties.
	name_values <- reactive({
		req(input$file2)
		data_dict <- data_dict_make()
		
		print("name_values")
		
		data_dict %>%
			subset(grepl("^a_tie[0-9]+$", Variable_Field_Name)) %>%
			"$"(Choices_Calculations_or_Slider_Labels) %>%
			unique() %>%
			strsplit("\\|") %>%
			sapply(trimws) %>%
			sub(pattern = ",", replacement =  ";;;", x = .) %>%
			strsplit(";;;") %>%
			lapply(trimws) %>%
			unlist() %>%
			matrix(ncol = 2, byrow = TRUE) %>%
			as.data.frame(stringsAsFactors = FALSE) %>%
			"colnames<-"(c("Integer", "assocValue")) %>% #-> name_values_df
			return()
	})
	
	# Returns a true/false statment to output$panelStatus ----
	#  which unhides the question for networks w/ more than 0,1,2. 
	observe({
		print(name_values())
		
		
		output$panelStatus <- reactive({
			# req(input$display_texttry)
			print(name_values())
			name_values_df <- name_values()
			
			print(paste("display_texttry:", input$display_texttry))

			print("panelStatus")
			(any(as.integer(name_values_df$Integer) > 2 |
				as.integer(name_values_df$Integer) < 0) | 
				input$display_texttry) %>% return()
		})
	})
	
	# Updates texttry question's text to tie levels ----
	observe({
		req(input$file2)
		name_values_df <- name_values()
		
		print("updateText")
		name_values_df %>%
			apply(1, function(input_row){
				paste0(input_row[2], " (", input_row[1], ")")
			}) %>%
			paste(collapse = ", ") ->
			output_text
		
		updateTextInput(session, "texttry",
										label = output_text)
	})
	
	# Outputs Network Structure DF ----
	network_structure <- reactive({
		#importing data
		req(input$file1)
		
		input_frame <- frame_make()
		name_values_df <- name_values()
		
		#Checks if the inputted id input$survey_identifier is actually in the
		#  dataframe. If not this will pop up an error for the user to fix.
		validate(
			need(input$survey_identifier %in% colnames(input_frame),
				 paste("Input survey ID does not match,", input$survey_identifier,
				 			"not found"))
			)
		
		#creating df's w/ each chunk of data w/ detection, id locked
		input_frame %>%
			"["(grepl("^name[0-9]+$|^more_names_[0-9]+$",colnames(input_frame)) |
						grepl(paste0("^",input$survey_identifier,"$"), colnames(input_frame))) ->
			names_frame
		
		input_frame %>%
			"["(grepl("^name_[0-9]+$",colnames(input_frame)) |
						grepl(paste0("^",input$survey_identifier,"$"), colnames(input_frame))) ->
			keep_remove_frame
		
		input_frame %>%
			"["(grepl("^tie[0-9]+$|^a_tie[0-9]+$",colnames(input_frame)) |
						grepl(paste0("^",input$survey_identifier,"$"), colnames(input_frame))) ->
			tie_frame
		
		#if statement for if the input data has ties which are not the normal 0, 1, 2,
		#  or have been indicated as such. Automatically replaces any values based
		#  upon user input. Has requirements that the question has been asked to the
		#  user and that the inputs match the length and don't have whitespace (in the
		#  case when a comma is hit but the value hasn't been entered yet).
		if((any(as.integer(name_values_df$Integer) > 2 |
						as.integer(name_values_df$Integer) < 0) | 
				input$display_texttry)){
			#requiring question being asked
			req(input$texttry)
			
			input$texttry %>%
				strsplit(",") %>%
				unlist() %>%
				trimws() -> input_ties
			#Requiring length to be the same, and no whitespace
			req(length(input_ties) == nrow(name_values_df) & !any(input_ties %in% ""))
			
			input_ties[input_ties == "NA" |
								 	input_ties == "N" |
								 	input_ties == "na" |
								 	input_ties == "n"] <- NA
			input_ties <- as.integer(input_ties)
			
			for(i in 1:nrow(name_values_df)){
				tie_frame[-1][apply(tie_frame[-1], 1, "%in%", as.integer(name_values_df$Integer[i])) %>% t()] <- input_ties[i]
			}
			
			print(paste(c("ties_imputation", input_ties)))
		}
		
		#Calculating how many ties appear to be present w/in our data
		tie_count <- length(keep_remove_frame)
		tie_combn_n <- factorial(tie_count) / (factorial(2) * factorial(tie_count - 2))
		
		#Function which construct matrices we use for structure calculations
		matrix_maker <- function(tie_vector, tie_count = NA, na.rm = TRUE){

			if(is.na(tie_count)){
				tie_combn_n <- sapply(2:30, function(x){factorial(x) / (factorial(2) * factorial(x - 2))})
				tie_count <- c(2:30)[(length(tie_vector) - 1) == tie_combn_n]
			}
			
			id <- tie_vector[input$survey_identifier]
			
			tie_vector_lim <- tie_vector %>%
				"["(!names(tie_vector) %in% input$survey_identifier) %>%
				unlist()
			
			mat <- matrix(NA, nrow = tie_count, ncol = tie_count)
			mat[lower.tri(mat)] <- tie_vector_lim
			mat <- mat %>% t()
			mat[lower.tri(mat)] <- tie_vector_lim
			colnames(mat) <- rownames(mat) <- c("EGO", paste0("name", 1:(ncol(mat) - 1)))
			(mat %>% apply(1, function(x){all(is.na(x))}) %>% "!"()) &
				(mat %>% apply(2, function(x){all(is.na(x))}) %>% "!"()) -> remove_ties
			mat <- mat[remove_ties, remove_ties]
			mat <- "diag<-"(mat, 0)
			
			if(na.rm){
				mat[is.na(mat)] <- 0
			}
			
			return(mat)
		}
		
		#Creating a list of matrices for our entire dataset
		mat_list <- apply(tie_frame, 1, matrix_maker, tie_count = tie_count) %>%
			"names<-"(c(tie_frame[input$survey_identifier] %>% unlist()))
		
		if(any(sapply(mat_list, ncol) < 3)){
			small_networks <- mat_list[sapply(mat_list, ncol) < 3]
			mat_list <- mat_list[!sapply(mat_list, ncol) < 3]
		}else{
			small_networks <- list()
		}
		
		graph_list <- lapply(mat_list, function(x){
			graph.adjacency(x[-1, -1], mode = "undirected", weighted = TRUE)
		})
		
		#Makes named vectors into dataframes. So I don't have to repeat this constantly
		named_vect_matrixer <- function(input_vect, id_name = "id", var_name = "var"){
			data.frame(names(input_vect), input_vect, stringsAsFactors = FALSE) %>%
				"colnames<-"(c(id_name, var_name))
		}
		
		#Calculating network size
		sapply(names_frame[[input$survey_identifier]],
					 function(input_id, names_frame, keep_remove_frame){
					 	
					 	keep_remove_row <- keep_remove_frame %>%
					 		subset(keep_remove_frame[[input$survey_identifier]] == input_id) %>%
					 		unlist() %>%
					 		"["(!grepl(paste0("^", input$survey_identifier, "$"),
					 							 colnames(keep_remove_frame)))
					 	
					 	names_frame_row <- names_frame %>%
					 		subset(names_frame[[input$survey_identifier]] == input_id) %>%
					 		unlist() %>%
					 		"["(!grepl(paste0("^", input$survey_identifier, "$"),
					 							 colnames(names_frame)))
					 	
					 	names_frame_row %>%
					 		"["(grepl("^more_names_[0-9]+$", names(names_frame_row))) ->
					 		more_names_row
					 	
					 	names_frame_row %>%
					 		"["(!grepl("^more_names_[0-9]+$", names(names_frame_row))) %>%
					 		"["(as.logical(keep_remove_row)) %>%
					 		gsub("[[:punct:]]", "", .) %>% trimws() %>% toupper() ->
					 		selected_names
					 	
					 	more_names_row %>%
					 		strsplit(",") %>%
					 		lapply(gsub, pattern = "[[:punct:]]", replacement = "") %>%
					 		lapply(trimws) %>% lapply(toupper) ->
					 		more_names_list
					 	
					 	lapply(more_names_list, function(more_names_list){
					 		more_names_list[!(more_names_list %in% selected_names)]
					 	}) %>% unlist() %>% c(selected_names) %>% length() -> netsize
					 	
					 	return(netsize %>% "names<-"(input_id))

		}, names_frame = names_frame, keep_remove_frame = keep_remove_frame) ->
			network_size
		
		#This should unload egonet in case its loaded in already
		if("egonet" %in% (.packages())){
			detach("package:egonet", unload=TRUE) 
		}
		# Calculate maximum, avg degree, and density of alters after removal of Ego
		max_degree <- unlist(lapply(graph_list, function(x){max(degree(x))}))
		
		mean_degree <- unlist(lapply(graph_list, function(x){mean(degree(x))}))
		
		density <- unlist(lapply(graph_list, function(x){graph.density(x)}))
		
		#*Important to note: The above is calculated WITHOUT THE EGO, whereas below is 
		#  calculated WITH THE EGO.*
		#Calculate constraint and effective size (Burt, 1992)
		
		library(egonet) # careful of interference with igraph functions. Do not load 
		#                 before calculating degree and density without EGO (above)
		
		#Calculate constraint with EGO
		constraint <- unlist(lapply(mat_list, function(x){as.numeric(index.egonet(x, 
																																					index = list("constraint")))}))
		
		#Calculate effective size with EGO
		effsize <- unlist(lapply(mat_list, function(x){as.numeric(index.egonet(x, 
																																			 index = list("effsize")))}))
		
		#Merging all into one dataframe, locked to id per each
		#  First we put all vectors into a list
		structure_vars <- lst(network_size, constraint, effsize, density, mean_degree, max_degree)
		#  We turn each into an id locked matrix from their names
		structure_vars <- lapply(structure_vars, named_vect_matrixer, id = input$survey_identifier)
		#  Renaming the second column in each dataframe as it would appear in dataset
		lapply(seq_along(structure_vars), function(i) {
			colnames(structure_vars[[i]]) <- c(colnames(structure_vars[[i]])[1],names(structure_vars)[i])
			return(structure_vars[[i]])
		}) %>% 'names<-'(names(structure_vars)) -> structure_vars
		#  Combining everything together
		structure_vars <- Reduce(function(...)
			merge(..., by=input$survey_identifier, all.x=TRUE),structure_vars)
		
		if(length(small_networks) > 0){
			lapply(small_networks, function(input_mat){
				if(nrow(input_mat) > 1){
					data.frame(constraint = 1,
										 effsize = 1,
										 density = NA,
										 mean_degree = 0,
										 max_degree = 0) %>%
						return()
				}else{
					data.frame(constraint = NA,
										 effsize = NA,
										 density = NA,
										 mean_degree = 0,
										 max_degree = 0) %>%
						return()
				}
			}) -> small_networks
			
			for(i in 1:length(small_networks)){
				small_networks[[i]][input$survey_identifier] <- names(small_networks)[i]
			}
			
			for(i in small_networks){
				structure_vars[
					structure_vars[[input$survey_identifier]] == i[[input$survey_identifier]],
					] %>% replace(names(i), i) ->
					structure_vars[
						structure_vars[[input$survey_identifier]] == i[[input$survey_identifier]],
						]
			}
			
		}
		
		return(structure_vars)
	})
	
	# Outputs Alter Frame DF -----
	alter_frame_maker <- reactive({
		input_frame <- frame_make()
		data_dict <- data_dict_make()
		level_list <- create_level_list()
		
		#creating df's w/ each chunk of data w/ detection, id locked
		input_frame %>%
			"["(grepl("^name[0-9]+$|^more_names_[0-9]+$",colnames(input_frame)) |
						grepl(paste0("^",input$survey_identifier,"$"), colnames(input_frame))) ->
			names_frame
		
		input_frame %>%
			"["(grepl("^name_[0-9]+$",colnames(input_frame)) |
						grepl(paste0("^",input$survey_identifier,"$"), colnames(input_frame))) ->
			keep_remove_frame
		
		# Radio Processing # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
		
		#Isolate level list to only radio alters
		level_list_alters_single <-
			level_list %>%
			subset(attr(level_list, "persnet_var_type") == "alter" &
						 	attr(level_list, "var_type") == "radio")
		
		if(length(level_list_alters_single) > 1){
			attributes(level_list_alters_single)$varname <-
				level_list_alters_single %>%
				names() %>%
				sub("^name[0-9]+", "", .)
			
			#lapply Function which iterates through each group of single alter data,
			# and constructs an alter_frame for it.
			lapply(unique(attr(level_list_alters_single, "varname")), function(i){
				level_list_alters_single_select <-
					level_list_alters_single %>%
					subset(attr(level_list_alters_single, "varname") == i)
				
				#Creaking list which contains values for each alter. We are making a list
				#  rather than a dataframe as the length of each vector will modulate based
				#  upon the count of selected inviduals.
				lapply(c(1:nrow(input_frame)), function(rownum){
					input_frame[rownum,c(input$survey_identifier,
															 names(level_list_alters_single_select))]
				}) -> workbench
				
				#Next we remove unselected names. Once we do this we assume all NA's are
				# genuinely missing data
				lapply(workbench, function(input_row){
					input_row <- input_row %>% unlist()
					wb_id <- input_row[input$survey_identifier]
					
					keep_remove_frame %>%
						"["(keep_remove_frame[[input$survey_identifier]] == wb_id,
								colnames(keep_remove_frame) != input$survey_identifier) %>%
						unlist() %>%
						as.logical() ->
						keep_remove_bench
					
					output_row <- input_row %>%
						"["(names(input_row) != input$survey_identifier) %>%
						"["(keep_remove_bench)
					
					return(c(wb_id, output_row))
				}) -> workbench
				
				#Next we construct a id-locked dataframe with information from each chunk
				#  of data
				lapply(workbench, function(input_row){
					input_row <- input_row %>% unlist()
					wb_id <- input_row[input$survey_identifier]
					input_row <- input_row[names(input_row) != names(wb_id)]
					
					#Breaking out of lapply in case of size 0 networks
					if(length(input_row) == 0)return()
					
					input_levels <- unlist(unique(level_list_alters_single_select))
					
					input_row %>%
						factor(levels = input_levels, labels = names(input_levels)) ->
						input_row
					
					data.frame(id = wb_id %>% "names<-"(NULL),
										 alter_id = sub(paste0(i, "$"), "", names(input_row)),
										 alter_num = sub("^name","",sub(paste0(i, "$"), "", names(input_row))),
										 value = input_row,
										 row.names = NULL,
										 stringsAsFactors = FALSE) ->
						output_frame
					
					output_frame %>%
						"colnames<-"(c("id", "alter_id", "alter_num", i)) %>%
						return()
					
				}) %>% "rownames<-"(NULL) -> workbench
				
				Reduce(function(...) rbind(...),workbench) %>% return()
			}) ->
				alter_bench
			
			#Reducing each alter_frame for the singles category into a single dataframe
			Reduce(function(...)
				left_join(..., by = c("id", "alter_id", "alter_num")),alter_bench) ->
				alter_frame_singles
			
		}else{
			alter_frame_singles <- data.frame(id = as.integer(character()),
																				alter_id = character(),
																				stringsAsFactors = FALSE)
		}
		
		# Checkbox Processing # # # # # # # # # # # # # # # # # # # # # # # # # # #
		
		#Isolate level list to only checkbox alters
		level_list_alters_checkbox <-
			level_list %>%
			subset(attr(level_list, "persnet_var_type") == "alter" &
						 	attr(level_list, "var_type") == "checkbox")
		
		if(length(level_list_alters_checkbox) > 1){
			
			attributes(level_list_alters_checkbox)$varname <-
				level_list_alters_checkbox %>%
				names() %>%
				sub("^name[0-9]+", "", .)
			
			#lapply Function which iterates through each group of checkbox alter data,
			# and constructs an alter_frame for it.
			lapply(unique(attr(level_list_alters_checkbox, "varname")), function(i){
				
				level_list_alters_checkbox_select <-
					level_list_alters_checkbox %>%
					subset(attr(level_list_alters_checkbox, "varname") == i)
				
				#Creaking list which contains values for each alter. We are making a list
				#  rather than a dataframe as the length of each vector will modulate based
				#  upon the count of selected inviduals.
				lapply(c(1:nrow(input_frame)), function(rownum){
					input_frame[rownum,
											c(input$survey_identifier,
												expand.grid(paste0("___", unlist(unique(level_list_alters_checkbox_select))),
																		names(level_list_alters_checkbox_select)) %>%
													apply(1, function(x){paste0(x[2], x[1])}))]
				}) -> workbench
				
				#Creates dataframe from raw checkboxes
				lapply(workbench, function(input_row){
					input_row <- input_row %>% unlist()
					wb_id <- input_row[input$survey_identifier]
					input_row <- input_row[names(input_row) != names(wb_id)]
					
					keep_remove_frame %>%
						"["(keep_remove_frame[[input$survey_identifier]] == wb_id,
								colnames(keep_remove_frame) != input$survey_identifier) %>%
						unlist() %>%
						as.logical() ->
						keep_remove_bench
					
					#Skips lapply iteration in individual has 0 network size
					if(all(!keep_remove_bench))return()
					
					input_df <- data.frame(id = sub(paste0(i, "___", "[0-9]+$"),"" , names(input_row)),
																 key = sub(paste0("^name[0-9]+",i, "___"),"" , names(input_row)),
																 value = input_row,
																 row.names = NULL, stringsAsFactors = FALSE)
					
					input_df$key <- input_df$key %>%
						factor(levels = unlist(unique(level_list_alters_checkbox_select)),
									 labels = trimws(names(unlist(unique(level_list_alters_checkbox_select)))))
					
					#An option on how to do this
					# input_df_spread <- spread(input_df, key, value)
					
					newcols <- paste0(i, 1:length(levels(input_df$key)))
					
					input_df %>%
						group_by(id) %>%
						lapply(1:length(levels(input_df$key)), slice, .data = .) %>%
						lapply(filter, value == 1) %>%
						lapply(select, -value) %>%
						append(list(input_df %>% select(id) %>% unique()), 0) %>%
						Reduce(function(...) left_join(..., by = c("id")), .) %>%
						apply(1, function(x){x[!is.na(x)] %>% "length<-"(length(x))}) %>%
						t() %>%
						as.data.frame(stringsAsFactors = FALSE) %>%
						"colnames<-"(c("alter_id", newcols)) ->
						thing
					
					thing[newcols] <- lapply(thing[newcols], factor, levels = levels(input_df$key))
					thing <- thing[keep_remove_bench,]
					thing <- data.frame(id = wb_id %>% "names<-"(NULL), thing)
					
					return(thing)
				}) %>%
					Reduce(function(...) rbind(...), .) -> output_frame
				
					return(output_frame)
				}) %>%
				Reduce(function(...) left_join(..., by = c("id", "alter_id")), .) ->
				alter_frame_checkbox
			
			#Forloop which checks each column of a checkbox group (but not the first)
			#  and removes it if its all NA's, this is to remove repetitive NA columns
			#Probably a cleaner way of doing this
			checkcols <- colnames(alter_frame_checkbox)
			for(i in 1:ncol(alter_frame_checkbox)){
				if(all(is.na(alter_frame_checkbox[,checkcols[i]])) &
					 !colnames(alter_frame_checkbox)[i] %in% c("id", "alter_id")){
					 	if(gsub(
					 		paste(
					 			paste0("^", unique(attr(level_list_alters_checkbox, "varname"))),
					 			collapse = "|"),
					 		"",
					 		checkcols[i]) %>%
					 		as.integer() > 1){
					 		alter_frame_checkbox[,checkcols[i]] <- NULL
				
					 	}
					}
					 
				# print(i)
			}
		
		}else{
			alter_frame_checkbox <- data.frame(id = as.integer(character()),
																				 alter_id = character(),
																				 stringsAsFactors = FALSE)
		}

		
		# Alter Text Processing # # # # # # # # # # # # # # # # # # # # # # # # # # 
		data_dict$Variable_Field_Name[
			data_dict$Field_Type == "text" &
				grepl("^name[0-9]+",data_dict$Variable_Field_Name) &
				!grepl("^name[0-9]+$",data_dict$Variable_Field_Name)
			] -> text_alters_list
		
		if(length(text_alters_list) > 1){
			names(text_alters_list) <- sub("^name[0-9]+", "", text_alters_list)
			
			lapply(unique(names(text_alters_list)), function(i){
				
				input_frame[c(input$survey_identifier, text_alters_list[names(text_alters_list) == i])] ->
					workbench
				
				lapply(1:nrow(workbench), function(x){workbench[x,]}) %>%
					lapply(function(input_row){
						input_row <- unlist(input_row)
						wb_id <- input_row[[input$survey_identifier]]
						input_row <- input_row[names(input_row) != input$survey_identifier]
						
						keep_remove_frame %>%
							"["(unlist(keep_remove_frame[input$survey_identifier]) == wb_id,) %>%
							"["(colnames(keep_remove_frame) != input$survey_identifier) %>%
							unlist() %>%
							as.logical() -> keep_remove_bench
						
						#Skips lapply iteration for individuals with networks of 0
						if(all(!keep_remove_bench))return()
						
						input_row <- input_row[keep_remove_bench]
						
						data.frame(id = wb_id,
											 alter_id = input_row %>% names() %>% sub(i,"",.) %>% trimws(),
											 value = input_row,
											 row.names = NULL, stringsAsFactors = FALSE) %>%
							"colnames<-"(c("id", "alter_id", i)) %>%
							return()
					}) %>%
					Reduce(function(...) rbind(...), .) %>% return()
			}) %>%
				Reduce(function(...) left_join(..., by = c("id", "alter_id")), .) ->
				alter_frame_textbox
			
		}else{
			alter_frame_textbox <- data.frame(id = as.integer(character()),
																				alter_id = character(),
																				stringsAsFactors = FALSE)
		}
		
		Reduce(function(...) left_join(..., by = c("id", "alter_id")),
					 list(alter_frame_singles, alter_frame_checkbox, alter_frame_textbox)) ->
			alter_frame
		
		## What I need to do next:
		# 1) left_join everything together
		# 2) Make this so that it can adapt to having 0, 1, or multiple vars for each category
		# 3) Make a requirement that there be at least 1 alter data for all this to run
		
		
		return(alter_frame)
	})
	
	# Control for type of display ----
	output$contents <- renderTable({
		req(input$disp %in% c("dataframe", "data_dictionary", "alter_frame",
													"leveled_dataframe", "network_structure", "all"))
		
		if(input$disp == "dataframe"){
			print("should be outputting dataframe")
			return(frame_make()[1:20])
		}else if(input$disp == "data_dictionary"){
			print("should be outputting data_dictionary")
			return(data_dict_make())
		}else if(input$disp == "alter_frame"){
			print("should be outputting alter_frame")
			return(alter_frame_maker())
		}else if(input$disp == "leveled_dataframe"){
			print("should be outputting leveled_dataframe")
			return(leveled_singles()[1:20])
		}else if(input$disp == "network_structure"){
			print("should be outputting network_structure")
			return(network_structure())
		}else{
			print("should be outputting all")
			return(frame_make()[1:12])
		}
	})
	
	# Control for type of download ----
	output$downloadContents <- downloadHandler(
		
		filename = function(){
			paste0(input$disp, ".csv")
		},
		content = function(file){
			if(input$disp == "dataframe"){
				print("should be outputting dataframe")
				write.csv(frame_make(), file, row.names = FALSE)
			}else if(input$disp == "data_dictionary"){
				print("should be outputting data_dictionary")
				write.csv(data_dict_make(), file, row.names = FALSE)
			}else if(input$disp == "alter_frame"){
				print("should be outputting alter_frame")
				write.csv(alter_frame_maker(), file, row.names = FALSE)
			}else if(input$disp == "leveled_dataframe"){
				print("should be outputting leveled_dataframe")
				write.csv(leveled_singles(), file, row.names = FALSE)
			}else if(input$disp == "network_structure"){
				print("should be outputting network_structure")
				write.csv(network_structure(), file, row.names = FALSE)
			}else{
				print("should be outputting all")
				write.csv(frame_make(), file, row.names = FALSE)
			}
		}
	)
	
	# Control for type of download load() ----
	output$downloadRDA <- downloadHandler(
		
		filename = function(){
			paste0(input$disp, ".rda")
		},
		content = function(file){
			if(input$disp == "dataframe"){
				print("should be outputting dataframe")
				output <- frame_make()
				# write.csv(frame_make(), file, row.names = FALSE)
			}else if(input$disp == "data_dictionary"){
				print("should be outputting data_dictionary")
				output <- data_dict_make()
				# write.csv(data_dict_make(), file, row.names = FALSE)
			}else if(input$disp == "alter_frame"){
				print("should be outputting alter_frame")
				output <- alter_frame_maker()
				# write.csv(alter_frame_maker(), file, row.names = FALSE)
			}else if(input$disp == "leveled_dataframe"){
				print("should be outputting leveled_dataframe")
				output <- leveled_singles()
				# write.csv(leveled_singles(), file, row.names = FALSE)
			}else if(input$disp == "network_structure"){
				print("should be outputting network_structure")
				output <- network_structure()
				# write.csv(network_structure(), file, row.names = FALSE)
			}else{
				print("should be outputting all")
				output <- frame_make()
				# write.csv(frame_make(), file, row.names = FALSE)
			}
			save(output, file = file)
		}
	)
	
	observe({
		print(input$disp)
		print(input$texttry)
	})
	
} 

# Run the application 
shinyApp(ui = ui, server = server)

