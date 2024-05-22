library(shiny)
library(shinyWidgets)
library(plotly)
library(bslib)

####################################################################################################
### Helper functions
####################################################################################################
fn_generalised_logistic = function(par, x) {
	# par = c(1, 1, 1, 1, 1, 1)
	# x = rnorm(10)
	A = par[1] ### horizontal left asymptote
	K = par[2] ### horizontal right asymptote
	C = par[3] ### upper asymptote-related
	Q = par[4] ### y-intercept-related
	B = par[5] ### growth rate
	v = par[6] ### related to the asymptote identity at which maximum growth rate occurs
	y = A + ( 
			(.Machine$double.eps + K-A) /
			(.Machine$double.eps + C + Q*exp(-B*x))^(1/v)
	)
	return(y)
}

fn_cost_logistic = function(par, y, x) {
	# par = c(1, 2, 3, 4, 5, 6)
	# x = rnorm(10)
	# y = fn_generalised_logistic(par=par, x=x) + rnorm(length(x))
	y_hat = fn_generalised_logistic(par=par, x=x)
	error = sum((y - y_hat)^2)
	return(error)
}

####################################################################################################
### Front-end
####################################################################################################
ui <- page_fillable(
	titlePanel("datavis"),
	card(
		card_header(h1(strong("Input files and 2D plot"), style="font-size:21px; text-align:left")),
		min_height="840px",
		layout_sidebar(
			sidebar=sidebar(
				 width=500,
				fileInput(
					inputId="input", 
					label="Load comma-separated input file",
					multiple=TRUE,
					accept=".csv"
				),
				shinyWidgets::pickerInput(inputId="y", label="y (response variable):", choices="", multiple=FALSE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),
				shinyWidgets::pickerInput(inputId="x", label="x (explanatory variable):", choices="", multiple=FALSE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),
				shinyWidgets::pickerInput(inputId="labels", label="Labels of each level or observation relating to the explanatory variable:", choices="", multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),

				shinyWidgets::materialSwitch(inputId="show_eigenvec", label="If the x and y variables are PC1 and PC2, then do you wish to plot the eigenvectors corresponding to the rotated variables?", value=TRUE, status="primary", right=TRUE),

				shinyWidgets::materialSwitch(inputId="regress", label="Fit a logistic, linear, quadratic or cubic regression on the main and additional variables?", value=FALSE, status="primary", right=TRUE),
				shinyWidgets::pickerInput(inputId="y_additional", label="additional y axis regression line (only applicable if x is numeric):", choices="", multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),
				textInput("logit_or_polyDegree", "Fit logistic or polynomial regression? Enter logistic, 1, 2, or 3 separated by spaces each of which correspond to the how the model treat the x variable.", value="3")
			),
			mainPanel(
				width=750,
				plotlyOutput(outputId="plot_1")
			)
		)
	),
	card(
		card_header(h1(strong("Debugging"), style="font-size:21px; text-align:left")),
		min_height="500px",
		layout_sidebar(
			mainPanel(
				verbatimTextOutput(outputId="debug")
			)
		)
	)
)
####################################################################################################
### Back-end
####################################################################################################
server <- function(input, output, session) {
	#######################################################################
	### Set maximum input file size to 42Mb
	#######################################################################
	options(shiny.maxRequestSize=42*1024^2) 
	#######################################################################
	### Load input Rds file/s
	#######################################################################
	data = shiny::reactive({
		if (is.null(input$input$datapath)) {
			### Dummy dataset to show at initialisation
			time = round(seq(0, 100, length=100), 2)
			dat = data.frame(
				id=rep(letters[1:10], times=10),
				class=rep(paste0("class", 1:5), each=20),
				level=sample(letters, size=100, replace=TRUE),
				time=time,
				weight=1/(1+exp(-0.1*(time - 50)+rnorm(100))),
				height=1/(1+exp(-0.1*(time - 25)+rnorm(100))),
				length=1/(1+exp(-0.1*(time - 75)+rnorm(100))),
				width=1/(1+exp(-0.1*(time - 5)+rnorm(100))),
				leaf_drop=1/(1+exp(+0.1*(time - 50)+rnorm(100)))
			)
		} else {
			### Load the user-input data
			dat = read.csv(input$input$datapath)
		}
		### Extract the numeric and non-numeric input data
		### where we convert the levels of the non-numeric fields into matrices and normalise everything to extract the first 2 PCs
		vec_colnames = colnames(dat)
		vec_numerics = c()
		vec_classes = c()
		mat_for_pc = NULL
		for (j in c(1:ncol(dat))) {
			if (sum(!is.na(dat[, j]))==0) {
				next
			}
			if (is.numeric(dat[, j])) {
				vec_numerics = c(vec_numerics, vec_colnames[j])
				mat_normalised = matrix(scale(dat[, j], scale=TRUE, center=TRUE), ncol=1)
				colnames(mat_normalised) = vec_colnames[j]
			} else {
				vec_classes = c(vec_classes, vec_colnames[j])
				mat_normalised = scale(model.matrix(~0+dat[, j]), scale=TRUE, center=TRUE)
				colnames(mat_normalised) = gsub("^dat\\[, j\\]", paste0(vec_colnames[j], "_"), colnames(mat_normalised))
			}
			if (is.null(mat_for_pc)) {
				mat_for_pc = mat_normalised
			} else {
				mat_for_pc = cbind(mat_for_pc, mat_normalised)
			}
		}
		### Mean value imputation of missing data and removal of fixed columns
		for (j in 1:ncol(mat_for_pc)) {
			# j = 330
			idx = which(is.na(mat_for_pc[, j]))
			if (length(idx) > 0) {
				print(j)
				mat_for_pc[idx, j] = mean(mat_for_pc[, j], na.rm=TRUE)
			}
		}
		### Compute and add the first 2 PCs (via singular value decomposition)
		PCs = prcomp(mat_for_pc)
		dat$PC1 = PCs$x[,1]
		dat$PC2 = PCs$x[,2]
		vec_colnames = c(vec_colnames, "PC1", "PC2")
		vec_numerics = c(vec_numerics, "PC1", "PC2")
		### Update the input selection based on the input data
		shinyWidgets::updatePickerInput(session, "x", choices=sort(vec_colnames), selected=vec_numerics[1])
		shinyWidgets::updatePickerInput(session, "labels", choices=sort(vec_classes), selected=vec_classes[1])
		shinyWidgets::updatePickerInput(session, "y", choices=sort(vec_numerics), selected=vec_numerics[2])
		shinyWidgets::updatePickerInput(session, "y_additional", choices=sort(vec_numerics), selected=c())
		return(list(dat=dat, PCs=PCs))
	})
	#######################################################################
	### Scatter or violin plot
	#######################################################################
	output$plot_1 = renderPlotly({
		### input = list(x="time", y="weight", labels="id", y_additional="height", logit_or_polyDegree="logistic")
		### input = list(x="Time_elapsed", y="Rainfall_mm", labels="Treatment", y_additional="Sigma_average_deg", logit_or_polyDegree="3")
		### Load the data
		list_dat_PCs = data()
		dat = data()$dat
		PCs = data()$PCs
		### Create the input data frame for the plotting and model fitting
		group = eval(parse(text=paste0("paste0(", paste(paste0("dat$`", input$labels, "`"), collapse=", '-x-', "), ")")))
		df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group, ",
			paste(paste0(input$labels, "=dat$`", input$labels, "`"), collapse=","), ")")))
		if (length(input$y_additional) > 0) {
			for (i in 1:length(input$y_additional)) {
				eval(parse(text=paste0("df$y_additional_", i, " = dat$", input$y_additional[i])))
			}
		}
		### Define the models
		vec_logit_or_polyd_tmp = unlist(strsplit(input$logit_or_polyDegree, " "))
		vec_logit_or_polyd = c()
		for (x in vec_logit_or_polyd_tmp) {
			if (x == "logistic") {
				vec_logit_or_polyd = c(vec_logit_or_polyd, 0)
			} else if (x == "1") {
				vec_logit_or_polyd = c(vec_logit_or_polyd, 1)
			} else if (x == "2") {
				vec_logit_or_polyd = c(vec_logit_or_polyd, 2)
			} else if (x == "3") {
				vec_logit_or_polyd = c(vec_logit_or_polyd, 3)
			} else {
				### Unrecognised input gets interpreted as linear
				vec_logit_or_polyd = c(vec_logit_or_polyd, 1)
			}
		}
		len_m = length(vec_logit_or_polyd)
		len_y = length(input$y_additional) + 1
		if (len_m < len_y) {
			vec_logit_or_polyd = rep(vec_logit_or_polyd, times=(1 + ceiling((len_y - len_m) / len_m)))
		}
		### Plot
		if (!is.numeric(df$x)) {
			##############################################
			### Violin plot for categorical x variable ###
			##############################################
			p = plot_ly(data=df,
				y=~y,
				x=~x,
				type='violin',
				box=list(visible=TRUE),
				meanline = list(visible=TRUE),
				split=~x
			)
			p = p %>% layout(
				title=paste0(input$x, " vs ", input$y),
				yaxis=list(title=input$y),
				xaxis=list(title=input$x)
			)
		} else {
			##############################################
			### Scatter plot for numeric x variable    ###
			##############################################
			idx = which(!is.na(df$x) & !is.na(df$y))
			df = df[idx, ]
			df = df[order(df$x, decreasing=FALSE), ]
			### Add hover text for each data point including all the information we have
			vec_colnames = colnames(dat)
			vec_labels = c()
			for (i in 1:nrow(dat)) {
				label = paste0(vec_colnames[1], "=", dat[i, 1])
				for (j in 2:ncol(dat)) {
					label = paste0(label, "<br>", paste0(vec_colnames[j], "=", dat[i, j]))
				}
				vec_labels = c(vec_labels, label)
			}
			vec_labels = vec_labels[idx]
			p = plotly::plot_ly(data=df,
				y=~y,
				x=~x,
				color=~group,
				type='scatter',
				text = vec_labels,
				hoverinfo = 'text',
				showlegend=TRUE
			)
			if (input$regress) {
				n_decimal = 4
				list_fit = list()
				if (vec_logit_or_polyd[1]==0) {
					fit = optim(
						par=rep(1, times=6),
						method="Nelder-Mead",
						control=list(maxit=1e4, pgtol=0, ndeps=rep(1e-6, 11), factr=0),
						fn=fn_cost_logistic, y=df$y, x=df$x)
					df$y_hat = fn_generalised_logistic(par=fit$par, x=df$x)
					list_fit$A = fit$par[1] ### horizontal left asymptote
					list_fit$K = fit$par[2] ### horizontal right asymptote
					list_fit$C = fit$par[3] ### upper asymptote-related
					list_fit$Q = fit$par[4] ### y-intercept-related
					list_fit$B = fit$par[5] ### growth rate
					list_fit$v = fit$par[6] ### related to the asymptote identity at which maximum growth rate occurs
					list_fit$sse = sum((df$y - df$y_hat)^2, na.rm=TRUE)
					list_fit$sst = sum((df$y - mean(df$y, na.rm=TRUE))^2, na.rm=TRUE)
					list_fit$r2 = 1 - (list_fit$sse/list_fit$sst)
					list_fit$mae = mean(abs(df$y - df$y_hat), na.rm=TRUE)
					p = p %>% plotly::add_trace(data=df, y=~y_hat, x=~x, mode="line", color=NULL,
						hoverinfo='text',
						text=paste0(
							"y = ", input$y,
							"<br>n = ", nrow(df),
							"<br>Minimum(y):", round(min(c(list_fit$A, list_fit$K)), n_decimal), 
							"<br>Maximum(y):", round(max(c(list_fit$A, list_fit$K)), n_decimal), 
							"<br>Rate:", round(list_fit$B, n_decimal), 
							"<br>R-squared:", round(100*list_fit$r2), "%",
							"<br>Mean absolute error:", round(list_fit$mae, n_decimal)
						),
						showlegend=FALSE
					)
				} else {
					fit = lm(y ~ poly(x, vec_logit_or_polyd[1]), data=df)
					df$y_hat = fitted(fit)
					list_fit$intercept = coef(fit)["(Intercept)"]; names(list_fit$intercept) = NULL
					for (i in 1:3) {
						eval(parse(text=paste0("list_fit$poly_", i, " = tryCatch(coef(fit)[i+1], error=function(e){NA})")))
						eval(parse(text=paste0("names(list_fit$poly_", i, ") = NULL")))
					}
					list_fit$r2 = summary(fit)$r.squared
					list_fit$mae = mean(abs(df$y - df$y_hat), na.rm=TRUE)
					p = p %>% plotly::add_trace(data=df, y=~y_hat, x=~x, mode="line", color=NULL,
						hoverinfo='text',
						text=paste0(
							"y = ", input$y,
							"<br>n = ", nrow(df),
							"<br>Intercept = ", round(list_fit$intercept, n_decimal),
							"<br>Linear coefficient:", round(list_fit$poly_1, n_decimal), 
							"<br>Quadratic coefficient:", round(list_fit$poly_2, n_decimal), 
							"<br>Cubic coefficient:", round(list_fit$poly_3, n_decimal), 
							"<br>R-squared:", round(100*list_fit$r2), "%",
							"<br>Mean absolute error:", round(list_fit$mae, n_decimal)
						),
						showlegend=FALSE
					)
				}
				### Additional regression lines
				if (length(input$y_additional) > 0) {
					y_min = min(df$y, na.rm=TRUE)
					y_max = max(df$y, na.rm=TRUE)
					for (i in 1:length(input$y_additional)) {
						y_additional = eval(parse(text=paste0("df$y_additional_", i)))
						y_additional_min = min(y_additional, na.rm=TRUE)
						y_additional_max = max(y_additional, na.rm=TRUE)
						y_additional = (y_additional - y_additional_min) / (y_additional_max - y_additional_min)
						y_additional = (y_additional*(y_max - y_min)) + y_min
						df_additional = data.frame(x=df$x, y=y_additional)
						if (vec_logit_or_polyd[i+1]==0) {
							assign(paste0("fit_additional_", i), 
								optim(par=rep(1, times=6),
									method="Nelder-Mead",
									control=list(maxit=1e4, pgtol=0, ndeps=rep(1e-6, 11), factr=0),
									fn=fn_cost_logistic, y=df_additional$y, x=df_additional$x))

							fit = eval(parse(text=paste0("fit_additional_", i)))
							df$y_hat = fn_generalised_logistic(par=fit$par, x=df$x)
							list_fit$A = fit$par[1] ### horizontal left asymptote
							list_fit$K = fit$par[2] ### horizontal right asymptote
							list_fit$C = fit$par[3] ### upper asymptote-related
							list_fit$Q = fit$par[4] ### y-intercept-related
							list_fit$B = fit$par[5] ### growth rate
							list_fit$v = fit$par[6] ### related to the asymptote identity at which maximum growth rate occurs
							list_fit$sse = sum((df$y - df$y_hat)^2, na.rm=TRUE)
							list_fit$sst = sum((df$y - mean(df$y, na.rm=TRUE))^2, na.rm=TRUE)
							list_fit$r2 = 1 - (list_fit$sse/list_fit$sst)
							list_fit$mae = mean(abs(df$y - df$y_hat), na.rm=TRUE)
							p = p %>% plotly::add_trace(y=fn_generalised_logistic(par=fit$par, x=df$x), x=~x, mode="line", color=NULL,
								hoverinfo='text',
								text=paste0(
									"y = ", input$y_additional[i],
									"<br>n = ", nrow(df),
									"<br>Minimum(y):", round(min(c(list_fit$A, list_fit$K)), n_decimal), 
									"<br>Maximum(y):", round(max(c(list_fit$A, list_fit$K)), n_decimal), 
									"<br>Rate:", round(list_fit$B, n_decimal), 
									"<br>R-squared:", round(100*list_fit$r2), "%",
									"<br>Mean absolute error:", round(list_fit$mae, n_decimal)
								),
								name=input$y_additional[i],
								showlegend=TRUE
							)
						} else {
							assign(paste0("fit_additional_", i), lm(y ~ poly(x, vec_logit_or_polyd[i+1]), data=df_additional))
							fit = eval(parse(text=paste0("fit_additional_", i)))
							df$y_hat = fitted(fit)
							list_fit$intercept = coef(fit)["(Intercept)"]; names(list_fit$intercept) = NULL
							for (j in 1:3) {
								eval(parse(text=paste0("list_fit$poly_", j, " = tryCatch(coef(fit)[j+1], error=function(e){NA})")))
								eval(parse(text=paste0("names(list_fit$poly_", j, ") = NULL")))
							}
							list_fit$r2 = summary(fit)$r.squared
							list_fit$mae = mean(abs(df$y - df$y_hat), na.rm=TRUE)
							p = p %>% plotly::add_trace(y=fitted(eval(parse(text=paste0("fit_additional_", i)))), x=~x, mode="line", color=NULL,
								hoverinfo='text',
								text=paste0(
									"y = ", input$y_additional[i],
									"<br>n = ", nrow(df),
									"<br>Intercept = ", round(list_fit$intercept, n_decimal),
									"<br>Linear coefficient:", round(list_fit$poly_1, n_decimal), 
									"<br>Quadratic coefficient:", round(list_fit$poly_2, n_decimal), 
									"<br>Cubic coefficient:", round(list_fit$poly_3, n_decimal), 
									"<br>R-squared:", round(100*list_fit$r2), "%",
									"<br>Mean absolute error:", round(list_fit$mae, n_decimal)
								),
								name=input$y_additional[i],
								showlegend=TRUE
							)
						}
						
					}
				}
			}
			### Plot eigenvectors for the first 2 PCs each corresponding to the rotation of the variables
			if ((((input$x=="PC1") & (input$y=="PC2")) | ((input$x=="PC2") & (input$y=="PC1"))) & input$show_eigenvec) {
				df_arrows = data.frame(ids=rownames(PCs$rotation), x=PCs$rotation[, 1], y=PCs$rotation[, 2])
				if ((input$x=="PC2") & (input$y=="PC1")) {
					df_arrows = data.frame(ids=df_arrows$id, x=df_arrows$y, y=df_arrows$x)
				}
				df_arrows$origin = 0.0
				### Adjust lengths
				df_arrows$x = df_arrows$x * (abs(diff(range(df$x))) / abs(diff(range(df_arrows$x))))
				df_arrows$y = df_arrows$y * (abs(diff(range(df$y))) / abs(diff(range(df_arrows$y))))
				## Plot
				p = p %>% add_annotations(
					x=df_arrows$origin,
					ax=df_arrows$x,
					y=df_arrows$origin,
					ay=df_arrows$y,
					axref="x", ayref="y",
					xref="x", yref="y",
					text=df_arrows$ids,
					arrowcolor="grey",
					arrowsize=2,
					arrowhead=4,
					arrowside="start",
					showarrow=TRUE,
					showlegend=TRUE)
			}
			p = p %>% plotly::layout(
				title=paste0(input$x, " vs ", input$y, "\n(additional ys: ", input$y_additional, ")"),
				yaxis=list(title=input$y),
				xaxis=list(title=input$x)
			)
		}
		p = p %>% plotly::config(toImageButtonOptions = list(format = "svg"))
	})
	#######################################################################
	### Debugging messages
	#######################################################################
	# output$debug= shiny::renderPrint({ str(data()) })
	output$debug= shiny::renderPrint({
		list_dat_PCs = data()
		dat = data()$dat
		PCs = data()$PCs
		### Create the input data frame for the plotting and model fitting
		group = eval(parse(text=paste0("paste0(", paste(paste0("dat$`", input$labels, "`"), collapse=", '-x-', "), ")")))
		df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group, ",
			paste(paste0(input$labels, "=dat$`", input$labels, "`"), collapse=","), ")")))
		if (length(input$y_additional) > 0) {
			for (i in 1:length(input$y_additional)) {
				eval(parse(text=paste0("df$y_additional_", i, " = dat$", input$y_additional[i])))
			}
		}
		df = df[!is.na(df$x), ]
		df = df[order(df$x, decreasing=FALSE), ]
		### Add hover text for each data point including all the information we have
		vec_colnames = colnames(dat)
		vec_labels = c()
		for (i in 1:nrow(dat)) {
			label = paste0(vec_colnames[1], "=", dat[i, 1])
			for (j in 2:ncol(dat)) {
				label = paste0(label, "<br>", paste0(vec_colnames[j], "=", dat[i, j]))
			}
			vec_labels = c(vec_labels, label)
		}
		str(dat)
		str(df)
		print(vec_labels)
		# for (i in 1:length(input$y_additional)) {
		#   df_additional = eval(parse(text=paste0("data.frame(y_additional=dat$`", input$y_additional[i], "`, x=dat$`", input$x, "`)")))
		#   fit_additional = lm(y_additional ~ poly(x, 3), data=df_additional)
		#   df_fit_additional = data.frame(summary(fit_additional)$coef)
		#   df_fit_additional$Pr[df_fit_additional$Pr >= 0.0001] = round(df_fit_additional$Pr[df_fit_additional$Pr >= 0.0001], 4)
		#   df_fit_additional$Pr[df_fit_additional$Pr < 0.0001] = "<0.0001"
		#   print(df_additional)
		# }
	#   vec_logit_or_polyd_tmp = unlist(strsplit(input$logit_or_polyDegree, " "))
	#   vec_logit_or_polyd = c()
	#   for (x in vec_logit_or_polyd_tmp) {
	#     if (x == "logistic") {
	#       vec_logit_or_polyd = c(vec_logit_or_polyd, 0)
	#     } else if (x == "1") {
	#       vec_logit_or_polyd = c(vec_logit_or_polyd, 1)
	#     } else if (x == "2") {
	#       vec_logit_or_polyd = c(vec_logit_or_polyd, 2)
	#     } else if (x == "3") {
	#       vec_logit_or_polyd = c(vec_logit_or_polyd, 3)
	#     } else {
	#       ### Unrecognised input gets interpreted as linear
	#       vec_logit_or_polyd = c(vec_logit_or_polyd, 1)
	#     }
	#   }
	#   len_m = length(vec_logit_or_polyd)
	#   len_y = length(input$y_additional) + 1
	#   if (len_m < len_y) {
	#     vec_logit_or_polyd = rep(vec_logit_or_polyd, times=(1+ceiling((len_y - len_m) / len_m)))
	#   }
	#   print(len_m)
	#   print(len_y)
	#   print(vec_logit_or_polyd_tmp)
	#   print(vec_logit_or_polyd)
	})
}
####################################################################################################
### Serve the app
####################################################################################################
shinyApp(ui = ui, server = server)