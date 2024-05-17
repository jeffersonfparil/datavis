library(shiny)
library(shinyWidgets)
library(plotly)
library(bslib)

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
        # shinyWidgets::materialSwitch(inputId="x_numeric", label="x is numeric (will not switch if the x variable is clearly not numeric)", value=TRUE, status="primary", right=TRUE),
        shinyWidgets::pickerInput(inputId="labels", label="Labels of each level or observation relating to the explanatory variable:", choices="", multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE)),

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
      time = round(seq(0, 1, length=100), 2)
      dat = data.frame(
        id=rep(letters[1:10], times=10),
        class=rep(paste0("class", 1:5), each=20),
        level=sample(letters, size=100, replace=TRUE),
        time=time,
        weight=1/(1+exp(-20*(time - 0.5)+rnorm(100))),
        height=1/(1+exp(-10*(time - 0.5)+rnorm(100))),
        length=1/(1+exp(-5*(time - 0.5)+rnorm(100))),
        width=1/(1+exp(-2*(time - 0.5)+rnorm(100)))
      )
    } else {
      ### Load the user-input data
      dat = read.csv(input$input$datapath)
    }
    ### Update input based on input data
    vec_colnames = colnames(dat)
    vec_numerics = unlist(lapply(c(1:ncol(dat)), FUN=function(j){
      if (is.numeric(dat[, j])) {
        return (vec_colnames[j])
      }
    }))
    shinyWidgets::updatePickerInput(session, "x", choices=vec_colnames, selected=vec_colnames[7])
    shinyWidgets::updatePickerInput(session, "labels", choices=vec_colnames, selected=vec_colnames[2])
    shinyWidgets::updatePickerInput(session, "y", choices=vec_numerics, selected=vec_numerics[1])
    shinyWidgets::updatePickerInput(session, "y_additional", choices=vec_numerics, selected=c())
    return(dat)
  })
  #######################################################################
  ### Scatter or violin plot
  #######################################################################
  output$plot_1 = renderPlotly({
    dat = data()
    # if (input$x_numeric & !is.numeric(eval(parse(text=paste0("dat$", input$x))))) {
    #   shinyWidgets::updateMaterialSwitch(session, "x_numeric", value=FALSE)
    # }
    group = eval(parse(text=paste0("paste0(", paste(paste0("dat$`", input$labels, "`"), collapse=", '-x-', "), ")")))
    df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group, ",
      paste(paste0("dat$`", input$labels, "`"), collapse=","), ")")))
    if (length(input$y_additional) > 0) {
        for (i in 1:length(input$y_additional)) {
          eval(parse(text=paste0("df$y_additional_", i, " = dat$", input$y_additional[i])))
        }
    }
    idx = which(rowSums(is.na(df)) == 0)
    df = droplevels(df[idx, ])
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
    # if (!input$x_numeric) {
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
        title="xaxis_title",
        yaxis=list(title=input$y),
        xaxis=list(title=input$x)
      )
    } else {
      ##############################################
      ### Scatter plot for numeric x variable    ###
      ##############################################
      if (vec_logit_or_polyd[1]==0) {
        fit = glm(y ~ x, family=binomial(link='logit'), data=df)
      } else {
        fit = lm(y ~ poly(x, vec_logit_or_polyd[1]), data=df)
      }
      df_fit = data.frame(summary(fit)$coef)
      df_fit$Pr[df_fit$Pr >= 0.0001] = round(df_fit$Pr[df_fit$Pr >= 0.0001], 4)
      df_fit$Pr[df_fit$Pr < 0.0001] = "<0.0001"
      p = plotly::plot_ly(data=df,
        y=~y,
        x=~x,
        color=~group,
        type='scatter',
        showlegend=TRUE
      )
      p = p %>% plotly::add_trace(y=~fitted(fit)[order(x)], x=~x[order(x)], mode="line", color=NULL,
        hoverinfo='text',
        text=~paste(
          "y = ", input$y,
          "<br>Intercept:", round(df_fit$Estimate[1], 4), " (p=", df_fit$Pr[1], ")", 
          "<br>Linear coefficient:", round(df_fit$Estimate[2], 4), " (p=", df_fit$Pr[2], ")", 
          "<br>Quadratic coefficient:", round(df_fit$Estimate[3], 4), " (p=", df_fit$Pr[3], ")", 
          "<br>Cubic coefficient:", round(df_fit$Estimate[4], 4), " (p=", df_fit$Pr[4], ")", 
          "<br>R-squared:", round(100*summary(fit)$r.squared), "%",
          "<br>Deviance:", round(1*summary(fit)$deviance),
          "<br>Correlation:", round(100*cor(df$x, df$y)), "%",
          "<br>n = ", nrow(df)
        ),
        showlegend=FALSE
      )
      if (length(input$y_additional) > 0) {
        for (i in 1:length(input$y_additional)) {
          if (vec_logit_or_polyd[i+1]==0) {
            eval(parse(tex=paste0("fit_additional_", i, " = glm(y_additional_", i, " ~ x, family=binomial(link='logit'), data=df)")))
          } else {
            eval(parse(tex=paste0("fit_additional_", i, " = lm(y_additional_", i, " ~ poly(x, vec_logit_or_polyd[", i+1, "]), data=df)")))
          }
          eval(parse(text=paste0("df_fit_additional_", i, " = data.frame(summary(fit_additional_", i, ")$coef)")))
          eval(parse(text=paste0("df_fit_additional_", i, "$Pr[df_fit_additional_", i, "$Pr >= 0.0001] = round(df_fit_additional_", i, "$Pr[df_fit_additional_", i, "$Pr >= 0.0001], 4)")))
          eval(parse(text=paste0("df_fit_additional_", i, "$Pr[df_fit_additional_", i, "$Pr < 0.0001] = '<0.0001'")))
          eval(parse(text=paste0("p = p %>% plotly::add_trace(y=~fitted(fit_additional_", i, ")[order(x)], x=~x[order(x)], mode='line', color=NULL,
            name=input$y_additional[", i, "],
            hoverinfo='text',
            text=~paste(
              'y_additional_', ", i, ", ' = ', input$y_additional[", i, "],
              '<br>Intercept:', round(df_fit_additional_", i, "$Estimate[1], 4), ' (p=', df_fit_additional_", i, "$Pr[1], ')', 
              '<br>Linear coefficient:', round(df_fit_additional_", i, "$Estimate[2], 4), ' (p=', df_fit_additional_", i, "$Pr[2], ')', 
              '<br>Quadratic coefficient:', round(df_fit_additional_", i, "$Estimate[3], 4), ' (p=', df_fit_additional_", i, "$Pr[3], ')', 
              '<br>Cubic coefficient:', round(df_fit_additional_", i, "$Estimate[4], 4), ' (p=', df_fit_additional_", i, "$Pr[4], ')', 
              '<br>R-squared:', round(100*summary(fit_additional_", i, ")$r.squared), '%',
              '<br>Deviance:', round(1*summary(fit_additional_", i, ")$deviance)
            ),
            showlegend=TRUE
          )")))
        }
      }
      p = p %>% plotly::layout(
        title="xaxis_title",
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
    dat = data()
    print(str(dat))
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