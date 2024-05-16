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
    card_header(h1(strong("Input files and main plot"), style="font-size:21px; text-align:left")),
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
        shinyWidgets::materialSwitch(inputId="x_numeric", label="x is numeric", value=FALSE, status="primary", right=TRUE),
        shinyWidgets::pickerInput(inputId="labels", label="Labels of each level or observation relating to the explanatory variable:", choices="", multiple=TRUE, options=list(`live-search`=TRUE, `actions-box`=TRUE))
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
      dat = data.frame(
        weight=(seq(0, 100, length=100)^2) * abs(rnorm(100)),
        time=seq(0, 100, length=100),
        class=rep(paste0("class", 1:5), each=20),
        height=rnorm(100),
        width=rnorm(100),
        level=sample(letters, size=100, replace=TRUE)
      )
    } else {
      ### Load the user-input data
      dat = read.csv(input$input$datapath)
    }
    ### Update input based on input data
    vec_colnames = colnames(dat)
    shinyWidgets::updatePickerInput(session, "y", choices=vec_colnames, selected=vec_colnames[1])
    shinyWidgets::updatePickerInput(session, "x", choices=vec_colnames, selected=vec_colnames[2])
    shinyWidgets::updatePickerInput(session, "labels", choices=vec_colnames, selected=vec_colnames[3])
    return(dat)
  })
  #######################################################################
  ### Scatter or violin plot
  #######################################################################
  output$plot_1 = renderPlotly({
    dat = data()
    group = eval(parse(text=paste0("paste0(", paste(paste0("dat$`", input$labels, "`"), collapse=", '-x-', "), ")")))
    df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group, ",
      paste(paste0("dat$`", input$labels, "`"), collapse=","), ")")))
    if (is.numeric(df$x)) {
      shinyWidgets::updateMaterialSwitch(session, "x_numeric", value=TRUE)
    }
    if (!input$x_numeric) {
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
      if (ncol(df) > 3) {
        lab = df[, 4]
        if (ncol(df)>4) {
          for (j in 5:ncol(df))
            lab = paste0(lab, "-x-", df[, j])
        } else {
          lab = lab
        }
      } else {
        lab = df$group
      }
      p = plotly::plot_ly(data=df,
        y=~y,
        x=~x,
        color=~group,
        type='scatter',
        hovertemplate=paste(
          '<i>Group1</i>: %{text}',
          '<br><b>x</b>: %{x}',
          '<br><b>y</b>: %{y}'
        ),
        text=lab
      )
      fit = lm(y ~ poly(x, 5), data=droplevels(df[complete.cases(df), ]))
      p = p %>% plotly::add_trace(y=~fitted(fit), x=~x, mode="line", color=NULL)
      p = p %>% plotly::layout(
        title="xaxis_title",
        yaxis=list(title=input$y),
        xaxis=list(title=input$x),
        showlegend=FALSE
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
    group = eval(parse(text=paste0("paste0(", paste(paste0("dat$`", input$labels, "`"), collapse=", '-x-', "), ")")))
    # df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group)")))
    df = eval(parse(text=paste0("data.frame(y=dat$`", input$y, "`, x=dat$`", input$x, "`, group=group, ",
      paste(paste0("dat$`", input$labels, "`"), collapse=","), ")")))
    print(df)

    if (ncol(df) > 3) {
      lab = df[, 4]
      if (ncol(df)>4) {
        for (j in 5:ncol(df))
          lab = paste0(lab, "-x-", df[, j])
      } else {
        lab = lab
      }
    } else {
      lab = df$group
    }
    print(lab)

  })
}
####################################################################################################
### Serve the app
####################################################################################################
shinyApp(ui = ui, server = server)