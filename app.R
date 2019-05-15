library(shiny)
library(RColorBrewer)
library(readxl)
library(ggplot2)
library(svglite)

# Define User Interface for the application that draws the barplot
ui <- fluidPage(
  theme = "style.css",
  tags$div(class="banner" , tags$img(src='banner.jpg')),
  h1("Barplot (application to the mRS)"),
  tags$div(class="footnote", tags$p("Author: Quentin Pilard")),
  tags$div(class="logo", checked = NA,
           tags$img(src='logo.jpg', width=80, height=80)),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
   tags$p("Application to create a barplot applied to the mRS (modified Rankin Scale), severity score ranged from 0 (no symptom) to 6 (dead). The following parameters must be provided:"),
   tags$ul(
     tags$li("A modified Rankin scale variable named 'mrs' coded from 0 to 6"),
     tags$li("An intervention dummy variable named 'trt', binary coded (0/1)")
   ),
   tags$br(),
   
   # Copy the line below to make a file upload manager
   fileInput("dataset", label = "File input (.xlsx format)", accept ='.xlsx'),
   
   # Copy the line below to add a title to the graph
   textInput("title", label = "Barplot title (sample size is added to the title)", value = "mRS (modified Rankin Scale) distribution according to the two treatment arms"),
   
   # Copy the line below to add a title to the graph
   textInput("intervention", label = "Intervention 1 label", value = "Control"),
   
   # Copy the line below to add a title to the graph
   textInput("control", label = "Intervention 2 label", value = "Intervention"),
   
   # Copy the line below to make a slider bar 
   sliderInput("barwidth", label = "Bar width", min = 0, 
               max = 1, value = .8, step = .1),
   
   # Copy the line below to make a set of radio buttons
   radioButtons("dashedline", label = "Dashed line",
                choices = list("Yes" = 1, "No" = 2), 
                selected = 1),
   
   actionButton("action", label = "Submit")
   
   ),

  # Main panel for displaying outputs ----
  mainPanel(
    
    # Output: Barplot ----
    conditionalPanel(condition = 'input.action',
                      plotOutput(outputId = "plot1"),
                      tags$div(class="Exportation_param",
                        tags$p(h4("Exportation parameters")),
                        tags$p("Please note that the plot size displayed above depends on the screen resolution, i.e. exportation parameters  affect only the downloaded file."),
                        sliderInput("width", label = "Width", min = 0, max = 20, value = 16),
                        sliderInput("height", label = "Height", min = 0, max = 20, value = 6),
                        downloadButton('downloadPlot_png', 'Export as png'),
                        downloadButton('downloadPlot_tiff', 'Export as tiff'),
                        downloadButton('downloadPlot_eps', 'Export as eps'),
                        downloadButton('downloadPlot_svg', 'Export as svg')
                        )
                    )
    )
  )
)


# Define server logic required to draw a barplot
server <- function(input, output) {
  
      #Color
      col<-rev(brewer.pal(7, "Reds"))
      
      #Function to create the barplot
      grotta_bar<-function(dataset, title, label_0, label_1, barwidth, dashedline){
        
        table<-table(dataset$mrs,dataset$trt)
        table2<-data.frame(prop.table(table,2))
        names(table2)<-c("mRS","Intervention","Percent")
        table2$mRS <- factor(table2$mRS,
                             levels=levels(table2$mRS)[order(levels(table2$mRS), decreasing = TRUE)])
        
        table2$Intervention<-as.factor(table2$Intervention)
        
        #Calculation of cumulative percentages to draw dashed lines
        if(dashedline == 1){
          cum_sum_0<-cumsum(table2[table2$Intervention=="0","Percent"])
          cum_sum_1<-cumsum(table2[table2$Intervention=="1","Percent"])
        }
        
        levels(table2$Intervention)[levels(table2$Intervention)=="0"] <- label_0
        levels(table2$Intervention)[levels(table2$Intervention)=="1"] <- label_1
        
        #With dashed lines
        if(dashedline == 1){
          graph<-ggplot(table2, aes(y=Percent, x=Intervention, fill=mRS,label=round(Percent*100))) +
            geom_bar(stat="identity", position="fill", colour="black", width = barwidth)+
            geom_text(aes(label = paste(round(Percent*100),"%",sep="")), position = position_stack(vjust = 0.5))+
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[1], xend=2-(barwidth/2), yend=cum_sum_1[1]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[2], xend=2-(barwidth/2), yend=cum_sum_1[2]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[3], xend=2-(barwidth/2), yend=cum_sum_1[3]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[4], xend=2-(barwidth/2), yend=cum_sum_1[4]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[5], xend=2-(barwidth/2), yend=cum_sum_1[5]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            geom_segment(aes(x=1+(barwidth/2), y=cum_sum_0[6], xend=2-(barwidth/2), yend=cum_sum_1[6]), colour="black", size=1.2, inherit.aes = FALSE, linetype = 2) +
            scale_fill_manual(values=col)+
            scale_y_continuous(labels = scales::percent)+
            ylab("Percentage (%)")+xlab("")+
            coord_flip()+
            guides(fill=guide_legend(title="modified Rankin Scale (mRS)"))+
            ggtitle(paste(title, " (N=", nrow(dataset), ")", sep=""))
        }
        
        #Without dashed lines
        if(dashedline == 2){
          graph<-ggplot(table2, aes(y=Percent, x=Intervention, fill=mRS,label=round(Percent*100))) +
            geom_bar(stat="identity", position="fill", colour="black", width = barwidth)+
            geom_text(aes(label = paste(round(Percent*100),"%",sep="")), position = position_stack(vjust = 0.5))+
            scale_fill_manual(values=col)+
            scale_y_continuous(labels = scales::percent)+
            ylab("Percentage (%)")+xlab("")+
            coord_flip()+
            guides(fill=guide_legend(title="modified Rankin Scale (mRS)"))+
            ggtitle(paste(title, " (N=", nrow(dataset), ")", sep=""))
        }

        
        return(graph)
      }
      

      plotInput <- eventReactive(input$action,{
          inFile <- input$dataset               
          if(is.null(inFile))
            return(NULL)
          file.rename(inFile$datapath,
                      paste(inFile$datapath, ".xlsx", sep=""))
          data<-readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
          grotta_bar(data[,c('trt','mrs')], input$title, input$control, input$intervention, input$barwidth, input$dashedline)
        })
      
      output$plot1 <- renderPlot({
        print(plotInput())
      })
      
      #Exportation parameters
      output$downloadPlot_png <- downloadHandler(
        filename = 'barplot.png',
        content = function(file) {
          ggsave(file, plot = plotInput(), height=input$height, width=input$width, units="in")
        })
      
      output$downloadPlot_tiff <- downloadHandler(
        filename = 'barplot.tiff',
        content = function(file) {
          ggsave(file, plot = plotInput(), device="tiff",height=input$height, width=input$width, units="in")
        })
      
      output$downloadPlot_png <- downloadHandler(
        filename = 'barplot.png',
        content = function(file) {
          ggsave(file, plot = plotInput(), height=input$height, width=input$width, units="in")
        })
      
      output$downloadPlot_eps <- downloadHandler(
        filename = 'barplot.eps',
        content = function(file) {
          ggsave(file, plot = plotInput(), device="eps",height=input$height, width=input$width, units="in")
        })
      
      output$downloadPlot_svg <- downloadHandler(
        filename = 'barplot.svg',
        content = function(file) {
          ggsave(file, plot = plotInput(), device="svg",height=input$height, width=input$width, units="in")
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
