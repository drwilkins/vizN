#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);require(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("So you're sayin' there's a chance?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("numero"),
         uiOutput("outOf"),
        sliderInput("pointcex","Point scale",min=1,max=20,value=1,step=.2)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("viz",height=600),
        verbatimTextOutput("string")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$viz <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
   #output$string<-renderText(rep("a",input$N))
   output$numero<-renderUI({
     numericInput(inputId = "k", label = "How many?", min = 1, max = 1e9,value=1)
    # numericInput(inputID= "k2", label="How many?",min=1,max=100,value=1)
   })
   
   
   output$outOf <- renderUI({
    numericInput(inputId = "numba", label = "Out of ____ (Enter an integer between 1 & 1 billion)", min = 1, max = 1e9,value=100) #2
    })
   
   output$viz<- renderPlot({
     N <- input$numba
     root<-sqrt(N)
     lim<-ceiling(root) #What's the nearest integer length of root N
     remainder<-ifelse(root==lim,0,(lim^2-N)) #if square axes don't equal root N, calculate how many points to remove
     d<-expand.grid(x=1:lim,y=1:lim)
     d<-d[order(d$x),]
     if(remainder!=0){ #If the grid isn't square, remove the remainder values
       remove.indx<-(nrow(d)-remainder):nrow(d)
       d<-d[-remove.indx[-1],]#remove[-1] first element bc of greedy remove.indx definition (remainder=1 should just remove 1 element)
       }
     g<-ggplot(d,aes(x=x,y=y))+geom_point(alpha=.3,size=input$pointcex)+theme_void()+xlab("")+ylab("")+coord_fixed()+geom_point(data=d[1:input$k,],aes(x=x,y=y),col="red",fill="red",pch=19,cex=input$pointcex*1.5)
     if(N>200000){
       binmagnitude<-round(log10(N))-3
       g <- g+stat_summary_2d()+ guides(fill = guide_legend(title = "LEFT", title.position = "left"))
   }
     g
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

