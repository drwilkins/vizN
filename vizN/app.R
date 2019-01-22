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
   titlePanel("vizN: visualizing numbers & fractions"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, uiOutput("numero")
            ),
          column(6, uiOutput("outOf"))
            ),
        fluidRow(
          column(4,
          checkboxInput("scalepts","Autoscale Points?",value=T)),
          column(8,
          sliderInput("pointcex","Manual point scale",min=.05,max=20,value=3,step=.2)
            )
      )
      ),
    
      
      # Show a vizualization
      mainPanel(
         plotOutput("viz",height=600),
        downloadButton("down",label="Download Plot")
      )
   )
)




 #--------------------------------------------------------
# Define server logic 
server <- function(input, output) {
   #Render input sliders that actually respect min/max values
   
  G<-reactiveValues() #Used to access graph outside renderPlot
  
  output$numero<-renderUI({
     numericInput(inputId = "k", label = "How many?", min = 1, max = 1e9,value=55)
   })
   
   
   output$outOf <- renderUI({
    numericInput(inputId = "numba", label = "Out of ____? (integer)", min = 1, max = 1e9,value=100) #2
    })
   
   output$viz<- renderPlot({
     N <- input$numba
     k<- input$k
     #Autoscale vs Manual point scaling
     if(input$scalepts==F){pts<-input$pointcex}else{
            #slightly different models for pt size before &after 9999
           if(N>9999){pts=30*exp(-.006*N)+.01}else{pts=20*exp(-.002*N)+.5} }
     
     root<-sqrt(N)
     lim<-ceiling(root) #What's the nearest integer length of root N
     remainder<-ifelse(root==lim,0,(lim^2-N)) #if square axes don't equal root N, calculate how many points to remove
     d<-expand.grid(x=1:lim,y=1:lim)
     d<-d[order(d$x),]
     if(remainder!=0){ #If the grid isn't square, remove the remainder values
       remove.indx<-(nrow(d)-remainder):nrow(d)
       d<-d[-remove.indx[-1],]#remove[-1] first element bc of greedy remove.indx definition (remainder=1 should just remove 1 element)
     }
     mytheme<-theme_void()
     g<-ggplot(d,aes(x=x,y=y))+geom_point(alpha=1,size=pts,col="#011627")+xlab("")+ylab("")+xlim(min(d$x)-1,max(d$x)+1)+ylim(min(d$y)-1,max(d$y)+1)+mytheme+ guides(fill = guide_legend(title = "LEFT", title.position = "left"))+coord_fixed()
     #If How many>0, add red dots
     if(k>0){g<-g+geom_point(data=d[1:k,],aes(x=x,y=y),col="#F71735",pch=19,cex=pts*1)}
     
     #####The tricky part: if N>100k
     if(N>1000000){
       binmagnitude<-round(log10(N))-3
       g <- g+stat_summary_2d()
     }
     #Add title
     warn<-ifelse(k>N,"  *Improper Fraction*","")
     ttl<-substitute( paste(frac(k,N),warn),list(k=format(k, big.mark=",", scientific=FALSE),N=format(N, big.mark=",", scientific=FALSE),warn=warn))
     
     g<-g+ ggtitle(ttl)+theme(plot.title = element_text(hjust = 0.5,size=34,face="bold",colour="#011627",family="serif"))#,plot.background = element_rect(colour = '#011627',size=1)
     G$plot<-g #assign reactive value
     g
   })#end renderPlot
   
   output$down<-downloadHandler("vizN.jpeg",
     content=function(file){
       ggsave(file,plot=G$plot,width=8,height=9)
       }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

