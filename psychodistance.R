library(psych)
library(shiny)
library(readxl)
library(shinythemes)
library(xlsxjars)
library(xlsx)
library(shiny)
library(mirt)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
   
   # Application title
   titlePanel("PSYCHOLOGICAL DISTANCE APPLICATION"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       fileInput("dataset", "Choose File", placeholder="File",buttonLabel = "Add",
                 accept = c(
                   "xlsx",
                   "xls",
                   "csv"))
      ),
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(
        tabPanel("About Application",br(),hr(),htmlOutput("about")),
        tabPanel("Converted Scale Values",br(),"Click button for download table",downloadButton('downloadcsv','Download Table'),hr(),tableOutput("table")),
        tabPanel("Total Scale Scores",br(),"Click button for download Scale Scores Table....",downloadButton('downloadscores','Download Table'),br(),"Click button for download Cut points Table........",downloadButton('downloadcutpoints','Download Table'),hr(),verbatimTextOutput("yazi"),verbatimTextOutput("kesme"),tableOutput("yorum"),tableOutput("scores")),
        tabPanel("Category Characteristic Curves",hr(),plotOutput("graf"))
      ))
   )
)

# Define server logic required to draw a histogram
server <- function(input,output,session) {
   fonk<- reactive({
     inFile <- input$dataset
     if (is.null(inFile))
       return("Please upload data")
     dataset<- read_xlsx(inFile$datapath, sheet=1)
     data<-as.matrix(dataset)
     maxx<-c()
     for(i in 1:ncol(data)){
       maxx[i]<-max(data[,i])
     }
     n<-max(maxx)
    
     model.gpcm <- paste("liking.science = 1-",ncol(data)) 
     results.gpcm <- mirt(data, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
     coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
     coef.gpcm$item
     a<-matrix(ncol = n-1,nrow =ncol(data))
     for(i in 1:ncol(data)){
       for(j in 1:n-1){
         a[i,j]<-mean(coef.gpcm$items[i,2:n])-coef.gpcm$items[i,j+1]
       }}
     yyy<-c()
     converted<-matrix(ncol = n,nrow =ncol(data))
     normdist<-c()
     fx<-c()
     integral<-c()
     result<-c()
     for(t in 1:ncol(data)){
       for(i in 1:n-1){
         normdist[i]<-1-pnorm(a[t,i])
       }
       for(i in 1:n-1){
         fx[i]<--(1/(sqrt(2*pi)))*exp(-((a[t,i])^2/2))}
       added<-append(normdist,0,after = 0)
       added<-append(added,1,after = n+1)
       
       for(i in 1:n){
         integral[i]<- added[i]-added[i+1]
       }
       addedfx<-append(fx,0,after = 0)
       addedfx<-append(addedfx,0,after = n+1)
       
       for(i in 1:n){
         result[i]<-round((addedfx[i]-addedfx[i+1])/integral[i],3)
       }
       scalevalue<-result
       for(i in 1:n-1){
         coeff<-(n-1)/(result[n]-result[1])
         yyy[i]<-round(((result[i+1]-result[1])*coeff+1),3)
       }
       converted[t,]<-append(yyy,1,after = 0)
       
     }
     convertedscalevalue<-converted
     meancsv<-c()
     for(i in 1:ncol(convertedscalevalue)){
       meancsv[i]<-round(mean(convertedscalevalue[,i]),3)
     }
     convertedscalevalue<-rbind(meancsv,convertedscalevalue)
     Item<-c()
     for(i in 1:ncol(data)){
      Item[i]<- paste(i,".item")
     }
     Item<-append(Item,"Scale CSV",after = 0)
     aa<-cbind(Item,convertedscalevalue)
      aa
   })
   cutpoints<-reactive({
     inFile <- input$dataset
     if (is.null(inFile))
       return("Please upload data")
     dataset<- read_xlsx(inFile$datapath, sheet=1)
     data<-as.matrix(dataset)
     maxx<-c()
     for(i in 1:ncol(data)){
       maxx[i]<-max(data[,i])
     }
     n<-max(maxx)
     
     model.gpcm <- paste("liking.science = 1-",ncol(data)) 
     results.gpcm <- mirt(data, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
     coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
     coef.gpcm$item
     a<-matrix(ncol = n-1,nrow =ncol(data))
     for(i in 1:ncol(data)){
       for(j in 1:n-1){
         a[i,j]<-mean(coef.gpcm$items[i,2:n])-coef.gpcm$items[i,j+1]
       }}
     yyy<-c()
     converted<-matrix(ncol = n,nrow =ncol(data))
     normdist<-c()
     fx<-c()
     integral<-c()
     result<-c()
     for(t in 1:ncol(data)){
       for(i in 1:n-1){
         normdist[i]<-1-pnorm(a[t,i])
       }
       for(i in 1:n-1){
         fx[i]<--(1/(sqrt(2*pi)))*exp(-((a[t,i])^2/2))}
       added<-append(normdist,0,after = 0)
       added<-append(added,1,after = n+1)
       
       for(i in 1:n){
         integral[i]<- added[i]-added[i+1]
       }
       addedfx<-append(fx,0,after = 0)
       addedfx<-append(addedfx,0,after = n+1)
       
       for(i in 1:n){
         result[i]<-round((addedfx[i]-addedfx[i+1])/integral[i],3)
       }
       scalevalue<-result
       for(i in 1:n-1){
         coeff<-(n-1)/(result[n]-result[1])
         yyy[i]<-round(((result[i+1]-result[1])*coeff+1),3)
       }
       converted[t,]<-append(yyy,1,after = 0)
       
     }
     convertedscalevalue<-converted
     meancsv<-c()
     for(i in 1:ncol(convertedscalevalue)){
       meancsv[i]<-round(mean(convertedscalevalue[,i]),3)
     }
     scale<-round(meancsv*ncol(data),2)
     categoryname<-c()
     for(i in 1:n){
       categoryname[i]<-paste(i,".Category")
     }
     hundscale<-c()
     for(i in 1:n){
     hundscale[i]<-round(scale[i]/(ncol(data)*n)*100,2)
   }
     res<-cbind(categoryname,scale,hundscale)
     res
   })
   info<-reactive({
     inFile <- input$dataset
     if (is.null(inFile))
       return("Please upload data")
     dataset<- read_xlsx(inFile$datapath, sheet=1)
     data<-as.matrix(dataset)
     maxx<-c()
     for(i in 1:ncol(data)){
       maxx[i]<-max(data[,i])
     }
     n<-max(maxx)
     str1<-paste("ScaleScores are between",ncol(data),"and",n*ncol(data),"HundredScores are between",(ncol(data)/(n*ncol(data)))*100,"and",100)
   })
   cuts<-reactive({
     "ScaleScores and HundredScores can interpret based on cut scores below."
     
   })
   graphic<-reactive({
     inFile <- input$dataset
     if (is.null(inFile))
       return("Please upload data")
     dataset<- read_xlsx(inFile$datapath, sheet=1)
     data<-as.matrix(dataset)
     polymodel<-mirt(data=data,model = 1, itemtype = "gpcm")
     plot(polymodel,type="trace")
     
   })
   score<-reactive({
     inFile <- input$dataset
     if (is.null(inFile))
       return("Please upload data")
     dataset<- read_xlsx(inFile$datapath, sheet=1)
     data<-as.matrix(dataset)
     maxx<-c()
     for(i in 1:ncol(data)){
       maxx[i]<-max(data[,i])
     }
     n<-max(maxx)
     model.gpcm <- paste("liking.science = 1-",ncol(data)) 
     results.gpcm <- mirt(data, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
     coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
     coef.gpcm$item
     a<-matrix(ncol = n-1,nrow =ncol(data))
     for(i in 1:ncol(data)){
       for(j in 1:n-1){
         a[i,j]<-mean(coef.gpcm$items[i,2:n])-coef.gpcm$items[i,j+1]
       }}
     yyy<-c()
     converted<-matrix(ncol = n,nrow =ncol(data))
     normdist<-c()
     fx<-c()
     integral<-c()
     result<-c()
     for(t in 1:ncol(data)){
       for(i in 1:n-1){
         normdist[i]<-1-pnorm(a[t,i])
       }
       for(i in 1:n-1){
         fx[i]<--(1/(sqrt(2*pi)))*exp(-((a[t,i])^2/2))}
       added<-append(normdist,0,after = 0)
       added<-append(added,1,after = n+1)
       
       for(i in 1:n){
         integral[i]<- added[i]-added[i+1]
       }
       addedfx<-append(fx,0,after = 0)
       addedfx<-append(addedfx,0,after = n+1)
       
       for(i in 1:n){
         result[i]<-round((addedfx[i]-addedfx[i+1])/integral[i],3)
       }
       scalevalue<-result
       for(i in 1:n-1){
         coeff<-(n-1)/(result[n]-result[1])
         yyy[i]<-round(((result[i+1]-result[1])*coeff+1),3)
       }
       converted[t,]<-append(yyy,1,after = 0)
     }
     convertedscalevalue<-converted
     scores<-matrix(ncol = ncol(data),nrow = nrow(data))
     for(i in 1:ncol(data)){
       for(j in 1:nrow(data)){
         if(data[j,i]==1){
           scores[j,i]<-1
         }    
         if(data[j,i]==2){
           scores[j,i]<-convertedscalevalue[i,2]
         }    
         if(data[j,i]==3){
           scores[j,i]<-convertedscalevalue[i,3]
         } 
         if(data[j,i]==4){
           scores[j,i]<-convertedscalevalue[i,4]
         } 
         if(data[j,i]==5){
           scores[j,i]<-convertedscalevalue[i,5]
         } 
         if(data[j,i]==6){
           scores[j,i]<-convertedscalevalue[i,6]
         }    
         if(data[j,i]==7){
           scores[j,i]<-convertedscalevalue[i,7]
         }    
         if(data[j,i]==8){
           scores[j,i]<-convertedscalevalue[i,8]
         }    
         if(data[j,i]==9){
           scores[j,i]<-convertedscalevalue[i,9]
         }    
       }
     }
     sums<-c()
     for(i in 1:nrow(scores)){
       sums[i]<-round(sum(scores[i,]),2)
     }
     meanscores<-round(mean(sums),2)
     sums<-append(sums,meanscores,after = 0)
     hundscore<-c()
     for(i in 1:length(sums)){
       hundscore[i]<-round((sums[i]/(n*ncol(data)))*100,2)
     }
     
     rows<-c()
     for(i in 1:nrow(data)){
       rows[i]<-paste(i,".individual")
     }
     rows<-append(rows,"Mean Score",after = 0)
     lastscore<-cbind(rows,sums,hundscore)
     lastscore
     
   })
   output$table<-renderTable({
     fonk()
   })
   output$scores<-renderTable({
     score()
   })
   output$yazi<-renderText({
     info()
   })
   output$yorum<-renderTable({
     cutpoints()
   })
   output$kesme<-renderText({
     cuts()
   })
   output$graf<-renderPlot({
     graphic()
   })
   output$about<-renderText({
    paste("This application was developed for calculating, the Converted Scale Value, corresponding to each of the scoring categories for each item in Likert Type
          and multiple scoring scales." ,br(),"In the calculations made with the application, the equationsput forward by Wakita, Ueshima and Noguchi (2012)
          were used. The model developed by Wakita et al. (2012) is based on Generalized Partial Credit Model (Muraki, 1993). The model put forward by Wakita et. al. (2012)
          allows calculation to be made for the whole scale. This application has a different feature since it provides the possibility of calculating at the item level
          This application does not require assumption of common category parameters for all items in the calculation process. With this application, category parameters and 
          converted scale values are calculated for each item. With this application, converted scale values are used for the scores obtained by individuals from each item. For example
          , consider an individual who selected the category of disagree for two diffrent items during scale application. According to the results of calculation, this individual can
          obtain 2.12 from one item and 1.98 from the other. Thus, it is possible to determine place of individuals on the score scale which is accepted as continuous. With the application,
          the total score is calculated for each individual according to the converted scale values.The total scores calculated for individuals are adjusted to the scale score system and 0-100
          score system. Interpretation intervals are also provided to the researchers for the interpretation of individual total scores and mean of the participants. Finally, with this application,
          category characteristic curves for each item in the scale are produced for the researchers. The researchers will be able to download these curves by taking print screen for all scale 
          items.",hr(),"Points to take into consideration when using the application:",br(),"1-Your data should be in Microsoft Office Excel format. In the other words, the extension of your file should be .xlsx",br(),
          "2-There should not be any data except item scores in the data set.",br(),"3-Item names should appear in the first row of the data set",br(),"4-There should be no missing data in the data set",br(),
          "5-There should be no Turkish characters in the item nomenclaturein the data set",br(),"6-Number of categories should be maximum 9",br(),"7-The processing time can be 1 to 30 seconds depending on the size of the data set",hr(),"Developers:",br(),"Hüseyin YILDIZ - e-mail:huseyinyildiz35@gmail.com",br(),
          "Dr. Alperen YANDI - e-mail:alperenyandi@gmail.com",hr())
   })
   output$downloadcsv <- downloadHandler(
     filename = function() {
       paste("ConvertedSV", ".xlsx", sep="")
     },
     content = function(file) {
       write.xlsx(fonk(), file,row.names = FALSE)
     }
   )
   output$downloadscores <- downloadHandler(
     filename = function() {
       paste("ScaleScores", ".xlsx", sep="")
     },
     content = function(file) {
       write.xlsx(score(), file,row.names = FALSE)
     }
   )
   output$downloadcutpoints <- downloadHandler(
     filename = function() {
       paste("Cutpoints", ".xlsx", sep="")
     },
     content = function(file) {
       write.xlsx(yorumlar(), file,row.names = FALSE)
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

