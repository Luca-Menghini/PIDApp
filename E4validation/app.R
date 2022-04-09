library(shiny)
library(pracma)
library(data.table)  ###################### interessante anche server(input,output,session)

if(!exists("ibi_new")){
  ibi_new <- data.frame(t=numeric(1),IBI=numeric(1))
}

dm <- NULL

#########################################################################################################################################
ui <- shinyUI(fluidPage(
  fluidRow(
    column(width=3,style="background-color:#E8F0FF;",
           div(
             HTML("<br>"),
             
             fileInput("datafile","1. Carica un file di dati",
                       accept=c("text/csv","text/comma-separated-values,text/plain",".csv"),
                       buttonLabel="Carica"),
             selectInput("condition","2. Scegli una condizione",
                         choices=c("SEATED","BREATH","STAND","MOVE","TYPE",
                                   "STROOP","ANTICIP","SPEECH","RECOVERY")),
             numericInput("epoch","3. Scorri le epoche",
                          min=0,max=100,step=1,value=0),
             actionButton("update","Aggiungi picchi",
                          style="background-color:#2A6FCC;
                          color: white;"),
             HTML("<br><br>"),
             HTML("Se individua i picchi correttamente, clicca su <i>Aggiungi picchi</i>
                  per aggiungerli al dataset e passa alla prossima epoca (fai attenzione a 
                  cliccare soltanto una volta!). 
                  <br>Altrimenti, modifica i <i>Parametri</i> dell'algoritmo.<br>"),
             
             
             h4("Parametri"),
             numericInput(inputId="minPeak",label="Altezza minima dei picchi",
                          value=0,min=-100,max=100,step=1),
             numericInput(inputId="minDist",label="Distanza minima = IBI (sec)*",
                          value=0.29,min=0.15,max=3,step=0.01),
             textOutput("HR"),
             HTML("<br>"),
             numericInput(inputId="nPeaks",label="Numero max di picchi",
                          value=30,min=0,max=20,step=1),
             numericInput(inputId="width",label="Larghezza epoca (sec)",
                          value=7.81,min=0,max=20,step=0.1),
             HTML("Aumentala leggermente se l'ultimo picco non rientra nell'ultimo
                  secondo rappresentato nel grafico, altrimenti avrai un GAP!<br><br>"),
             
             h5("4. Salva il dataset",style="font-weight: bold"),
             HTML("Quando hai completato i picchi di una condizione, salva il dataset e
                  clicca su <i>Riparti dall'inizio</i> per iniziare con la prossima (punto 2).<br><br>"),
             downloadButton("download","Download",
                            style="background-color:#2A6FCC;color: white;"),
             hr(),
             style="font-size:90%;"
             )
    ),
    column(width=6,offset=0,
           
           h3("Recoding plot"),
           hr(),
           plotOutput("recording",height="100px"),
           
           h3("Peaks plot"),
           hr(),
           plotOutput("window"),
           
           h3("IBI plot"),
           hr(),
           plotOutput("cardio"),
           
           h3("Photopletysmography vs ECG"),
           hr(),
           fileInput("ekg","",
                     accept=c("text/csv","text/comma-separated-values,text/plain",".csv"),
                     buttonLabel="Carica dati ECG"),
           plotOutput("PPGvsECG")
    ),
    column(width=3,offset=0,style="background-color:#FFE6E6;font-size:80%;",
           h4("Peaks"),
           tableOutput("peaks"),
           hr(),
           h4("Dataset IBI"),
           tableOutput("dataset")
    )
    
           
    )
  )
)


##################################################################################################################################
server <- function(input, output){
  
  
  
  # Displayin HR related to minDist
  output$HR <- renderText({
    paste("*Significa che HR max =",round(1/input$minDist*60,0))
  })
  
  # loading data
  options(shiny.maxRequestSize=30*1024^2) # setting max file size to 30 MB
  bvp <- reactive({
    bvp <- read.csv(input$datafile$datapath,header=TRUE,sep=";",dec=".")
    bvp$dataInv <- bvp$data*-1 # inverted bvp data (easier to check)
    bvp
  })
  
  # selecting experimental condition
  bvpPlot <- reactive({
    if(input$condition=="SEATED"){
      bvpPlot <- bvp()[which(bvp()$event=="SEATED"):which(bvp()$event=="SEATED_stop"),]}
    else if(input$condition=="PACED BREATHING"){
      bvpPlot <- bvp()[which(bvp()$event=="BREATH1"):which(bvp()$event=="BREATH18_stop"),]}
    else if(input$condition=="STAND"){
      bvpPlot <- bvp()[which(bvp()$event=="STAND"):which(bvp()$event=="STAND_stop"),]}
    else if(input$condition=="MOVE"){
      bvpPlot <- bvp()[which(bvp()$event=="MOVE"):which(bvp()$event=="MOVE_stop"),]}
    else if(input$condition=="TYPE"){
      bvpPlot <- bvp()[which(bvp()$event=="TYPE"):which(bvp()$event=="TYPE_stop"),]}
    else if(input$condition=="STROOP"){
      bvpPlot <- bvp()[which(bvp()$event=="STROOP"):which(bvp()$event=="STROOP_stop"),]}
    else if(input$condition=="ANTICIP"){
      bvpPlot <- bvp()[which(bvp()$event=="ANTICIP"):which(bvp()$event=="SPEECH"),]}
    else if(input$condition=="SPEECH"){
      bvpPlot <- bvp()[which(bvp()$event=="SPEECH"):which(bvp()$event=="RECOVERY"),]}
    else{
      bvpPlot <- bvp()[which(bvp()$event=="RECOVERY"):which(bvp()$event=="RECOVERY_stop"),]}
    bvpPlot
  })
  
  # peaks finding and coding
  peaks <- reactive({
    peaks <- findpeaks(bvpPlot()$dataInv[(input$epoch*440):(input$epoch*440+input$width*64)],
                       minpeakheight=input$minPeak,
                       minpeakdistance=input$minDist*64,
                       zero="+",
                       npeaks=input$nPeaks)
    peaks <- peaks[order(peaks[,2]),] # sorting peaks by col2 values
    peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating IBI column
    for(i in 1:nrow(peaks)-1){
      peaks[i,1] <- ((peaks[i+1,3]-peaks[i,3])/64) # calculate IBIs in seconds
    }
    sec <- input$epoch*440/64 # sec from 0 of the first IBI
    peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating TIME column
    for(i in 1:nrow(peaks)){
      peaks[i,1] <- (sec+peaks[i,4]/64) # calculate sec from zero
    }
    colnames(peaks) <- c("t","IBI","y","x","start","end")
    peaks <- as.data.frame(peaks)
    peaks
  })
  
  ######################
  # DATASET POPULATION # ######################################################
  ######################
  
  # creating starting empty dataframe ibi() with t=1 and IBI=1
  ibi <- reactive({
    req(input$datafile)
    # adding permanentely (<<-) features to ibi()
    dm <<- data.frame(t=numeric(1),IBI=numeric(1))
  })
  
  # adding new peaks data to the existing ibi()
  addData <- eventReactive(input$update, {
    # removing peaks repeated between two epochs
    for(i in 1:nrow(ibi_new)){ 
      ifelse((ibi()[i,1]==ibi_new[i-1,1]),
             ibi() <- ibi()[-i,],
             ibi() <- ibi())
    }
    dm <<- rbind.data.frame(ibi(),peaks()[1:nrow(peaks())-1,1:2]) # not taking the last peaks() row
  })
  
  RESET <- "OFF"
  
  # adding peaks[,1:2] to dm (<<-)
  ibi_new <- eventReactive(input$update,{
        dm <<- rbind.data.frame(dm,peaks()[1:nrow(peaks())-1,1:2])
  })
  
  #########
  # RESET # #################################################################
  #########
  
  RESET <- eventReactive(input$reset, {
    RESET <- "ON"
  })
  
  #################################
  # DATASET PRINTING AND PLOTTING # #########################################
  #################################
  
  # displaying peaks dataset
  output$peaks <- renderTable({
    req(input$datafile)
    peaks_res <- cbind(peaks()[,1:2],peaks()[,2]-mean(peaks()[,2])) # IBI residuals
    colnames(peaks_res) <- c("t","IBI","res")
    data.table(peaks_res,keep.rownames=TRUE)
  })
  
  # displaying IBI dataset with GAPS
  output$dataset <- renderTable({
    req(input$datafile)
    gap <- cbind(ibi_new(),rep(1,NROW(ibi_new()))) # creating "t2-(t1+d1)" column
    for(i in 1:nrow(gap)){
      gap[i,3] <- (gap[i+1,1]-(gap[i,1]+gap[i,2]))
    }
    colnames(gap) <- c("t","IBI","GAP")
    data.table(gap,keep.rownames=TRUE)
  })
  
  
  # plotting whole recording
  output$recording <- renderPlot({
    req(input$datafile)
    par(mai=c(0,0,0,0),mar=c(0,0,0,1),bg="gray")
    plot(bvpPlot()$data, 
         xaxt="n",yaxt="n",xlab="",ylab="",type="l",ylim=c(-100,100),
         xaxs="i",yaxs="i")
    rect(input$epoch*440,-100,input$epoch*440+input$width*64,100,col=rgb(1,1,0,alpha=0.3))
  }
  ,height = 100)
  
  # plotting peaks
  output$window <- renderPlot({
    req(input$datafile)
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    plot(bvpPlot()$dataInv[(input$epoch*440):(input$epoch*440+input$width*64)],
         ylab="bvp",xlab="sec",xaxt="n",type="l")
    axis(1,at=seq(0,512,64),labels=round(seq(input$epoch*440/64,input$epoch*440/64+512/64,1),1))
    abline(v=seq(0,512,64),lty=2,col="gray")
    abline(h=input$minPeak,lwd=2,col="blue")
    peaks <- findpeaks(bvpPlot()$dataInv[(input$epoch*440):(input$epoch*440+input$width*64)],
                       minpeakheight=input$minPeak,
                       minpeakdistance=input$minDist*64,
                       zero="+",
                       npeaks=input$nPeaks)
    points(peaks[,2],peaks[,1],pch=20,col="red") # shows peaks on graph
    text(peaks[order(peaks[,2]),][,2]+5,peaks[order(peaks[,2]),1]+5,cex= 0.8)
    lines(peaks[order(peaks[,2]),2],peaks[order(peaks[,2]),1],lty=2,col="red")
    })
  
  # polygon(c(0,seq(0,6,0.01),6), # 
  #        c(0,dnorm(mean=3,seq(0,6,0.01)),0),
  #        col="#004987")
  
  
  # plotting IBI serie
  output$cardio <- renderPlot({
    req(input$datafile)
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    plot(ibi_new()$t,ibi_new()$IBI,type="b",lwd=1,pch=20,col="red",
         ylab="IBI (sec)",xlab="time (sec)")
  })
  
  # loading ECG ibi
  ekg <- reactive({
    ekg <- read.csv(input$ekg$datapath,header=TRUE,sep=";",dec=".")
    ekg
  })
  
  # plotting ECG vs PPG ibi serie
  output$PPGvsECG <- renderPlot({
    req(input$datafile)
    req(input$ekg)
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    plot(ibi_new()$IBI,col="red2",
         ylab="IBI (sec)",xlab="time (sec)",type="l")
    lines(ekg()[ekg()$event==as.character(input$condition),2])
    legend("topright",
           legend=c("PPG","ECG"),col=c("red2","black"),
           lwd=3)
  })

  ###################
  # DATASET DOWLOAD # ###########################################################################
  ###################
  
  output$download <- downloadHandler(
    filename = paste("ibi_",input$condition,".csv",sep=""),
    content = function(file) {
      write.csv(ibi_new(),file,sep=";",row.names=FALSE)
    },
    contentType="text/csv"
  )  
  
}

shinyApp(ui = ui, server = server)
