library(shiny)
library(ggplot2)
library(birk)
library(empatica)
library(data.table)
library(pracma)
# new packages
library(plotly)
library(purrr)
library(readr)
library(DT)

# problema time zone e cambio ora
# (events, brush_output, plot whole recording, zooming)

###################################################################################### USER INTERFACE

ui <- shinyUI(fluidPage(
  
  titlePanel("PIDApp - Pulse Intervals Detection App"),
  tabsetPanel(
    
    # -----------------------------------------------------------------------
    # TAB 1: INTERVAL SELECTION
    # -----------------------------------------------------------------------
    
    tabPanel("1. Interval Selection",
             
             # -----------------------------------------------------------------------
             # COLUMN 1
             # -----------------------------------------------------------------------
             
             column(width=3,style="background-color:#BBCDEB;font-size:90%;",
                    div(
                      
                      # --------------------------------------------------------------
                      # Data loading
                      # --------------------------------------------------------------
                      
                      h3("Open file",style="font-weight: bold"),
                      fileInput("datafile","",
                                accept=c("text/csv","text/comma-separated-values,text/plain",".csv"),
                                buttonLabel="Open"),
                      
                      radioButtons("datafileFormat",label="File type",
                                                    choices=c("BVP Empatica","BVP (other)","IBI"),selected="BVP Empatica",inline=T),
                      radioButtons("datafileOptions",label="File options",
                                                    choices=c("Hide","Show"),selected="Hide",inline=T),
                      
                      conditionalPanel(condition = "input.datafileOptions == 'Show'",
                                       style="background-color:#A2B9E5;font-size:90%;text-indent:1em",
                                       HTML("<br>"),
                                       h3("File options",style="font-weight: bold"),
                                       HTML("<br>"),
                                       checkboxInput("header","Header",TRUE),
                                       HTML("<br><b>Data column</b>"),
                                       numericInput("DATAcol","",
                                                    value=2,step=1,width="30%"),
                                       radioButtons("DATAunit",label="Data units",
                                                    choices=c("ms","s"),selected="ms",inline=T),
                                       HTML("<br><b>Time index column (leave 0 if not)</b>"),
                                       numericInput("TIMEcol","",
                                                    value=1,step=1,width="30%"),
                                       conditionalPanel(condition = "input.TIMEcol == 0",
                                                        numericInput("SamplingRate","Sampling rate",
                                                                     value=1000,step=1,width="30%")
                                       ),
                                       radioButtons("TIMEunit",label="Time units",
                                                    choices=c("ms","s"),selected="s",inline=T),
                                       HTML("<br><b>Events column (leave 0 if not)</b>"),
                                       numericInput("NOTEcol","",
                                                    value=0,step=1,width="30%"),
                                       HTML("<br>"),
                                       radioButtons("sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                "White Space" = "",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       radioButtons("quote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       HTML("<br>"),
                                       radioButtons("disp", "Display",
                                                    choices = c("Head","Hide"),
                                                    selected = "Hide"),
                                       HTML("<br>"),
                                       tableOutput("head")
                                       
                                       
                      ),

                      conditionalPanel(condition = "input.datafileFormat == 'BVP Empatica'",
                                       
                                       HTML("<br>"),
                                       HTML("Recording date (year/month/day):"),
                                       textOutput("brush_output_date"),
                                       tags$head(tags$style("#brush_output_date{color: darkred;
                                                            font-size: 20px;
                                                            }"))
                      ),
                                       
                      
                      hr(),
                      
                      # --------------------------------------------------------------
                      # Interval selection
                      # --------------------------------------------------------------
                      
                      h3("Interval selection",
                         style="font-weight: bold"),
                      HTML("Drag the mouse over the <b>Recording plot</b> to select the <b>Interval</b> to be considered for IBI detecion.<br><br>"),
                      
                      # add/remove seconds to LOWER and UPPER bounds
                      textOutput("brush_output_max"),
                      tags$head(tags$style("#brush_output_max{color: darkred;
                                           font-size: 20px;
                                           }")),
                      HTML("<b>Add or remove seconds to the UPPER bound</b>"),
                      numericInput(inputId="addsec_max","",
                                   min=-180,max=180,step=1,value=0,width="30%"),
                      textOutput("brush_output_min"),
                      tags$head(tags$style("#brush_output_min{color: darkred;
                                           font-size: 20px;
                                           }")),
                      HTML("<b>Add or remove seconds to the LOWER bound</b>"),
                      numericInput(inputId="addsec_min","",
                                   min=-180,max=180,step=1,value=0,width="30%"),
                      
                      # --------------------------------------------------------------
                      # Markers table
                      # --------------------------------------------------------------
                      
                      hr(),
                      h3("Event Markers",
                         style="font-weight: bold"),
                      HTML("If available, the table shows the timestamp of the events marked on the recording.
                            Upper bounds are marked as 'SUP'. When selected, events are marked as 'OK'.<br><br>"),
                      tableOutput("events"),
                      hr()
                      
                      )), # close column and div
             
             # -----------------------------------------------------------------------
             # COLUMN 2 (plotting)
             # -----------------------------------------------------------------------
             column(width=9,offset=0,
                    div(
                      
                      conditionalPanel(condition = "input.datafileFormat != 'IBI'",
                                       
                                       h3("Recoding plot",
                                          style="font-weight: bold"),
                                       HTML("The BVP signal over the recording. 
                                             Red lines represents event markers.
                                             <br>Drag the mouse to select the <b>interval</b>.<br><br>"),
                                       radioButtons("portion",label="Portion to be visualized:",
                                                    choices=c("First part",
                                                              "Second part",
                                                              "Whole recording"),selected="First part",inline=T),
                                       hr(),
                                       plotOutput("recording",
                                                  height="250px",
                                                  brush = brushOpts(id="brush", # brush is for zooming
                                                                    direction="x")),
                                       
                                       h3("Interval plot",
                                          style="font-weight: bold"),
                                       HTML("Interval of interest selected on the Recording plot."),
                                       hr(),
                                       plotOutput("zoom",
                                                  height="250px")
                      ), # close conditional panel
                      
                      conditionalPanel(condition = "input.datafileFormat == 'IBI'",
                                       h3("IBI time series",
                                          style="font-weight: bold"),
                                       HTML("IBI (ms) time series and istantaneous HR (bpm) over time.<br><br>"),
                                       radioButtons("plotMode",label="Metric:",
                                                    choices=c("IBI","HR"),selected="IBI",inline=T),
                                       hr(),
                                       
                                       plotOutput("ibiPlot",
                                                  height="350px")
                      ) # close conditional panel
                      
                    )) # close div() and column
             
                      ), # close tabpanel
                    
    
    # -----------------------------------------------------------------------
    # TAB 2: PEAKS DETECTION
    # -----------------------------------------------------------------------

    tabPanel("2. Peaks Detection",

             # -----------------------------------------------------------------------
             # 2.1. Settings - COLUMN 1
             # -----------------------------------------------------------------------

             column(width=3,style="background-color:#BBCDEB;font-size:90%;",
                    div(
                      
                      ########################################################## 1.1. ADD PEAKS AND CHANGE EPOCH
                      
                      h3("Peaks detection",style="font-weight: bold"),
                      HTML("Add detected peaks to the IBIs dataset and shift to the following epoch, 
                            untill the end of the interval. Change the algorithm's <b>parameters</b> or 
                            <b>mark the artifacts</b> when peaks are not correctly detected.
                            Shift to <b>BVP mode</b> if your focus is on BVP amplitude.<br>"),
                      hr(),
                      conditionalPanel(condition="input.datafileFormat!='IBI'",
                        radioButtons("detectionMode",label="Detection mode",
                                     choices=c("IBI","BVP"),selected="IBI",inline=T)
                      ),
                      HTML("<b>Shift between epochs</b>"),
                      numericInput("epoch","",
                                   min=1,max=100,step=1,value=1,width="30%"),
                      
                      actionButton("update","Add peaks to the dataset",
                                   icon = icon("plus"),
                                   style="background-color:#337AB7;color: white;"),
                      HTML("<br><br>"),
                      
                      ################################################################# 1.2. DELETE PEAKS
                      
                      actionButton("back"," Delete selected peaks",
                                   icon = icon("minus"),
                                   style="background-color:#337AB7;color: white;"),
                      HTML("<br>"),
                      
                      ################################################################## 1.3. ADD ALL INTERVAL
                      
                      HTML("<br>"),
                      actionButton("addAll"," Add the whole interval",
                                   icon = icon("plus"),
                                   style="background-color:#337AB7;color: white;"),
                      
                      ################################################################# 1.4. GAP WARNINGS
                      
                      HTML("<br><br>"),
                      verbatimTextOutput("feedback"), # number of added peaks
                      verbatimTextOutput("gapPrevention",placeholder=FALSE), # gap prevention warning
                      verbatimTextOutput("gapWarning",placeholder=FALSE), # gap detection warning
                      
                      hr(),
                      ################################################################## 1.5. FIND PEAKS SETTINGS
                      
                      h3("Algoritm's parameters",style="font-weight: bold"),
                      # 3. epoch width
                      sliderInput(inputId="width",label="Epoch width (sec)",
                                   value=7.81,min=7.0,max=8.5,step=0.1),
                      HTML("Change it untill the last epoch is marked in green.<br><br>"),
                      
                      # 2. min peaks height
                      numericInput(inputId="minPeak",label="MIN peaks height",
                                  value=0,min=-100,max=100,step=1,width="50%"),
                      numericInput(inputId="maxPeak",label="MAX peaks height",
                                   value=500,min=-100,max=Inf,step=1,width="50%"),
                      HTML("Peaks under (MIN) or over (MAX) the blue lines will not be considered.<br><br>"),
                      
                      # 3. min peaks distance
                      numericInput(inputId="minDist",label="MIN IBI length (sec)*",
                                  value=0.40,min=0.15,max=3,step=0.01,width="50%"),
                      HTML("Change it to avoid double or missing peaks."),
                      textOutput("HR"),
                      HTML("<br><br><br><br><br>")
                    )
                      ),
             # 
             # -----------------------------------------------------------------------
             # 2.2. Plotting - COLUMN 2
             # -----------------------------------------------------------------------
             # 
             column(width=6,offset=0,style="font-size:90%;",

                    h3("Interval plot",
                       style="font-weight: bold"),
                    HTML("BVP signal in the selected interval.
                          The selected epoch is marked in yellow. Epochs added to the IBI dataset are marked in green.
                          Sensor acceleration (if available) is marked in blue."),
                    hr(),
                    plotOutput("zoom2",height="100px"),

                    h3("Peaks plot",
                       style="font-weight: bold"),
                    HTML("L'andamento del segnale BVP nel tempo (sec) nell'epoca selezionata.
                         I picchi (i.e., battiti) sono indicati dai pallini rossi e dai numeri.
                         Gli intervalli interbattito (IBI) sono indicati dalle linee rosse tratteggiate,
                         mentre le linee verdi tratteggiate indicano l'ampiezza del segnale BVP."),
                    hr(),
                    plotOutput("window2"),
                    
                    # showing IBIs and BVP depending on detectionMode
                    conditionalPanel(condition = "input.detectionMode == 'BVP'",
                                     
                                     h3("BVP amplitude plot",
                                        style="font-weight: bold"),
                                     HTML("BVP amplitude over time."),
                                     hr(),
                                     plotOutput("BVPamp")
                    ),
                    
                    h3("IBI plot",
                       style="font-weight: bold"),
                    HTML("IBIs (sec) time series and instantaneous HR (bpm) over time (sec).<br><br>"),
                    radioButtons("plotMode2",label="Metric:",
                                 choices=c("IBI","HR"),selected="IBI",inline=T),
                    hr(),
                    plotOutput("cardio"),
                    
                    conditionalPanel(condition = "input.detectionMode == 'BVP'",
                                     h3("HR and BVP amplitude",
                                        style="font-weight: bold"),
                                     HTML("Relationship between HR and BVP amplitude"),
                                     hr(),
                                     plotOutput("HRvsBVP")
                    
                    )
                    
             ),

             # -----------------------------------------------------------------------
             # 2.3. Showing datasets and artifact marking - COLUMN 3
             # -----------------------------------------------------------------------

             column(width=3,offset=0,style="background-color:#BBCDEB;font-size:90%;",
                    
                    h3("Peaks table",
                       style="font-weight: bold"),
                    conditionalPanel(condition="input.detectionMode == 'IBI'",
                                     HTML("<br><i>IBI</i> = sec between a peak and the following one
                                          <br><i>RES</i> = IBi residual<br><br>")
                                     ),
                    conditionalPanel(condition="input.detectionMode == 'BVP'",
                                     HTML("<br><i>BVPamp</i> = BVP amplitude of a given peak 
                                          <br><i>RES</i> = BVPamp residual<br><br>")
                    ),
                    div(tableOutput("peaks"), style = "font-size:85%"),
                    verbatimTextOutput("feedback.hrv"),
                    hr(),
                    
                    h3("Marca gli artefatti",
                       style="font-weight: bold"),
                    HTML("Dopo aver aggiunto i picchi al dataset, marca come artefatti
                         quelli che ti sembrano anomali (prima devi aggiungerli al dataset!).<br><br>"),
                    numericInput("artefact",label="Scegli un picco", 
                                 min=1,max=100000,value=1,width="50%"),
                    verbatimTextOutput("markWarning",placeholder=FALSE),
                    actionButton("markArtefact"," Marca il picco come artefatto",
                                style="background-color:#337AB7;color: white;",
                                icon=icon("crosshairs")),
                    HTML("<br><br>"),
                    actionButton("demarkArtefact"," Cancella il marker",
                                 style="background-color:#337AB7;color: white;",
                                 icon=icon("undo")),
                    hr(),
                    
                    h3("Dataset IBI",
                       style="font-weight: bold"),
                    HTML("<i>ampl</i> = ampiezza del segnale BVP
                          <br><i>artefact</i> = picchi marcati come artefatti<br><br>"),
                    div(tableOutput("dataset"), style = "font-size:85%"),
                    hr(),
                    h3("Elimina un IBI",style="font-weight: bold"),
                    HTML("Al termine dell'intervallo, elimina i picchi superflui associati
                         al movimento per la pressione del pulsante. Controlla il tempo (seconda colonna)
                         dell'ultimo picco pulito, inserisci il numero corrispondente
                         ed elimina tutti gli IBI fino a quel numero.<br><br>"),
                    numericInput("row.selection",label="Seleziona un picco da eliminare", 
                                 min=1,max=440,value=1,width="50%"),
                    actionButton("delete","Elimina picco",
                                 style="background-color:#337AB7;color: white;",
                                 icon=icon("bomb")),
                    HTML("<br><br>")
             )
    ),
    
    # -----------------------------------------------------------------------
    # TAB 3: INTERPOLATION
    # -----------------------------------------------------------------------
    
    tabPanel("3. IBI correction & interpolation",
             
             # -----------------------------------------------------------------------
             # 3.1. Settings - COLUMN 1
             # -----------------------------------------------------------------------
             
             column(width=4,style="background-color:#BBCDEB;font-size:90%;",
                    div(
                      
                      # --------------------------------------------------------------
                      # 3.1.1 Instructions
                      # --------------------------------------------------------------
                      
                      # h3("Istruzioni",style="font-weight: bold"),
                      # HTML("In questa sezione ?? possibile <b>eliminare gli artefatti</b>
                      #       e <b>filtrare</b> la serie temporale della frequenza cardiaca.
                      #       Dopo aver corretto gli artefatti, procedere all'<b>interpolazione</b>
                      #       del segnale e salvare il dataset degli IBI premendo sul tasto download.<br><br>
                      #      Una volta premuto il pulsante dell'interpolazione e salvato il dataset,
                      #      passare alla prossima sezione.<br><br>"),
                      # --------------------------------------------------------------
                      # 3.1.2. Correction
                      # --------------------------------------------------------------
                      
                      h3("Artefacts processing",style="font-weight: bold"),
                      HTML("If working with BVP signal, process the data points marked as artefacts in section 2.
                           Manual correction allows to manually change or mark the artefacts<br><br>"),
                      
                      # manual correction
                      radioButtons("manualEditor",label="Manual correction:",
                                   choices=c("Show","Hide"),selected="Hide",inline=T),
                      
                      # automatic filtering
                      conditionalPanel(condition = "input.manualEditor=='Show'",
                                       style="background-color:#A2B9E5;font-size:90%;text-indent:1em",
                                       HTML("<br>"),
                                       h3("Manual Correction",style="font-weight: bold"),
                                       HTML("<br>Drag data points on the plot to correct them.<br><br>"),
                                       DT::dataTableOutput("manualEditor.dataset"),
                                       HTML("<br>")
                                       
                      ),
                      
                      HTML("<br>"),
                      radioButtons("processingMethod",label="Processing method:",
                                   choices=c("Cubic spline interpolation",
                                             "Linear interpolation",
                                             "Delete",
                                             "Mark as missing data"),
                                   selected="Delete",inline=F),
                      HTML("<br>"),
                      actionButton("deleteArtefacts","Process artefacts",
                                   style="background-color:#337AB7;color: white;",
                                   icon=icon("bomb")),
                      HTML("<br><br>"),
                      verbatimTextOutput("artefactFeedback",placeholder=FALSE),
                      
                      hr(),
                      
                      h3("Automatic filtering",style="font-weight: bold"),
                      HTML("Artefacts are detected based on minimum and maximum HR values
                            and a threshold computed based on the IBIs average.<br><br>"),
                      radioButtons("filteringOptions",label="Filtering option",
                                   choices=c("Show","Hide"),selected="Hide",inline=T),
                      
                      conditionalPanel(condition = "input.filteringOptions=='Show'",
                                       style="background-color:#A2B9E5;font-size:90%;text-indent:1em",
                                       HTML("<br>"),
                                       h2("Filtering options",style="font-weight: bold"),
                                       HTML("<br><br>"),
                                       # min and max bpm
                                       numericInput(inputId="minbpm",label="MIN HR (bpm)",
                                                    min=1,max=200,step=1,value=35),
                                       numericInput(inputId="maxbpm",label="MAX HR (bpm)",
                                                    min=1,max=300,step=1,value=200),
                                       # number of IBIs considered for calculating the averaged threshold
                                       sliderInput(inputId="long",label="Number of IBIs considered by the threshold",
                                                   min=1,max=100,step=1,value=50),
                                       verbatimTextOutput("longWarning",placeholder=FALSE),
                                       HTML("Define the number of IBIs used to compute the average
                                            with which each IBI is compared.<br><br>"),
                                       sliderInput(inputId="last",label="Initial threshold (sec)",
                                                   min=0,max=1,step=0.01,value=0.50),
                                       HTML("Define the maximum difference between a given IBI and the average.
                                            IBIs over this value are considered as artefacts.<br><br>")              
                      ),
                      
                      HTML("<br>"),
                      radioButtons("processingMethod2",label="Processing method:",
                                   choices=c("Cubic spline interpolation",
                                             "Linear interpolation",
                                             "Delete",
                                             "Mark as missing data"),
                                   selected="Delete",inline=F),
                      
                      HTML("<br>"),
                      actionButton("filter","Filter data",
                                   icon = icon("cut"),
                                   style="background-color:#337AB7;color: white;"),
                      HTML("<br><br>"),
                      verbatimTextOutput("correctionFeedback",placeholder=FALSE),
                      
                      # --------------------------------------------------------------
                      # 3.1.3. Interpolation
                      # --------------------------------------------------------------
                      
                      hr(),
                      h3("Interpolation",style="font-weight: bold"),
                      HTML("Interpolation is necessary procedure to perform frequency-domain analyses, 
                            which assume equidistant samplings (usually 4Hz).<br><br>"),
                      radioButtons("which", "Signal to be interpolated",
                                  choices=c("Original signal",
                                            "Corrected signal"),
                                  selected="Corrected signal"),
                      HTML("<br>"),
                      sliderInput(inputId="freqhr",label="Sampling frequency (Hz)",
                                  value=4,min=0,max=8,step=1),
                      radioButtons("method", "Interpolation method",
                                  choices=c("spline",
                                            "linear"),
                                  selected="spline"),
                      # HTML("<br>"),
                      # actionButton("interpolate","Interpolate",
                      #              icon = icon("drafting-compass"),
                      #              style="background-color:#337AB7;color: white;"),
                      
                      # --------------------------------------------------------------
                      # 3.1.4. Download
                      # --------------------------------------------------------------
                      
                      hr(),
                      h3("Download dataset",style="font-weight: bold"),
                      
                      HTML("Specify the file format in the file name (e.g., 'data.csv')<br><br>"),
                      downloadButton("download","Download",
                                     style="background-color:#337AB7;color: white;"),
                      HTML("<br><br><br><br><br>")
                      
                      )),
             
             # -----------------------------------------------------------------------
             # 3.2. Plotting - COLUMN 2
             # -----------------------------------------------------------------------
             
             column(width=8,offset=0,
                    div(
                      
                      h3("Original, corrected and interpolated IBIs",
                         style="font-weight: bold"),
                      HTML("IBIs time series selected in section 1 or obtained through section 2.
                            Artefacts are marked by dark red symbols."),
                      hr(),
                      conditionalPanel(condition = "input.manualEditor=='Show'",
                                       plotlyOutput("plotManualEdit",
                                                    height="450px"),
                                       hr()
                      
                      ),
                      plotOutput("plotNIHR",
                                 height="450px"),
                      plotOutput("plotNIHR.filt",
                                 height="450px"),
                      # tableOutput("HR2plot"),
                      # tableOutput("HR2plot.edited"),
                      plotOutput("plotHR",
                                 height="450px")
                      ))
             
             ), # panel 3 closing bracket
    
    # -----------------------------------------------------------------------
    # TAB 4: HRV ANALYSIS
    # -----------------------------------------------------------------------
    
    tabPanel("4. HRV analysis",
             
             # -----------------------------------------------------------------------
             # 4.1. Settings - COLUMN 1
             # -----------------------------------------------------------------------
             
             column(width=4,style="background-color:#BBCDEB;font-size:90%;",
                    div(
                      
                      # --------------------------------------------------------------
                      # 4.1.1. Instructions
                      # --------------------------------------------------------------
                      
                      h3("Istruzioni",style="font-weight: bold"),
                      HTML("In questa sezione ?? possibile svolgere le analisi
                           della variabilit?? della frequenza cardiaca (heart rate
                           variability, HRV), estraendo gli indici nel dominio del
                           tempo e della frequenza.<br><br>
                           
                           Dopo aver estratto gli indici di HRV, <b>aggiungili al dataset</b>
                           e premi il tasto <b>Reset</b> per azzerare il dataset degli IBI.
                           Quindi riparti dalla prima sezione e seleziona un nuovo intervallo.<br>"),
                      hr(),
                      
                      actionButton("showHRV"," Analisi HRV",
                                   icon = icon("heartbeat"),
                                   style="background-color:#337AB7;color: white;"),
                      HTML("<br><br>Premi il pulsante per eseguire le analisi."),
                      
                      # --------------------------------------------------------------
                      # 4.1.2. HRV datasets
                      # --------------------------------------------------------------
                      
                      hr(),
                      h3("HRV measures",
                         style="font-weight: bold"),
                      HTML("La tabella mostra i valori degli indici di HRV
                           nell'intervallo considerato."),
                      div(tableOutput("hrv"), style = "font-size:80%"),
                      
                      hr(),
                      actionButton("addHRV","Aggiungi gli indici al dataset",
                                   icon = icon("plus"),
                                   style="background-color:#337AB7;color: white;"),
                      
                      h3("HRV dataset",
                         style="font-weight: bold"),
                      HTML("La tabella mostra i valori degli indici di HRV
                           nell'intervallo considerato."),
                      div(tableOutput("HRV"), style = "font-size:80%"),
                      
                      # --------------------------------------------------------------
                      # 4.1.3. DownloadHRV
                      # --------------------------------------------------------------
                      
                      hr(),
                      h3("Salva il dataset",style="font-weight: bold"),
                      
                      HTML("Quando hai aggiunto e corretto tutti i picchi di un intervallo, 
                           salva il dataset chiamandolo:<br>
                           <i>IDpartecipante_NumeroSessione_HRV.csv</i><br><br>"),
                      downloadButton("downloadHRV","Download",
                                     style="background-color:#337AB7;color: white;"),
                      
                      hr(),
                      h3("Resetta il dataset",style="font-weight: bold"),
                      HTML("Dopo aver estratto le misure di HRV, premi questo tasto
                           per resettare il dataset degli IBI e seleziona un nuovo intervallo
                           nella prima sezione. Il valore di RMSSD e quello dell'ampiezza
                           media del segnale BVP verr?? indicato nel primo grafico (recording plot)!<br><br>"),
                      actionButton("reset","Resetta il dataset",
                                   icon=icon("undo"),
                                   style="background-color:#337AB7;color: white;"),
                      HTML("<br><br><br>")
                      
                      )),
             
             # -----------------------------------------------------------------------
             # 4.2. Plotting - COLUMN 2
             # -----------------------------------------------------------------------
             
             column(width=8,offset=0,
                    div(
                      
                      h3("Stima della densit?? dello spettro di frequenza",
                         style="font-weight: bold"),
                      HTML("La potenza associata agli spettri di frequenza
                           nella serie temporale degli IBI, stimata con la
                           Fast Fourier Transform."),
                      hr(),
                      plotOutput("powerplot",
                                 height="350px"),
                      
                      plotOutput("plotHR2",
                                 height="300px"),
                      plotOutput("IBIdist",
                                 height="300px")
                      
                    ))
                      
      # ------------------------------------------------------------------------------
      # CLOSING BRACKETS
      # ------------------------------------------------------------------------------
      ) # panel 4
    ) # tabset panel
)) # fluid page and ui
  
             
###################################################################################### SERVER

server <- function(input, output){
  
  # --------------------------------------------------------------
  # Loading and recoding
  # --------------------------------------------------------------
  
  timeGAP <- 2*60*60 # depending on daylight time (affecting only the recording plot)
  timeGAP.acc <- timeGAP - 1*60*60 # acceleration has a different time zone (?)
  
  # loading data
  options(shiny.maxRequestSize=30*1024^2) # setting max file size to 30 MB
  bvp <- reactive({
    if(input$datafileFormat=="BVP Empatica"){
      
      # loading .zip and selecting the BVP file
      recording <- read.empatica(input$datafile$datapath)
      e4start <- as.POSIXct(recording$properties$time.start,format="%Y-%m-%d %H:%M:%S") # start time
      options(digits.secs=3) # to have resolution in milliseconds
      bvp <- as.data.frame(recording$signal$bvp)[,1:2]
      bvp$time <- e4start+bvp$t
      bvp$dataInv <- bvp$data*-1 # inverted bvp data (easier to check)
      
      # loading ibi file
    } else if(input$datafileFormat=="IBI"){
     
      tryCatch(
        {
          ibi <- read.csv(input$datafile$datapath,
                          header=input$header,
                          sep=input$sep,
                          quote=input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      # ibi in milliseconds
      if(input$DATAcol<=ncol(ibi)){
        colnames(ibi)[input$DATAcol] = "IBI"
        if(input$DATAunit=="s"){
          ibi$IBI <- ibi$IBI*1000
        }
      }
      
      # time in seconds
      if(!is.null(input$TIMEcol) & input$TIMEcol<=ncol(ibi) & input$TIMEcol!=input$DATAcol){
        colnames(ibi)[input$TIMEcol] = "t"
        if(input$TIMEunit=="ms"){ibi$IBI <- ibi$IBI/1000}
        
        if(input$NOTEcol!=0 & input$NOTEcol<=ncol(ibi) & input$NOTEcol!=input$DATAcol & input$NOTEcol!=input$TIMEcol){
          colnames(ibi)[input$NOTEcol] = "notes"
          ibi <- ibi[,c("t","IBI","notes")] # structure: IBI | t | (notes)
          }else{
          ibi <- ibi[,c("t","IBI")]
          }
        }else{
          ibi$t <- NA
          ibi[1,2] <- ibi[1,1]/1000
          for(i in 2:nrow(ibi)){
            ibi[i,2] <- ibi[i-1,2] + ibi[i,1]/1000
            }
          ibi <- ibi[,c("t","IBI")]
          }
          
      bvp <- ibi
       
    } else {
      
      bvp <- read.csv(input$datafile$datapath,header=input$header,sep=input$sep)
      # data column
      if(input$DATAcol<=ncol(bvp)){
        colnames(bvp)[input$DATAcol] = "data"
      }
      if(input$TIMEcol<=ncol(bvp)&input$TIMEcol>0){
        colnames(bvp)[input$DATAcol] = "t"
      }else{
        Step <- 1/input$SamplingRate
        bvp$t <- seq(0+Step,(nrow(bvp)/input$SamplingRate),Step)
        bvp <- bvp[seq(1,nrow(bvp),round(input$SamplingRate/64,0)),]
        bvp$time <- bvp$t
      }
      bvp$dataInv <- bvp$data*-1 # inverted bvp data (easier to check)
      
    }
    bvp
  })
  
  # events
  events <- reactive({
    req(input$datafile,input$datafileFormat=="BVP Empatica")
    recording <- read.empatica(input$datafile$datapath)
    # add start & stop time to events
    events <- rbind(c("start",NA,as.character(as.POSIXct(recording$properties$time.start,format="%H:%M:%OS")),0),
                    recording$events,c("stop",NA,as.character(as.POSIXct(recording$properties$time.stop,format="%Y-%m-%d %H:%M:%S")),0))
    # in case of no events:
    if(nrow(recording$events)==0){ colnames(events) <- c("id","time_raw","timestamp","timedelta") }
    events })
  
  # --------------------------------------------------------------
  # Zooming
  # --------------------------------------------------------------
  
  brushmin <- reactive({
    req(input$datafile)
    if(!is.null(input$brush)){
      as.POSIXct(input$brush$xmin,
                 origin="1970-01-01 00:00:00",
                 format="%Y-%m-%d %H:%M:%S") + input$addsec_min
    }
  })
  
  brushmax <- reactive({
    req(input$datafile)
    if(!is.null(input$brush)){
      as.POSIXct(input$brush$xmax,
                 origin="1970-01-01 00:00:00",
                 format="%Y-%m-%d %H:%M:%S") + input$addsec_max
    }
  })
  
  # Selecting signal to consider
  bvpPlot <- reactive({
    
    # time shift between epochs is set to 7, so that if input$width is slightly higher there is some OVERLAP
    bvpPlot <- bvp()[which.closest(bvp()$time,
                                   brushmin()+(input$epoch*6.875-6.875)):which.closest(bvp()$time,
                                                                               brushmin()+(input$epoch*6.875-6.875)+input$width),]
    bvpPlot
  })
  
  # --------------------------------------------------------------
  # Peaks detection (add & delete, mark artefacts)                 !!!
  # --------------------------------------------------------------
  
  # dataframe of ibi
  values <- reactiveValues()
  values$ibi <- data.frame(t=NULL,IBI=NULL,BVPamp=NULL,artefact=NULL) 
  
  # assign uploaded ibi to values$ibi if IBIs are imported
  IBIassignment <- observe({
    req(input$datafile,input$datafileFormat=="IBI")
    ibi <- bvp()
    ibi$artefact <- NA
    isolate(values$ibi <- ibi)
  })
  
  # IBI detection function
  find.peaks <- function (x, nups = 1, ndowns = nups, zero = "0", peakpat = NULL, 
                          minpeakheight = -Inf, maxpeakheight = Inf, minpeakdistance = 1, threshold = 0, 
                          npeaks = 0, sortstr = FALSE){
    stopifnot(is.vector(x, mode = "numeric") || length(is.na(x)) == 
                0)
    if (!zero %in% c("0", "+", "-")) 
      stop("Argument 'zero' can only be '0', '+', or '-'.")
    xc <- paste(as.character(sign(diff(x))), collapse = "")
    xc <- gsub("1", "+", gsub("-1", "-", xc))
    if (zero != "0") 
      xc <- gsub("0", zero, xc)
    if (is.null(peakpat)) {
      peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
    }
    rc <- gregexpr(peakpat, xc)[[1]]
    if (rc[1] < 0) 
      return(NULL)
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1:n) {
      xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
      xv[i] <- x[xp[i]]
    }
    inds <- which(xv >= minpeakheight & xv <= maxpeakheight & xv - pmax(x[x1], x[x2]) >= 
                    threshold)
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
    if (minpeakdistance < 1) 
      warning("Handling 'minpeakdistance < 1' is logically not possible.")
    if (sortstr || minpeakdistance > 1) {
      sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
      X <- X[sl, , drop = FALSE]
    }
    if (length(X) == 0) 
      return(c())
    if (minpeakdistance > 1) {
      no_peaks <- nrow(X)
      badpeaks <- rep(FALSE, no_peaks)
      for (i in 1:no_peaks) {
        ipos <- X[i, 2]
        if (!badpeaks[i]) {
          dpos <- abs(ipos - X[, 2])
          badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
        }
      }
      X <- X[!badpeaks, , drop = FALSE]
    }
    if (npeaks > 0 && npeaks < nrow(X)) {
      X <- X[1:npeaks, , drop = FALSE]
    }
    return(X)
  }
  # IBI detection
  # Only showed epoch
  peaks <- reactive({
    # peaks detection
    peaks <- find.peaks(bvpPlot()$dataInv,
                       minpeakheight=input$minPeak,
                       maxpeakheight=input$maxPeak,
                       minpeakdistance=input$minDist*64,
                       zero="+",
                       npeaks=Inf)
    peaks <- peaks[order(peaks[,2]),] # sorting peaks by col2 values
    Peaks <- as.data.frame(peaks)
    peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating IBI column
    for(i in 1:nrow(peaks)-1){
      peaks[i,1] <- ((peaks[i+1,3]-peaks[i,3])/64) # calculate IBIs in seconds
    }
    sec <- input$epoch*440/64-440/64 # sec from 0 of the first IBI
    peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating TIME column
    for(i in 1:nrow(peaks)){
      peaks[i,1] <- (sec+peaks[i,4]/64) # calculate sec from zero
    }
    colnames(peaks) <- c("time","IBI","y","x","start","end")
    peaks <- as.data.frame(peaks)
    
    # bvp amplitude
    sistPeaks <- find.peaks(bvpPlot()$data,
                        minpeakheight=input$minPeak,
                        maxpeakheight=input$maxPeak,
                        minpeakdistance=input$minDist*64,
                        zero="+",
                        npeaks=Inf)
    sistPeaks <- as.data.frame(sistPeaks[order(sistPeaks[,2]),1:3])
    # checking time synchr between systolic and diastolic peaks
    repeat{
      if(sistPeaks[1,2]<Peaks[1,2]){
        sistPeaks <- sistPeaks[2:nrow(sistPeaks),]
      }else{
        break
      }
    }
    repeat{
      if(nrow(sistPeaks)<nrow(Peaks)){
        sistPeaks <- rbind(sistPeaks,rep(NA,3))
      }else{
        break
      }
    }
    repeat{
      if(nrow(sistPeaks)>nrow(Peaks)){
        sistPeaks <- sistPeaks[1:(nrow(sistPeaks)-1),]
      }else{
        break
      }
    }
    colnames(sistPeaks) <- c("sistPeaks","V3","V2")
    Peaks$sistPeaks <- sistPeaks$sistPeaks
    # BVP amplitude is the sum of abs values of data and dataInv peaks
    peaks$BVPampl <- peaks$y + Peaks$sistPeaks 
    
    peaks <- peaks[,c(1,2,7,3:6)] # t, IBI, bvpAmp, y, x, start, end
    colnames(peaks)[1:3] <- c("t","IBI","BVPamp")
    peaks
    
  })
  # Whole interval
  PEAKS <- reactive({
    
    # selecting whole interval
    bvpInterval <- bvp()[which.closest(bvp()$time,
                                       brushmin()):which.closest(bvp()$time,
                                                                         brushmax()),]
    # peaks detection
    peaks <- find.peaks(bvpInterval$dataInv,
                        minpeakheight=input$minPeak,
                        maxpeakheight=input$maxPeak,
                        minpeakdistance=input$minDist*64,
                        zero="+",
                        npeaks=Inf)
    if(is.vector(peaks[,2])){
      peaks <- as.data.frame(peaks[order(peaks[,2]),]) # sorting peaks by col2 values
      Peaks <- as.data.frame(peaks)
      peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating IBI column
      for(i in 1:nrow(peaks)-1){
        peaks[i,1] <- ((peaks[i+1,3]-peaks[i,3])/64) # calculate IBIs in seconds
      }
      peaks <- cbind(rep(1,NROW(peaks)),peaks) # creating TIME column
      sec <- 1*440/64-440/64 # sec from 0 of the first IBI
      for(i in 1:nrow(peaks)){
        peaks[i,1] <- (sec+peaks[i,4]/64) # calculate sec from zero
      }
      colnames(peaks) <- c("time","IBI","y","x","start","end")
      peaks <- as.data.frame(peaks)
      
      # bvp amplitude
      sistPeaks <- find.peaks(bvpInterval$data,
                              minpeakheight=input$minPeak,
                              maxpeakheight=input$maxPeak,
                              minpeakdistance=input$minDist*64,
                              zero="+",
                              npeaks=Inf)
      sistPeaks <- as.data.frame(sistPeaks[order(sistPeaks[,2]),1:3])
      # checking time synchr between systolic and diastolic peaks
      repeat{
        if(sistPeaks[1,2]<Peaks[1,2]){
          sistPeaks <- sistPeaks[2:nrow(sistPeaks),]
        }else{
          break
        }
      }
      repeat{
        if(nrow(sistPeaks)<nrow(Peaks)){
          sistPeaks <- rbind(sistPeaks,rep(NA,3))
        }else{
          break
        }
      }
      repeat{
        if(nrow(sistPeaks)>nrow(Peaks)){
          sistPeaks <- sistPeaks[1:(nrow(sistPeaks)-1),]
        }else{
          break
        }
      }
      colnames(sistPeaks) <- c("sistPeaks","V3","V2")
      Peaks$sistPeaks <- sistPeaks$sistPeaks
      # BVP amplitude is the sum of abs values of data and dataInv peaks
      peaks$BVPampl <- peaks$y + Peaks$sistPeaks
      
      PEAKS <- peaks[,c(1,2,7,3:6)] # t, IBI, bvpAmp, y, x, start, end
      colnames(PEAKS)[1:3] <- c("t","IBI","BVPamp")
      PEAKS
    } 
  })
  
  # Add only showed IBIs
  newEntry <- observe({
    req(input$datafile,input$update>0)
    cat("newEntry\n")
    new.value <- isolate(peaks())
    newRow <- isolate(req(peaks()))
    isolate(values$ibi <- rbind(values$ibi,
                                data.frame(peaks()[1:nrow(peaks())-1,1:3],
                                      artefact=rep(NA,nrow(peaks())-1))))
  })
  
  # Add whole interval    
  allIN <- observe({
    req(input$datafile,input$addAll>0)
    cat("allIN\n")
    # selecting the whole interval
    new.value <- isolate(PEAKS())
    newRow <- isolate(req(PEAKS()))
    isolate(values$ibi <- rbind(values$ibi,
                                cbind(PEAKS()[1:nrow(PEAKS())-1,1:3],
                                      artefact=rep(NA,nrow(PEAKS())-1))))
    
  })
  
  # one step back (removing nrow(peaks) from IBI dataset)
  back.1.step <- observe({
    cat("back.1.step\n")
    if(input$back > 0) {
      values$ibi <- isolate(values$ibi[-((nrow(values$ibi)-(nrow(peaks())-2)):nrow(values$ibi)),])
      
    }
  })
  
  markArtefact <- observeEvent(input$markArtefact,{
    req(input$datafile)
    cat("mark Artefact\n")
    if(nrow(values$ibi[!is.na(values$ibi),])){
      isolate(values$ibi[values$ibi$t==peaks()[rownames(peaks())==input$artefact,1],4] <- "MARKED")
    } 
  })
  
  demarkArtefact <- observeEvent(input$demarkArtefact,{
    req(input$datafile)
    cat("demark Artefact\n")
    if(nrow(values$ibi[!is.na(values$ibi),])){
      isolate(values$ibi[values$ibi$t==peaks()[rownames(peaks())==input$artefact,1],4] <- NA)
    } 
  })

  # mark warning
  output$markWarning <- renderPrint({
    if((input$markArtefact > 0 | input$demarkArtefact > 0) & nrow(values$ibi)==0){
      cat("Prima di marcare un \nartefatto devi aggiungere \ni picchi al dataset!")
    }
  })
  
  # delete specific IBI
  deletePeak <- observe({
    cat("deletePeak\n")
    if(input$delete > 0) {
      values$ibi <- isolate(values$ibi[-input$row.selection,])
    }
  })
  
  # RESET (delete all IBIs from the dataset)
  reset <- observe({
    cat("reset\n")
    if(input$reset > 0) {
      values$ibi <- isolate(data.frame(t=NULL,IBI=NULL))
    }
  })
  
  # --------------------------------------------------------------
  ################################################################ TAB 1
  # --------------------------------------------------------------
  # Tab 1: reporting values (interval timestamps and events)
  # --------------------------------------------------------------
  
  # upper interval
  output$brush_output_max <- renderText({
    req(input$datafile)
    if(!is.null(input$brush)){
      substr(as.POSIXct(brushmax(),
                        origin="1970-01-01 00:00:00",
                        format="%Y-%m-%d %H:%M:%S"),
             start=12,stop=19)
    }
  })
  
  # lower interval (showing 5 secs less because the last 5 secs will probably show artifacts due to bu)
  output$brush_output_min <- renderText({
    req(input$datafile)
    if(!is.null(input$brush)){
      substr(as.POSIXct(brushmin(),
                        origin="1970-01-01 00:00:00",
                        format="%Y-%m-%d %H:%M:%S"),
             start=12,stop=19)
    }
  })
  
  # recoding date
  output$brush_output_date <- renderText({
    req(input$datafile)
      substr(as.POSIXct(events()[1,3],
                        origin="1970-01-01 00:00:00",
                        format="%Y-%m-%d %H:%M:%S"),
             start=1,stop=10)
  })
  
  # events markers table
  output$events <- renderTable({
    req(input$datafile,input$brush)
    events <- data.frame(event=events()$id,
                         time=substr(as.POSIXct(events()$timestamp,
                                                  origin="1970-01-01 00:00:00",
                                                  format="%Y-%m-%d %H:%M:%S"),
                                       start=12,stop=19))
    # this part is temporarily disabled to work even with recordings without event marks
    # events$diff <- NA
    # events$marker <- NA
    # for(i in 2:(nrow(events)-1)){
    #   events[i,3] <- difftime(as.POSIXct(events()[i,3],
    #                                      origin="1970-01-01 00:00:00",
    #                                      format="%Y-%m-%d %H:%M:%S"),
    #                           as.POSIXct(events()[i-1,3],
    #                                      origin="1970-01-01 00:00:00",
    #                                      format="%Y-%m-%d %H:%M:%S"))
    # 
    #   if(events[i,3]<4&events[i,3]>2){
    #     events[i,4] <- "SUP"
    #   }
    # }
    # events$check <- NA
    # for(i in 1:nrow(events)){
    #   if(events[i,2]==substr(as.POSIXct(brushmax(),
    #                                     origin="1970-01-01 00:00:00",
    #                                     format="%Y-%m-%d %H:%M:%S"),
    #                          start=12,stop=19)){
    #     events[i,5] <- "OK"
    #   }
    # }
    # events <- events[,c(1,2,4,5)]
    # events[is.na(events$marker),3] <- as.character("")
    # events[is.na(events$check),4] <- as.character("")
    data.table(events)
  })
  
  # dataset head
  output$head <- renderTable({
    req(input$datafile)
    if(input$disp=="Head"){
      data.table(head(bvp()))
    }
  })
  
  # --------------------------------------------------------------
  # Tab 1: Plotting
  # --------------------------------------------------------------
  
  # plotting whole recording
  output$recording <- renderPlot({
  req(input$datafile)

  if(input$datafileFormat!="IBI"){

    bvpPlot.dwns <- bvp()[seq(1,nrow(bvp()),32),]
    # bvpPlot.dwns <- bvp()
    if(input$portion=="First part"){
      p <- ggplot(bvpPlot.dwns[640:(nrow(bvpPlot.dwns)/2),],
                  aes(time,data))

    } else if(input$portion=="Second part"){
      p <- ggplot(bvpPlot.dwns[(nrow(bvpPlot.dwns)/2):nrow(bvpPlot.dwns),],
                  aes(time,data))
    }else{
      p <- ggplot(bvpPlot.dwns[640:nrow(bvpPlot.dwns),],
                  aes(time,data))
    }
    p <- p  + geom_line()
      
      if((input$datafileFormat=="BVP Empatica" | input$NOTEcol!=0) & nrow(events())>2){
        p <- p + geom_vline(xintercept=events()[,3],colour="red") }
      p <- p + labs(x="time",y="BVP") +
      theme(axis.title=element_text(size=17),
            axis.text=element_text(size=15))
    
    p

  }

  })
  
  # plotting the option zoomed
  output$zoom <- renderPlot({
    req(input$datafile,input$brush)
    
    if(input$datafileFormat=="IBI"){
      
      ggplot(bvp(),aes(t,6000/IBI))+
        geom_line(colour="red")+
        geom_point(colour="red")+
        ggtitle("HR time series")
      
    }else{
      
      bvpPlot.dwns <- bvp()[seq(1,nrow(bvp()),32),]
      p <- ggplot(bvpPlot.dwns,
                  aes(time,data)) + 
        geom_line() 
      if((input$datafileFormat=="BVP Empatica" | input$NOTEcol!=0) & nrow(events())>2){
        p <- p + geom_vline(xintercept=events()[,3],colour="red") }
      # zooming
      if(!is.null(input$brush)){
        p <- p + xlim(brushmin(),brushmax()) # ZOOMING
        bvp.zoom <- bvpPlot.dwns[which.closest(bvpPlot.dwns$time,
                                               brushmin()):which.closest(bvpPlot.dwns$time,
                                                                                 brushmax()),]
        p <- p + ylim(min(bvp.zoom$data),
                      max(bvp.zoom$data))
      }
      p + labs(x="time",y="BVP") +
        theme(axis.title=element_text(size=17),
              axis.text=element_text(size=15))
      
    }
    
    
  })
  
  # plotting ibi
  output$ibiPlot <- renderPlot({
    req(input$datafile)
    if(input$datafileFormat=="IBI"){
      if(input$plotMode=="IBI"){
        ggplot(bvp(),aes(t,IBI))+
          geom_line(colour="red")+
          geom_point(colour="red")+
          labs(x="time (sec)",y="IBI (ms)")+
          theme(text = element_text(size=20))
      } else {
        ggplot(bvp(),aes(t,60000/IBI))+
          geom_line(colour="red")+
          geom_point(colour="red")+
          labs(x="time (sec)",y="HR (bpm)")+
          theme(text = element_text(size=20))
      }
    }
  })
  
  # --------------------------------------------------------------
  ################################################################ TAB 2
  # --------------------------------------------------------------
  # Tab 2: reporting values (nIBI, gaps, HR and datasets)
  # --------------------------------------------------------------
  
  # Display number of added peaks
  output$feedback <- renderText({
    paste("Total number of added peaks =",nrow(values$ibi[!is.na(values$ibi$t),]))
  })
  
  # Display time-domain measures
  output$feedback.hrv <- renderText({
    req(nrow(values$ibi)>0)
    ibi <- values$ibi
    colnames(ibi)[4] <- "artefact"
    ibi <- ibi[is.na(ibi$artefact),] # only unmarked IBIs
    if(input$detectionMode=="IBI"){
      paste("SDNN =",
            round(sd(ibi$IBI*1000),2),
            "ms, RMSSD =",
            round(sqrt(mean(diff(ibi$IBI*1000)^2)),2),
            "ms")
    } else {
      paste("SDNN =",
            round(sd(na.omit(ibi$BVPamp)),2),
            "AU, RMSSD =",
            round(sqrt(mean(diff(na.omit(ibi$BVPamp))^2)),2),
            "AU\n\nNote: in BVP mode these are \nreferred to BVPamp")
        }
    
  })
  
  # Display HR related to minDist
  output$HR <- renderText({
    paste("*Means that max HR =",round(1/input$minDist*60,0))
  })

  # displaying peaks dataset
  output$peaks <- renderTable({
    req(input$datafile)
    if(input$detectionMode=="IBI"){
      if(nrow(values$ibi[!is.na(values$ibi$t),])==0){
        residuals <- peaks()[,2]-mean(peaks()[,2]) # IBI residuals
      }else{
        hrv.artefactFree <- values$ibi[is.na(values$ibi$artefact),]
        residuals <- peaks()[,2]-mean(hrv.artefactFree$IBI) # IBI residuals
      }
      peaks_res <- cbind(peaks()[,1:2],residuals) # IBI residuals
      
      
      colnames(peaks_res) <- c("time","IBI","RES")
      peak <- as.integer(seq(1,nrow(peaks_res),1))
      HR <- as.integer(round(60/peaks_res$IBI,0))
      data.table(peak,peaks_res,HR)
      
    } else {
      if(nrow(values$ibi[!is.na(values$ibi$t),])==0){
        residuals <- peaks()[,3]-mean(na.omit(peaks()[,3])) # BVP residuals
      }else{
        hrv.artefactFree <- values$ibi[is.na(values$ibi$artefact),]
        residuals <- peaks()[,3]-mean(hrv.artefactFree$BVPamp) # BVP residuals
      }
      peaks_res <- cbind(peaks()[,c(1:3)],residuals) # BVP residuals
      
      
      colnames(peaks_res) <- c("time","IBI","BVPamp","RES")
      peak <- as.integer(seq(1,nrow(peaks_res),1))
      HR <- as.integer(round(60/peaks_res$IBI,0))
      peaks_res <- peaks_res[,c(1,3,4)]
      data.table(peak,peaks_res,HR)
    }
    
  },keep.rownames=TRUE)
  
  # preventing presence of gaps (repeated or missed peaks)
  output$gapPrevention <- renderPrint({
    req(input$datafile)
    # finding peaks
    peaks <- find.peaks(as.vector(bvpPlot()$dataInv),
                        minpeakheight=input$minPeak,
                        maxpeakheight=input$maxPeak,
                        minpeakdistance=input$minDist*64,
                        zero="+",
                        npeaks=Inf)
    peaks <- as.data.frame(peaks[order(peaks[,2]),])

    if(input$width>=0){
      epochWidthGap <- input$width - 7.8
    }else{
      epochWidthGap <- input$width + 7.8
    }
    if(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5<peaks[nrow(peaks)-1,2]){
      cat("WARNING!\nDECREASE epoch width. The red area should include ONLY ONE peak.")
    }
    # gap warning (double peaks)
    if(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5>=peaks[nrow(peaks),2]){
      cat("WARNING!\nINCREASE epoch width. The red area should include AT LEAST ONE peak.")
    }
  })

  # segnaling presence of gaps
  output$gapWarning <- renderPrint({
    req((input$update>0|input$addAll>0))
    if(nrow(values$ibi[!is.na(values$ibi$t),])>0){
      gap <- cbind(values$ibi[,c(1,2)],rep(1,NROW(values$ibi))) # creating "t2-(t1+d1)" column
      colnames(gap) <- c("time","IBI","GAP")
      for(i in 1:nrow(gap)){
        gap[i,3] <- (gap[i+1,1]-(gap[i,1]+gap[i,2]))
      }
      for(i in 1:nrow(na.omit(gap))){
        if(gap[i,3]!=0){
          if(gap[i,1]==gap[i+1,1]){
            cat(paste("WARNING!\npeak number",i,"has been added twice. Delete it using te button below the IBI dataset."))
          } else {
            cat(paste("Ops! Something strange hapenned. Check the IBI dataset.",
                      sep=""))
          }
        }
      }
    }else{
      cat("The IBI dastaset is empty.")
    }
    
  })

  # displaying IBI dataset
  output$dataset <- renderTable({
    req(input$datafile,nrow(values$ibi[!is.na(values$ibi$t),])>0)
    ibi <- values$ibi[!is.na(values$ibi$t),]
    ibi <- cbind(as.integer(seq(1,nrow(ibi),1)),ibi)
    if(input$datafileFormat=="IBI"){
      ibi$artefact <- NA
      ibi <- colnames(ibi) <- c("peak","time","IBI","artefacts")
    }else{
      colnames(ibi) <- c("peak","time","IBI","ampl","artefact")
      
    }
    data.table(ibi,keep.rownames=FALSE)
    
  })
  
  # ------------------------------------------------------------
  # Tab 2: plotting
  # ------------------------------------------------------------
  
  # Interval plot 2 (plotting the whole recording)
  output$zoom2 <- renderPlot({
    req(input$datafile)
    req(input$brush)
    par(mai=c(0,0,0,0),mar=c(0,0,0,0),bg="#BBCDEB")
    plot(bvp()$data,
         xlim=c(as.numeric(row.names(bvp()[which.closest(bvp()$time,
                                                         brushmin()),])),
                as.numeric(row.names(bvp()[which.closest(bvp()$time,
                                                         brushmax()),]))),
         ylim=c(-100,100),
         xaxt="n",yaxt="n",xlab="",ylab="",type="l",
         xaxs="i",yaxs="i")
    # update feedback
    if(nrow(values$ibi[!is.na(values$ibi$t),])>0){
    rect(as.numeric(row.names(bvp()[which.closest(bvp()$time,
                                                  brushmin()),])),
         -100,
         as.numeric(row.names(bvp()[which.closest(bvp()$time,
                                                  brushmin()+max(values$ibi$t)),])),
         100,
         col="#E5FFE5",border=NA)
    }
    # selected epoch
    rect(as.numeric(row.names(bvp()[which.closest(bvp()$time,min(bvpPlot()$time)),])),
         -100,
         as.numeric(row.names(bvp()[which.closest(bvp()$time,max(bvpPlot()$time)),])),
         100,
         col=rgb(1,1,0,alpha=0.3))
    
    # adding acceleration
    if(substr(input$datafile$datapath,
              start=nchar(input$datafile$datapath)-3,
              stop=nchar(input$datafile$datapath))==".zip"){
      recording <- read.empatica(input$datafile$datapath)
      e4start <- as.POSIXct(recording$properties$time.start,format="%Y-%m-%d %H:%M:%S") # start time
      acc <- data.frame(t=recording$signal$acc_x$t,
                        x=recording$signal$acc_x$data* 2/128, # expressed in g
                        y=recording$signal$acc_y$data* 2/128,
                        z=recording$signal$acc_z$data* 2/128)
      # total acceleration
      acc$tot <- sqrt(acc$x^2 + acc$y^2 +acc$z^2)
      # Empatica Way
      acc$totAcc <- pmax(abs(acc$x-c(NA,acc[1:(nrow(acc)-1),2])),
                         abs(acc$y-c(NA,acc[1:(nrow(acc)-1),3])),
                         abs(acc$z-c(NA,acc[1:(nrow(acc)-1),4])))
      acc$time <- e4start+acc$t
      lines(acc[rep(seq_len(which.closest(acc$time,
                                          brushmin()):which.closest(acc$time,
                                                                            brushmax())),each=2),6]*1000-100,
            col=rgb(0,0,1,alpha=0.3),lwd=7)
    }
    lines(bvp()$data,lwd=2)
    
  },height = 100)

  # plotting peaks 2
  output$window2 <- renderPlot({
    req(input$datafile,input$brush)
    
    # ---------------------------------------------------------
    # plotting bvp
    # ---------------------------------------------------------
    
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    plot(bvpPlot()$dataInv,
         ylab="BVP",xlab="TIME (sec)",xaxt="n",type="l",
         ylim=c(min(bvpPlot()$dataInv)-10,
                max(bvpPlot()$dataInv)+10))
    # Seconds from start in the X axis
    axis(1,at=seq(0,512,64),
         labels=round(seq(input$epoch*440/64-440/64,
                          input$epoch*440/64-440/64 + 512/64,
                          1),
                      1))
    # seconds
    abline(v=seq(0,512,64),lty=2,col="gray")

    # finding peaks
    peaks <- find.peaks(as.vector(bvpPlot()$dataInv),
                        minpeakheight=input$minPeak,
                        maxpeakheight=input$maxPeak,
                        minpeakdistance=input$minDist*64,
                        zero="+",
                        npeaks=Inf)
    peaks <- as.data.frame(peaks[order(peaks[,2]),])
    
    # ---------------------------------------------------------
    # gap warning
    # ---------------------------------------------------------
    
    if(input$width>=0){
      epochWidthGap <- input$width - 7.8
    }else{
      epochWidthGap <- input$width + 7.8
    }
    # last second in yellow
    rect(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5,
         min(bvpPlot()$dataInv)-10,
         nrow(bvpPlot()) + 5.5,
         max(bvpPlot()$dataInv)+10,
         col=rgb(0,1,0,alpha=0.1),border=NA)
    if(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5<peaks[nrow(peaks)-1,2]){
      rect(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5,
           min(bvpPlot()$dataInv)-10,
           nrow(bvpPlot())+5.5,
           max(bvpPlot()$dataInv)+10,
           col=rgb(1, 0, 0.2,alpha=0.4),border=NA)
    }
    # gap warning (double peaks)
    if(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5>=peaks[nrow(peaks),2]){
      rect(nrow(bvpPlot()) - epochWidthGap*64 - 64+5.5,
           min(bvpPlot()$dataInv)-10,
           nrow(bvpPlot())+5.5,
           max(bvpPlot()$dataInv)+10,
           col=rgb(1, 0, 0.2,alpha=0.4),border=NA)
    }
    
    # ---------------------------------------------------------
    # showing bvp amplitude
    # ---------------------------------------------------------
    
    sistPeaks <- find.peaks(as.vector(bvpPlot()$data),
                        minpeakheight=input$minPeak,
                        maxpeakheight=input$maxPeak,
                        minpeakdistance=input$minDist*64,
                        zero="+",
                        npeaks=Inf)
    sistPeaks <- as.data.frame(sistPeaks[order(sistPeaks[,2]),1:3])
    repeat{
      if(sistPeaks[1,2]<peaks[1,2]){
        sistPeaks <- sistPeaks[2:nrow(sistPeaks),]
      }else{
        break
      }
    }
    repeat{
      if(nrow(sistPeaks)<nrow(peaks)){
        sistPeaks <- rbind(sistPeaks,rep(NA,3))
      }else{
        break
      }
    }
    repeat{
      if(nrow(sistPeaks)>nrow(peaks)){
        sistPeaks <- sistPeaks[1:(nrow(sistPeaks)-1),]
      }else{
        break
      }
    }
    
    colnames(sistPeaks) <- c("sistPeaks","V3","V2")
    peaks$sistPeaks <- sistPeaks$sistPeaks
    
    # showing bvp amplitude
    for(i in 1:nrow(peaks)){
      lines(x=c(peaks[i,2],peaks[i,2]),
            y=c(peaks[i,1],-peaks[i,5]),
            col="green",lty=2,lwd=1.5)
      lines(x=c(peaks[i,2],peaks[i,4]),
            y=c(-peaks[i,5],-peaks[i,5]),
            col="gray",lty=2)
    }
    
    # ---------------------------------------------------------
    # showing peaks on graph
    # ---------------------------------------------------------
    
    points(peaks[,2],peaks[,1],pch=20,col="red",cex=2)
    text(peaks[,2]+5,peaks[,1]+5,cex= 0.8)
    lines(peaks[,2],peaks[,1],lty=2,col="red")
    
    # showing systolic peaks
    points(peaks[,2],-peaks[,5],pch=20,col="green",cex=2)
    
    # min and max peaks height
    abline(h=input$minPeak,lwd=2,col="blue")
    abline(h=input$maxPeak,lwd=2,col="purple")
    
    # ---------------------------------------------------------
    # showing potential artefact
    # ---------------------------------------------------------
    if(nrow(values$ibi[!is.na(values$ibi$t),])>0){
      ibi <- values$ibi
      for(i in 1:nrow(ibi)){
        if(!is.na(ibi[i,4])){
          points(peaks[peaks$V2/64+(input$epoch*6.875-6.875)==ibi[i,1],2],
                 peaks[peaks$V2/64+(input$epoch*6.875-6.875)==ibi[i,1],1],
                 pch=10,col="darkred",cex=5,lwd=2)
        }
      }
    }
    })
  
  # plotting IBI serie
  output$cardio <- renderPlot({
    
    req(input$datafile,nrow(values$ibi)>0)
    ibi <- values$ibi
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    
    if(input$plotMode2=="IBI"){
      plot(values$ibi$t,
           values$ibi$IBI*1000,
           type="b",lwd=1,pch=20,col="red",
           ylab="IBI (ms)",xlab="time (sec)")
      abline(v=seq(0,max(values$ibi$t),5),lty=3,col="gray")
      abline(v=seq(0,max(values$ibi$t),10),lty=2,col="darkgray")
      
      # showing artefacts
      for(i in 1:nrow(ibi)){
        if(!is.na(ibi[i,4])){
          points(values$ibi[i,1],
                 values$ibi[i,2]*1000,
                 pch=10,col="darkred",cex=3,lwd=2)
        }
      }
      } else {
        plot(values$ibi$t,
             60/values$ibi$IBI,
             type="b",lwd=1,pch=20,col="red",
             ylab="HR (bpm)",xlab="time (sec)")
        abline(v=seq(0,max(values$ibi$t),5),lty=3,col="gray")
        abline(v=seq(0,max(values$ibi$t),10),lty=2,col="darkgray")
        
        # showing artefacts
        for(i in 1:nrow(ibi)){
          if(!is.na(ibi[i,4])){
            points(ibi[i,1],
                   60/ibi[i,2],
                   pch=10,col="darkred",cex=3,lwd=2)
          }
        }
      }
    
    
  })
  
  # plotting BVP ampl
  output$BVPamp <- renderPlot({
    
    req(input$datafile,nrow(values$ibi)>0)
      
      par(mai=c(1,1,0,1),mar=c(5,4,0,1))
      plot(values$ibi$t,
           values$ibi$BVPamp,
           type="l",lwd=1,pch=20,col="green",
           ylab="BVP amplitude (arbitrary units)",xlab="time (sec)")
      points(values$ibi$t,
             values$ibi$BVPamp,
             col="#24BD28",cex=1,pch=20)
      abline(v=seq(0,max(values$ibi$t),5),lty=3,col="gray")
      abline(v=seq(0,max(values$ibi$t),10),lty=2,col="darkgray")
      # showing artefacts
      ibi <- values$ibi
      for(i in 1:nrow(ibi)){
        if(!is.na(ibi[i,4])){
          points(values$ibi[i,1],
                 values$ibi[i,3],
                 pch=10,col="red",cex=3,lwd=2)
        }
      }
  })
  
  # plotting linear regression BVP HR
  output$HRvsBVP <- renderPlot({
    req(input$datafile,nrow(values$ibi)>0)
    par(mai=c(1,1,0,1),mar=c(5,4,0,1))
    plot(60/values$ibi$IBI,
         values$ibi$BVPamp,
         lty=20,
         xlab="HR (bpm)",ylab="BVP amplitude (arbitrary units)")
    ibi <- as.data.frame(values$ibi)
    colnames(ibi) <- c("time","IBI","BVPamp")
    abline(lm(BVPamp~IBI,data=ibi)) # regression line
    # artefact
    ibi <- values$ibi
    for(i in 1:nrow(ibi)){
        if(!is.na(ibi[i,4])){
          points(60/values$ibi[i,2],
                 values$ibi[i,3],
                 pch=10,col="darkred",cex=3,lwd=2)
        }
    }
  })
  
  # --------------------------------------------------------------
  ################################################################ TAB 3
  # --------------------------------------------------------------
  # Tab 3: Correction and interpolation
  # --------------------------------------------------------------
  
  # threshold length should be <= n.IBI
  output$longWarning <- renderPrint({
    req(input$update>0)
    if(nrow(values$ibi)<input$long){
      cat("ATTENZIONE!\nIl numero di IBI per definire la soglia deve essere minore o uguale al numero totale di IBI!")
    }
  })
  
  # delete artefacts
  hrv.artefactFree <- reactive({
    if(input$datafileFormat=="IBI"){
      hrv.artefactFree <- values$ibi
      hrv.artefactFree$IBI <- hrv.artefactFree$IBI/1000
    }else{
      # deleting all artefacts
      if(input$deleteArtefacts > 0) {
        hrv.artefactFree <- data.frame(t=NULL,IBI=NULL,BVPamp=NULL,artefact=NULL)
        for(i in 1:nrow(values$ibi)){
          if(is.na(values$ibi[i,4])){
            hrv.artefactFree <- rbind(hrv.artefactFree,values$ibi[i,])
          }
        }
      } else {
        hrv.artefactFree <- values$ibi
      }
    }
    # updating dataset based on manual editing (see section 3's plotting)
    if(is.data.frame(hrv.manuallyEdited())){
      for(i in 1:nrow(hrv.artefactFree)){
        if(round(1000*hrv.artefactFree[i,"IBI"],2)!=round(hrv.manuallyEdited()[i,"IBI"],2)){
          hrv.artefactFree[i,"IBI"] <- hrv.manuallyEdited()[i,"IBI"]/1000
          hrv.artefactFree[i,"artefact"] <- "MARKED"
        }
      }
    }
    hrv.artefactFree
  })
  
  # artefacts feedback
  output$artefactFeedback <- renderPrint({
    if(input$deleteArtefacts > 0) {
      cat("Deleted",nrow(values$ibi)-nrow(hrv.artefactFree()),"artefacts.")
    }
  })
  
  # Filtering
  hrv.data.filt <- reactive({
    
    ibi.table <- hrv.artefactFree()
    if(input$filter>0 & nrow(values$ibi)>1){
      hrv.data.filt <- data.frame(t=NULL,IBI=NULL,BVPamp=NULL,artefact=NULL)
      
      if(input$detectionMode=="IBI"){
        
        # which IBIs to be used for the averaged threshold (possibly those before the IBI under assessment)
        for(i in 1:nrow(ibi.table)){
          if(i==1){
            average <- mean(ibi.table[2:(input$long+1),2])
          }else if(i<=input$long){
            average <- mean(ibi.table[c(1:(i-1),(i+1):(input$long+1)),2])
          }else{
            average <- mean(ibi.table[(i-input$long):(i-1),2])
          }
          if(abs(ibi.table[i,2]-average)<input$last){
            hrv.data.filt <- rbind(hrv.data.filt,ibi.table[i,])
          }
        }
        # filtering by max and min HR
        hrv.data.filt <- hrv.data.filt[which(60/hrv.data.filt$IBI<input$maxbpm),] # max HR
        hrv.data.filt <- hrv.data.filt[which(60/hrv.data.filt$IBI>input$minbpm),] # min HR
        
      } else {
        
        # which IBIs to be used for the averaged threshold (possibly those before the IBI under assessment)
        for(i in 1:nrow(ibi.table)){
          if(i==1){
            average <- mean(ibi.table[2:(input$long+1),3])
          }else if(i<=input$long){
            average <- mean(ibi.table[c(1:(i-1),(i+1):(input$long+1)),3])
          }else{
            average <- mean(ibi.table[(i-input$long):(i-1),3])
          }
          if(abs(ibi.table[i,3]-average)<input$last){
            hrv.data.filt <- rbind(hrv.data.filt,ibi.table[i,])
          }
        }
        # filtering by max and min HR
        hrv.data.filt <- hrv.data.filt[which(60/hrv.data.filt$BVPamp<input$maxbpm),] # max HR
        hrv.data.filt <- hrv.data.filt[which(60/hrv.data.filt$BVPamp>input$minbpm),] # min HR
        
      }
      
    }else{
      hrv.data.filt <- hrv.artefactFree()
    }
    
    # avoiding double peaks
    for(i in 2:nrow(hrv.data.filt)){
      if(i>nrow(hrv.data.filt)){
        break
      }
      if(hrv.data.filt[i,1]==hrv.data.filt[i-1,1]){
        hrv.data.filt <- hrv.data.filt[-i,]
        i = i-1
      }
    }
    
    hrv.data.filt
  })
  output$correctionFeedback <- renderPrint({
    if(input$filter>0 & nrow(values$ibi)>1){
      cat("Deleted",nrow(hrv.artefactFree())-nrow(hrv.data.filt()),"artefacts.")
    }
  })
  
  # Interpolation
  hrv.data.inter <- reactive({
    if(input$which=="Original signal"){
      hrv.data <- values$ibi
    }else{
      hrv.data <- hrv.data.filt()
    }
    if(nrow(values$ibi)>1){
      
      # avoiding double peaks
      for(i in 2:nrow(hrv.data)){
        if(i>nrow(hrv.data)){
          break
        }
        if(hrv.data[i,1]==hrv.data[i-1,1]){
          hrv.data <- hrv.data[-i,]
          i = i-1
        }
      }
      # interpolation parameters
      freqhr = input$freqhr
      method <- input$method
      
      first = head(hrv.data$t,1)
      last = tail(hrv.data$t, 1)
      npoints = as.integer((last - first) * freqhr + 1)
      if (method == "linear") {
        interpolate = approxfun(hrv.data$t,60/hrv.data$IBI, 
                                method = "linear", ties = "ordered")
      } else {
        interpolate = splinefun(hrv.data$t,60/hrv.data$IBI,
                                method = "monoH.FC", ties = "ordered")
      }
      
      samples = seq(first, last, 1/freqhr)
      hrv.data.inter = interpolate(samples)
      limit = 30
      
      begindex = which(diff(hrv.data$t) > limit)
      beg = hrv.data$t[begindex]
      end = hrv.data$t[begindex + 1]
      if (length(begindex) > 0) {
        for (i in 1:length(beg)) {
          hrv.data.inter[samples > beg[i] & samples < end[i]] = 0
          warning("interval without beats detected!")
        }
      }
    }
    hrv.data.inter
  })
  
  # --------------------------------------------------------------
  # Tab 3: Download IBI
  # --------------------------------------------------------------
  
  output$download <- downloadHandler(
    file = paste("ID_SESSION_INTERVAL_IBI.csv",sep=""),
    content = function(file) {
      ibi.new <- hrv.data.filt()[,c(1:3)]
      ibi.new$IBI <- round(ibi.new$IBI*1000,2)
      ibi.new$t <- round(ibi.new$t,2)
      write.csv(ibi.new,file,row.names=FALSE)
    },
    contentType="text/csv"
  )
  # --------------------------------------------------------------
  # Tab 3: plotting
  # --------------------------------------------------------------
  
  HR2plot <- reactiveValues(
    x = NA,
    y = NA,
    artefact = NA
  )
  # changhing HR2plot when dots are changed manually
  changeHR2plot <- observe({
    req(input$datafile,nrow(values$ibi)>0)
    ed <- event_data("plotly_relayout")
    if(is.null(ed)){
      HR2plot$x <- hrv.data.filt()$t
      if(input$detectionMode=="IBI"){
        # if(input$datafileFormat!="IBI"){
          HR2plot$y <- 60/hrv.data.filt()$IBI
        # }else{
        #   HR2plot$y <- 60000/hrv.data.filt()$IBI
        # }
        HR2plot$artefact <- values$ibi$artefact
        # HR2plot$artefact <- hrv.data.filt()$artefact
      } else {
        # HR2plot$y <- values$ibi$BVPamp
        HR2plot$y <- hrv.data.filt()$BVPamp
      }
      
    }
  })
  
  # plotting HR2plot over original data
  output$plotManualEdit <- renderPlotly({
    req(input$datafile)
    # if(input$datafileFormat=="IBI"&nrow(values$ibi)>1){
    if(nrow(values$ibi)>1){  
      if(input$detectionMode=="IBI"){
        circles <- map2(
          HR2plot$x, HR2plot$y,
          ~list(
            type = "circle",
            # anchor circles at (mpg, wt)
            xanchor = .x,
            yanchor = .y,
            # give each circle a 2 pixel diameter
            x0 = -5, x1 = 5,
            y0 = -5, y1 = 5,
            xsizemode = "pixel", 
            ysizemode = "pixel",
            # other visual properties
            # fillcolor = "red",
            # line = list(color = "transparent")
            line=list(color="red")
            
          )
        )
      } else {
        circles <- map2(
          HR2plot$x, HR2plot$y,
          ~list(
            type = "circle",
            # anchor circles at (mpg, wt)
            xanchor = .x,
            yanchor = .y,
            # give each circle a 2 pixel diameter
            x0 = -5, x1 = 5,
            y0 = -5, y1 = 5,
            xsizemode = "pixel", 
            ysizemode = "pixel",
            # other visual properties
            # fillcolor = "red",
            # line = list(color = "transparent")
            line=list(color="green")
            
          )
        )
      }
      
      
      # ART <- data.frame(t=HR2plot$x,
      #                   IBI=HR2plot$y,
      #                   artefact=HR2plot$artefact)
      # ART <- ART[!is.na(ART$artefact),]
      
      # original and corrected IBIs
      ibi_original <- values$ibi
      if(input$datafileFormat=="IBI"){
        ibi_original$IBI <- ibi_original$IBI/1000
      }
      ibi_corrected <- HR2plot
      
      # plot the shapes and fitted line
      if(input$detectionMode=="IBI"){    # IBI mode
        p <- plot_ly() %>%
          # plotting original signal (dashed black line)
          # add_markers(x = ibi$t, y = 60/ibi$IBI,
          add_markers(x = ibi_original$t, y = 60/ibi_original$IBI,
                      name='Original data',color=I("black"),
                      line=list(dash='dash'),
                      showlegend=FALSE) %>%
          # legend for original signal
          # add_lines(x = ibi$t, y = 60/ibi$IBI,color=I("black"),
          add_lines(x = ibi_original$t, y = 60/ibi_original$IBI,color=I("black"),
                    name=paste('Original data\nRMSSD = ',
                               # round(sqrt(mean(diff(ibi$IBI*1000)^2)),2)," ms",
                               round(sqrt(mean(diff(ibi_original$IBI*1000)^2)),2)," ms",
                               sep="")) %>%
          #
          # second y axis
          add_lines(x = ibi_corrected$x, y = ibi_corrected$y,color=I("transparent"),
                    showlegend=FALSE,yaxis="y2") %>%
          add_lines(x = ibi_corrected$x, y = ibi_corrected$y, color = I("red"),
                    name=paste("Corrected data\nRMSSD = ",
                               round(sqrt(mean(diff(60000/ibi_corrected$y)^2)),2)," ms",
                               sep=""),
                    line=list(dash='dot')) %>%
          layout(shapes = circles,
                 legend = list(valign="top",orientation="h"),
                 margin = list(l=30,r=50),
                 title = paste("Change in HRV (RMSSD) = ",
                               round(abs(sqrt(mean(diff(ibi_original$IBI*1000)^2)) - sqrt(mean(diff(60000/ibi_corrected$y)^2))),2),
                               " ms",sep=""),
                 xaxis = list(title = "time (s)"),
                 yaxis = list(title = "HR (bpm)"),
                 yaxis2 = list(overlaying = "y",
                               side = "right",
                               title = "IBI (ms)",
                               range = c(max(ibi_original$IBI*1000),min(ibi_original$IBI*1000))),
                 plot_bgcolor="#EBEBEB"
          ) %>%
          config(edits = list(shapePosition = TRUE))
        
        p  %>% # abline DOESN'T WORK (!)
          add_lines(x=ibi_original$t,y=rep(input$minbpm,nrow(ibi_original)),showlegend=FALSE,
                    color = I("green")) %>%
          add_lines(x=ibi_original$t,y=rep(input$maxbpm,nrow(ibi_original)),showlegend=FALSE)
        
      } else {  # BVP mode
        p <- plot_ly() %>%
          add_markers(x = ibi_original$t, y = ibi_original$BVPamp,
                      name='Original data',color=I("black"),
                      line=list(dash='dash'),
                      showlegend=FALSE) %>%
          add_lines(x = ibi_original$t, y = ibi_original$BVPamp,color=I("black"),
                    name=paste('Original data\nRMSSD = ',
                               round(sqrt(mean(diff(ibi_original$BVPamp)^2)),2)," AU",
                               sep="")) %>%
          add_lines(x = ibi_corrected$x, y = ibi_corrected$y,color=I("transparent"),
                    showlegend=FALSE,yaxis="y2") %>%
          add_lines(x = ibi_corrected$x, y = ibi_corrected$y, color = I("green"),
                    name=paste("Corrected data\nRMSSD = ",
                               round(sqrt(mean(diff(ibi_corrected$y)^2)),2)," AU",
                               sep=""),
                    line=list(dash='dot')) %>%
          layout(shapes = circles,
                 legend = list(valign="top",orientation="h"),
                 margin = list(l=30,r=50),
                 title = paste("Change in BVP (RMSSD) = ",
                               round(sqrt(mean(diff(ibi_original$BVPamp)^2)) - sqrt(mean(diff(ibi_corrected$y)^2)),2),
                               " ms",sep=""),
                 xaxis = list(title = "time (s)"),
                 yaxis = list(title = "BVP (arbitrary units)"),
                 yaxis2 = list(overlaying = "y",
                               side = "right",
                               title = "BVP amplitude",
                               range = c(max(ibi_original$IBI),min(ibi_original$IBI))),
                 plot_bgcolor="#EBEBEB"
          ) %>%
          config(edits = list(shapePosition = TRUE))
        
        p  %>% # abline DOESN'T WORK (!)
          add_lines(x=ibi_original$t,y=rep(input$minbpm,nrow(ibi_original)),showlegend=FALSE,
                    color = I("green")) %>%
          add_lines(x=ibi_original$t,y=rep(input$maxbpm,nrow(ibi_original)),showlegend=FALSE)
      }
      
      p
      
    }
    
  })
  
  # update x/y reactive values in response to changes in shape anchors
  observe({
    ed <- event_data("plotly_relayout")
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    if (length(shape_anchors) != 2) return()
    row_index <- unique(parse_number(names(shape_anchors)) + 1)
    pts <- as.numeric(shape_anchors)
    HR2plot$x[row_index] <- pts[1]
    HR2plot$y[row_index] <- pts[2]
    # HR2plot$artefact <- values$ibi$artefact
    HR2plot$artefact <- hrv.data.filt()$artefact
  })
  
  # saving changes
  hrv.manuallyEdited <- reactive({
    req(input$datafile,nrow(values$ibi)>0)
    # if(input$datafileFormat=="IBI"){
      ed <- event_data("plotly_relayout")
      if(!is.null(ed)){
        if(input$detectionMode=="IBI"){
          hrv.manuallyEdited <- data.frame(t=HR2plot$x,
                                           IBI=60000/HR2plot$y,
                                           artefact=values$ibi$artefact)
        } else {
          hrv.manuallyEdited <- data.frame(t=HR2plot$x,
                                           IBI=HR2plot$y,
                                           artefact=values$ibi$artefact)
        }
      
    } else {
      hrv.manuallyEdited <- "no changes"
    }
    
    hrv.manuallyEdited
  })
  
  # showing dataset
  output$manualEditor.dataset <- DT::renderDataTable(
    if(!is.null(input$datafile) & nrow(values$ibi)>0){
      df <- data.frame(t=round(hrv.data.filt()$t,2),
                       IBI.edited=round(hrv.data.filt()$IBI*1000,2),
                       artefact=hrv.data.filt()$artefact)
      df <- plyr::join(df,round(values$ibi[,1:2],2),by="t",type="full")
      df <- df[order(df$t),c(1,4,2,3)]
      colnames(df)[2] <- "IBI.original"
      DT::datatable(df) %>%
        formatStyle(columns=c("t","IBI.original","IBI.edited","artefact"),  
                    backgroundColor = '#BBCEEB', fontWeight = 'bold')
    }
    ,options = list(pageLength = 25)
  )

  # Plot non interpolated HR
  output$plotNIHR <- renderPlot({
    if(nrow(values$ibi)>1){
      
      # selecting data depending on input file
      if(input$datafileFormat=="IBI"){
        df <- data.frame(t=values$ibi$t,
                         IBI=60000/values$ibi$IBI)
        } else {
          df <- data.frame(t=values$ibi$t,
                           IBI=60/values$ibi$IBI,
                           BVPamp=values$ibi$BVPamp)
        }
      
      # plotting
      par(mar=c(5, 4, 4, 4))
      if(input$detectionMode=="IBI"){
        plot(df$t,
             df$IBI,
             xlab = "time (sec)", ylab = "HR (bpm)",type="l",lwd=1.2,
             cex.lab=1.2)
      } else {
        plot(df$t,
             df$BVPamp,
             xlab = "time (sec)", ylab = "BVP (AU)",type="l",lwd=1.2,
             cex.lab=1.2)
      }
      
      title("Original signal", line = 2,adj=0,cex.main=2)
      grid(col="darkgray")
      if(input$detectionMode=="IBI"){
        points(df$t,df$IBI,
               pch=20,cex=1.7)
      } else {
        points(df$t,df$BVPamp,
               pch=20,cex=1.7)
      }
      # bpm min and max
      abline(h=c(input$minbpm,input$maxbpm),
             lwd=2,col="blue")
      polygon(c(-min(df[,1])^2,-min(df[,1])^2,
                max(df[,1])^2,max(df[,1])^2),
              c(input$maxbpm,max(df[,2])^2,
                max(df[,2])^2,input$maxbpm),col=rgb(0,0,1,0.2))
      polygon(c(-min(df[,1])^2,-min(df[,1])^2,
                max(df[,1])^2,max(df[,1])^2),
              c(-min(df[,2])^2,input$minbpm,
                input$minbpm,-min(df[,1])^2),col=rgb(0,0,1,0.2))
      
      # second axis IBIs
      if(input$detectionMode=="IBI"){
        axis(side = 4,at=seq(min(df$IBI),max(df$IBI),length.out=5),
             labels=round(seq(max(60000/df$IBI),min(60000/df$IBI),length.out=5),0))
        mtext(side = 4, line = 3, 'IBI (ms)',cex=1.2)
        
        # artefact
        points(df[df$IBI>input$maxbpm|df$IBI<input$minbpm,1],
               df[df$IBI>input$maxbpm|df$IBI<input$minbpm,2],
               pch=10,col="red",cex=3,lwd=2)
        points(df[df$IBI>input$mINbpm,1],
               df[df$IBI>input$mINbpm,2],
               pch=10,col="red",cex=3,lwd=2)
      } else {
        # artefact
        points(df[df$BVPamp>input$maxbpm|df$BVPamp<input$minbpm,1],
               df[df$BVPamp>input$maxbpm|df$BVPamp<input$minbpm,2],
               pch=10,col="red",cex=3,lwd=2)
        points(df[df$BVPamp>input$mINbpm,1],
               df[df$BVPamp>input$mINbpm,2],
               pch=10,col="red",cex=3,lwd=2)
      }
      
      
      
      # ibi <- values$ibi
      ibi <- hrv.artefactFree()
      if(input$detectionMode=="IBI"){
      for(i in 1:nrow(ibi)){
        if(!is.na(ibi[i,"artefact"])){
          points(df[i,1],
                 df[i,2],
                 pch=10,col="red",cex=3,lwd=2)
        }
      }
        } else {
          
          for(i in 1:nrow(ibi)){
            if(!is.na(ibi[i,"artefact"])){
              points(df[i,"t"],
                     df[i,"BVPamp"],
                     pch=10,col="red",cex=3,lwd=2)
            }
          }
          }

      }
  })
  
  # Plot automatic correction
  output$plotNIHR.filt <- renderPlot({
    if(nrow(values$ibi)>1){
      
      if(input$datafileFormat=="IBI"){
        df <- data.frame(t=hrv.data.filt()$t,
                         IBI=60/hrv.data.filt()$IBI)
      } else {
        df <- data.frame(t=hrv.data.filt()$t,
                         IBI=60/hrv.data.filt()$IBI,
                         BVPamp=hrv.data.filt()$BVPamp)
      }
      
      # plotting
      par(mar=c(5, 4, 4, 4))
      if(input$detectionMode=="IBI"){
        plot(df$t,
             df$IBI,
             xlab = "time (sec)", ylab = "HR (bpm)",type="l",lwd=1.2,col="red",
             cex.lab=1.2)
      } else {
        plot(df$t,
             df$BVPamp,
             xlab = "time (sec)", ylab = "BVP (AU)",type="l",lwd=1.2,col="green",
             cex.lab=1.2)
      }
      
      title("Corrected signal", line = 2,adj=0,cex.main=2)
      grid(col="darkgray")
      if(input$detectionMode=="IBI"){
        points(df$t,df$IBI,col="red",
               pch=20,cex=1.7)
      } else {
        points(df$t,df$BVPamp,col="#24BD28",
               pch=20,cex=1.7)
      }
      
      # bmp min and max
      abline(h=c(input$minbpm,input$maxbpm),
             lwd=2,col="blue")
      polygon(c(-min(df[,1])^2,-min(df[,1])^2,
                max(df[,1])^2,max(df[,1])^2),
              c(input$maxbpm,max(df[,2])^2,
                max(df[,2])^2,input$maxbpm),col=rgb(0,0,1,0.2))
      polygon(c(-min(df[,1])^2,-min(df[,1])^2,
                max(df[,1])^2,max(df[,1])^2),
              c(-min(df[,2])^2,input$minbpm,
                input$minbpm,-min(df[,1])^2),col=rgb(0,0,1,0.2))
      
      # second axis (IBIs)
      if(input$detectionMode=="IBI"){
        axis(side = 4,at=seq(min(df$IBI),max(df$IBI),length.out=5),
             labels=round(seq(max(60000/df$IBI),min(60000/df$IBI),length.out=5),0))
        mtext(side = 4, line = 3, 'IBI (ms)',cex=1.2)
      }
      
      # artefacts
      # ibi <- hrv.artefactFree()
      # for(i in 1:nrow(ibi)){
      #   if(!is.na(ibi[i,"artefact"])){
      #     points(df[i,1],
      #            df[i,2],
      #            pch=10,col="red",cex=3,lwd=2)
      #   }
      # }  
    }
  })
  
  # Plot interpolated HR
  output$plotHR <- renderPlot({
    if(nrow(values$ibi)>1 & input$detectionMode=="IBI"){
      
      if(input$which=="Original signal"){
        hrv.data <- values$ibi
        hrv.data$IBI <- 60/hrv.data$IBI
        INTERP <- hrv.data.inter()*1000
        
        
      }else{
        hrv.data <- hrv.data.filt()
        hrv.data$IBI <- 60000/hrv.data$IBI
        INTERP <- hrv.data.inter()
      }
      
      colnames(hrv.data)[2] <- "HR"
      
      par(mar=c(5, 4, 4, 4))
      plot(seq(0,max(hrv.data$t),length.out=length(INTERP)),
           INTERP,
           xlab = "time (sec)", ylab = "HR (bpm)",type="l",lwd=1.2,col="red",
           cex.lab=1.2)
      
      # # second axis (IBIs)
      # axis(side = 4,at=seq(min(INTERP),max(INTERP),length.out=5),
      #      labels=round(seq(max(60000/hrv.data$HR),min(60000/hrv.data$HR),length.out=5),0))
      axis(side = 4,at=seq(min(INTERP),max(INTERP),length.out=5),
           labels=round(seq(60000/min(INTERP),60000/max(INTERP),length.out=5),0))
      mtext(side = 4, line = 3, 'IBI (ms)',cex=1.2)
      
      title("Interpolated signal", line = 2,adj=0,cex.main=2)
      grid(col="darkgray")
      
      # # bpm min and max
      # abline(h=c(input$minbpm,input$maxbpm),
      #        lwd=2,col="blue")
      # polygon(c(-min(hrv.data[,1])^2,-min(hrv.data[,1])^2,
      #           max(hrv.data[,1])^2,max(hrv.data[,1])^2),
      #         c(input$maxbpm,max(hrv.data[,2])^2,
      #           max(hrv.data[,2])^2,input$maxbpm),col=rgb(0,0,1,0.2))
      # polygon(c(-min(hrv.data[,1])^2,-min(hrv.data[,1])^2,
      #           max(hrv.data[,1])^2,max(hrv.data[,1])^2),
      #         c(-min(hrv.data[,2])^2,input$minbpm,
      #           input$minbpm,-min(hrv.data[,1])^2),col=rgb(0,0,1,0.2))
      
      
      
      points(seq(0,max(hrv.data$t),length.out=length(INTERP)),
             INTERP,
             pch=20,cex=1.5,col="red")
    }
  })
  
  # --------------------------------------------------------------
  ################################################################ TAB 4
  # --------------------------------------------------------------
  # Tab 4: Analyses
  # --------------------------------------------------------------
  
  # HRV analyses
  hrv.analysis <- reactive({
    
    if(input$showHRV>0 & nrow(values$ibi)>1){
      
      if(input$which=="Segnale originale"){
        hrv.data <- values$ibi
      }else{
        hrv.data <- hrv.data.filt()
      }
      
      hrv.analysis <- list(NULL)
      # time domain analysis
      hrv <- data.frame(HR=mean(60/hrv.data$IBI),
                        BVPamp=mean(na.omit(values$ibi$BVPamp)),
                        SDNN=sd(hrv.data$IBI*1000),
                        RMSSD=sqrt(mean(diff(hrv.data$IBI*1000)^2)))
      
      # FREQUENCY-DOMAIN ANALYSIS (see Cohen, 2014; chapter 11)
      IBI <- detrend((60/hrv.data.inter())*1000,tt="linear") # detrend
      IBI.ft <- abs(fft(IBI)) # fast Fourier Transform
      hrv.analysis$IBI.ft <- IBI.ft
      SampFreq <- input$freqhr # sampling frequency used for interpolation (default = 4Hz)
      Nyquist_Freq <- SampFreq/2 # Nyquist Frequency = the faster frequency (in Hz) that can be measured with the used SampFreq
      Freqs_lab <- seq(0, Nyquist_Freq,length.out=round(length(IBI.ft)/2,0)+1) # labels corresponding to FFT
      fft_freq <- hrv.analysis$IBI.ft[1:length(Freqs_lab)] # power corresponging to frequency bands
      
      # average number of points per frequency band (by which deviding the abs power...)
      mean.point <- mean(c(length(IBI.ft[Freqs_lab < 0.04]),
                           length(IBI.ft[Freqs_lab >= 0.04 & Freqs_lab < 0.15]),
                           length(IBI.ft[Freqs_lab >= 0.15 & Freqs_lab <= 0.4])))
      
      hrv$VLF=round(sum(IBI.ft[Freqs_lab < 0.04])/mean.point,2)
      hrv$LF=round(sum(IBI.ft[Freqs_lab >= 0.04 & Freqs_lab < 0.15])/mean.point,2)
      hrv$HF=round(sum(IBI.ft[Freqs_lab >= 0.15 & Freqs_lab <= 0.4])/mean.point,2)
      
      hrv.analysis <- list(NULL)
      hrv.analysis$hrv <- hrv
      hrv.analysis$IBI.ft <- IBI.ft
      hrv.analysis$Freqs_lab <- Freqs_lab
      hrv.analysis$fft_freq <- fft_freq
      hrv.analysis
      
    }
    
  })
  
  # adding analysis to HRV dataset
  values$hrv <- data.frame(HR=NULL,BVPamp=NULL,SDNN=NULL,RMSSD=NULL,
                           VLF=NULL,LF=NULL,HF=NULL)
  addHRV <- observe({
    req(input$datafile)
    cat("addHRV\n")
    if(input$addHRV>0) {
      new.value <- isolate(hrv.analysis()$hrv)
      newRow <- isolate(req(hrv.analysis()$hrv))
      isolate(values$hrv <- rbind(values$hrv,hrv.analysis()$hrv))
      
    }
  })
  
  # --------------------------------------------------------------
  # Tab 4: Display dataset
  # --------------------------------------------------------------
  
  # HRV measures of the considered interval
  output$hrv <- renderTable({
    if(input$showHRV>0 & nrow(values$ibi)>1){
      hrv <- hrv.analysis()$hrv
      colnames(hrv) <- c("HR","BVP","SDNN","RMSSD","VLF","LF","HF")
      data.table(hrv)
    }
  })
  
  # HRV measures of all intervals
  output$HRV <- renderTable({
    if(input$addHRV>0){
      hrv <- values$hrv
      colnames(hrv) <- c("HR","BVP","SDNN","RMSSD","VLF","LF","HF")
      data.table(hrv)
    }
  })
  
  # --------------------------------------------------------------
  # Tab 4: Download HRV
  # --------------------------------------------------------------
  
  output$downloadHRV <- downloadHandler(
    file = paste("ID_SESSION_HRV.csv"),
    content = function(file) {
      hrv.tosave <- values$hrv
      hrv.tosave$meanBVPamp <- mean(na.omit(hrv.data.filt()$BVPamp))
      write.csv(round(hrv.tosave,2),file,row.names=FALSE)
    },
    contentType="text/csv"
  )
  
  # --------------------------------------------------------------
  # Tab 4: Plotting frequency bands
  # --------------------------------------------------------------
  
  # Spectral power
  output$powerplot <- renderPlot({
    if(input$showHRV>0){
      
      hrv.data <- hrv.analysis()
      IBI.ft <- hrv.data$IBI.ft
      Freqs_lab <- hrv.data$Freqs_lab
      fft_freq <- hrv.data$fft_freq
      
      # plot Power
      plot(Freqs_lab,fft_freq, type="l",
           xlab="Frequenza (Hz)",ylab="Potenza (ms^2)",xlim=c(0,0.4),log="",
           main="Power Spectrum Density (PSD)")
      abline(v=c(0.04,0.15),lwd=2)
      # VLF
      polygon(c(min(Freqs_lab),
                Freqs_lab[Freqs_lab<=Freqs_lab[which.closest(Freqs_lab,0.04)]],0.039),
              c(fft_freq[Freqs_lab==min(Freqs_lab)],
                fft_freq[Freqs_lab<=Freqs_lab[which.closest(Freqs_lab,0.04)]],0),
              col=rgb(1,0,0,alpha=0.3))
      # LF
      polygon(c(0.04,Freqs_lab[which.closest(Freqs_lab,0.04):which.closest(Freqs_lab,0.15)],0.15),
              c(0,fft_freq[which.closest(Freqs_lab,0.04):which.closest(Freqs_lab,0.15)],0),
              col=rgb(0,0.5,1,alpha=0.3))
      # HF
      polygon(c(0.15,Freqs_lab[which.closest(Freqs_lab,0.15):which.closest(Freqs_lab,0.4)],0.4),
              c(0,fft_freq[which.closest(Freqs_lab,0.15):which.closest(Freqs_lab,0.4)],0),
              col=rgb(0,1,0,alpha=0.3))
      
      
    }
    
  })
  
  # Plot IBI time series
  output$plotHR2 <- renderPlot({
    if(input$interpolate>0 & nrow(values$ibi)>1){
      
      plot(hrv.data.filt()$t,
           hrv.data.filt()$IBI*1000,
           xlab = "time (sec)", ylab = "IBI (ms)",type="l",
           main="Serie temporale degli intervalli interbattito")
      grid()
      points(hrv.data.filt()$t,
             hrv.data.filt()$IBI*1000,
             pch=20,cex=1.5,col="red")
    }
  })
  
  # plot IBI distribution
  output$IBIdist <- renderPlot({
    if(input$interpolate>0 & nrow(values$ibi)>1){
      hrv.data <- hrv.data.filt()
      hist(hrv.data$IBI*1000,breaks=nrow(hrv.data.filt()),col="black",
           xlab="IBI (ms)",main="Distribuzione degli intervalli interbattito")
    }
  })
  
}

shinyApp(ui = ui, server = server)