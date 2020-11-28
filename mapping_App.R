
suppressWarnings(suppressPackageStartupMessages(require(shiny)))
suppressWarnings(suppressPackageStartupMessages(require(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(require(MALDIquant)))
suppressWarnings(suppressPackageStartupMessages(require(plotKML)))
suppressWarnings(suppressPackageStartupMessages(require(data.table)))
suppressWarnings(suppressPackageStartupMessages(require(tidyr)))
suppressWarnings(suppressPackageStartupMessages(require(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(require(rdrop2)))
suppressWarnings(suppressPackageStartupMessages(require(shinyWidgets)))
source('merge_log_gpx.R')



# check dropbox connectivity
drop_auth(rdstoken = "droptoken.rds") # connect and authorise the connection to dropbox. Log in may be required

# check for monitorin app folder in dropbox, and create it if it is the first time using the app
if (drop_exists('monitoring_app')){

}else{  # create the files from to store logs and outputs
  logs =list.files('logs/')
  drop_upload(
    paste('logs/', logs, sep=''), # upload the template
    path = "monitoring_app/logs/")
  drop_upload(
    'monitoring.txt',   # this is also an example. CLEAN IT BEFORE
    path = 'monitoring_app'
  )
} # main folder with all the file structures

sps <- read.table('lists/species.txt', header=TRUE, colClasses='character')
cats <- read.table('lists/category.txt', header=TRUE, na.strings="na" )
proj <- read.table('lists/projects.txt', header=TRUE, colClasses='character' )
surv <- read.table('lists/surveys.txt', header=TRUE, colClasses='character' )
mir <- read.table('lists/tags.txt', header=TRUE, colClasses='character' )
nor_map <- read.table('norway_coast.txt', header=TRUE, sep='\t')




# Define UI ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Shellfish Monitoring Tool"),
  tabsetPanel(type = "tabs",
              # logger ----
              tabPanel("Logging changes in cover", 
                       fluidRow(column(6,
                                       div(style = "height:10px"),
                                       div(style="display:inline-block", selectInput('pro', h5(strong('Project')), proj[,1])),
                                       div(style="display:inline-block", selectInput('sur', h5(strong('Survey')), surv[,1]))
                       )),
                       fluidRow(column(12,
                                       actionButton("start", "start log", class = "btn-warning"),
                                       div(style="display:inline-block;width:40px"),
                                       shinyjs::disabled(div(style="display:inline-block", actionButton("end", "end log", class = "btn-warning"))),
                                       div(style = "height:20px"),
                                       div(style = "height:2px;background-color:black")
                       )),
                       fluidRow(column(6,
                                     div(style = "height:10px"),
                                     div(style="display:inline-block", selectInput('Species', h5(strong('Species')), sps[,1])),
                                     div(style="display:inline-block",selectInput('num', h5(strong('Category')), cats[,1])),
                                     div(style = "height:20px"),
                                     div(style="display:inline-block", shinyjs::disabled(actionButton("loging", "log category",class = "btn-danger"))),
                                     div(style="display:inline-block; width:100px"),
                                     div(style="display:inline-block", shinyjs::disabled(actionButton("chnlog", "update category",class = "btn-warning"))),
                                     div(style = "height:20px"),
                                     multiInput(
                                       inputId = "Tags", label = "Select all that apply:",
                                       choices = mir[,1],
                                       width = "400px",
                                       options = list(
                                         enable_search = FALSE
                                       )
                                     ),
                                     textAreaInput("notes", h5(strong("Notes"),em('notes and tags added to the last log line.')), value = "", width='100%', height='100px'),
                                     shinyjs::disabled(actionButton("adnote", "add notes")),
                                     shinyjs::disabled(actionButton("renote", "clear"))
                       ),

                                column(6,
                                       tableOutput('logger'))
                       )
              ),
              # Metadata management ----
              tabPanel("Updating metadata lists",
                       fluidRow(
                         column(2,
                                div(style = "height:10px"),
                                actionButton('add', 'add metadata', class="btn-warning")),
                         column(10, 
                                helpText('This tab allows the user to add metadata options to be available during logging. Just introduce the new values below on the relevant place. Several metadata fields can be entered at once but only one value for each list')
                         )
                       ),
                       fluidRow(column(2, textInput('projin', h5(strong('Projects')))),
                                column(3, textInput('survin', h5(strong('Surveys')))),
                                column(2, textInput('sppin', h5(strong('Species')))),
                                column(2, textInput('catsin', h5(strong('Categories')))),
                                column(3, textInput('tagsin', h5(strong('Tags'))))
                       ),
                       fluidRow(column(2, tableOutput('proj')),
                                column(3, tableOutput('surv')),
                                column(2, tableOutput('spp')),
                                column(2, tableOutput('cats')),
                                column(3, tableOutput('mir'))
                                )
              ),
              
              # Merge logger and GPS data ----
              tabPanel("Uploading GPX file",
                       tags$hr(),
                       fluidRow(column(12, wellPanel( # select file -----
                                                      fileInput("file1", "Choose *.gpx file with GPS track",
                                                                multiple = TRUE,
                                                                accept = ".gpx"
                                                                ),
                       ))),
                       fluidRow(
                         column(12,
                                div(style="display:inline-block", actionButton("ready", "merge data")),
                                div(style="display:inline-block", actionButton("append", "append log"))
                                )
                       ),

                       fluidRow(
                         column(4,
                                div(style="height:20px"),
                                selectInput('spss', label="Species", sps[,1]),
                                div(style="height:20px"), 
                                sliderInput("lat", "Latitude:",min = round(min(nor_map$lat),0), max = round(max(nor_map$lat),0),value = c(60,70)),
                                sliderInput("lon", "Longitude:",min = round(min(nor_map$long),0), max = round(max(nor_map$long),0),value = c(5,25)),
                         ),
                         column(8,
                                plotOutput("plot")
                                )
                       )

              ),
              # Instructions ----
              tabPanel("User Manual",
                       helpText(strong("Shellfish monitoring tool: "),
                                br("This app is a event logger and includes a tool to merge the logged events with GPS tracks in .gpx format"))
              ) # tab pannel 
              )
)


# Define server logic ----
server <- function(input, output, session) {
  # logger behaviour ----
  log <- reactiveValues(df_data = NULL)  # to store the log
  log <- reactiveValues(name = NULL)   # to store the name of the log
  
  observeEvent(input$start,{ # start a new file
      shinyjs::enable("loging")
      shinyjs::enable("end")
      shinyjs::disable("start")
      log$df_data<-data.frame(project = input$pro, survey = input$sur, species='NA', log='start_log', time = as.character(Sys.time()), notes=as.character(" "), stringsAsFactors = FALSE)
      log$name <- paste('logs/',log$df_data[1,1], '_',log$df_data[1,2], '_', gsub(":", "_", log$df_data[1,5], fixed = TRUE), '.txt', sep='')
      write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
  })
  
  observeEvent(input$loging, { # add lines to the log
    shinyjs::enable("chnlog")
    shinyjs::enable("adnote")
    shinyjs::enable("renote")
    addon <- data.frame(project = input$pro, survey = input$sur, species=input$Species, log=input$num, time = as.character(Sys.time()), notes=as.character(" "))
    lg <- rbind(log$df_data, addon)
    log$df_data <- lg
    write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
  })
  
  observeEvent(input$chnlog,{ # change the last log category, correct mistake
    log$df_data[nrow(log$df_data),4] <- input$num
    write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
  })
  
  observeEvent(input$adnote, { # introduce the notes for the last log
    if (is.null(input$Tags)){
      notes <- input$notes
      log$df_data[nrow(log$df_data),6] <- notes
      write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
    }else{
      ids <- paste(input$Tags, collapse=", ")
      notes <- paste(ids,', ',input$notes, sep='')
      log$df_data[nrow(log$df_data),6] <- notes
      write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
    }
  })
  
  observeEvent(input$renote, { # introduce the notes for the last log
    reset('notes')
  })
  
  
  observeEvent(input$end, { # add end line to the log file, save it, clean the running log and reactivate start button
    shinyjs::disable("loging")
    shinyjs::disable("end")
    shinyjs::disable("adnote")
    shinyjs::disable("renote")
    shinyjs::enable("start")
    addon <- data.frame(project = input$pro, survey = input$sur, species='NA', log='end_log', time = as.character(Sys.time()), notes=as.character(" "))
    lg <- rbind(log$df_data, addon)
    log$df_data <- lg
    write.table(log$df_data, file=log$name, row.names=FALSE, sep='\t')
    drop_upload(log$name, path='monitoring_app/logs/')
    file.remove(log$name)  # to avoid the files to be stored in the shiny server
    log$name = NULL
    log$df_data = NULL
  })
  

  # 
  output$logger<-renderTable(tail(log$df_data, n=5))
  
  # Update metadata droplists ----
  p <- reactiveValues(df_project = read.table('lists/projects.txt', header=TRUE, colClasses='character'))  
  s <- reactiveValues(df_survey = read.table('lists/surveys.txt', header=TRUE, na.strings="na" ))  
  ss <- reactiveValues(df_species = read.table('lists/species.txt', header=TRUE, na.strings="na" )) 
  ct <- reactiveValues(df_categories = read.table('lists/category.txt', header=TRUE, na.strings="na"))  
  tg <- reactiveValues(df_tags = read.table('lists/tags.txt', header=TRUE, na.strings="na"))
  
  observeEvent(input$add,{
    if((input$projin)=='' & (input$survin)=='' & (input$sppin)=='' & (input$catsin)=='' & (input$tagsin)==''){
      showModal(modalDialog(
        renderText('All fields are empty'),
        title = NULL,
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      if (input$projin!=''){
        p$df_project <- rbind(p$df_project, input$projin)
        p$df_project <- unique(p$df_project)
        write.table(p$df_project , file='lists/projects.txt', row.names=FALSE, sep='\t')
        updateSelectInput(session, inputId='pro', label='Project', choices=p$df_project[,1])
      }
      if (input$survin!=''){
        s$df_survey <- rbind(s$df_survey, input$survin)
        s$df_survey <- unique(s$df_survey)
        write.table(s$df_survey , file='lists/surveys.txt', row.names=FALSE, sep='\t')
        updateSelectInput(session, inputId='sur', label='Survey', choices=s$df_survey[,1])
      }
      if (input$sppin!=''){
        ss$df_species <- rbind(ss$df_species, input$sppin)
        ss$df_species <- unique(ss$df_species)
        write.table(ss$df_species , file='lists/species.txt', row.names=FALSE, sep='\t')
        updateSelectInput(session, inputId='Species', label='Species', choices=ss$df_species[,1])
      }
      if (input$catsin!=''){
        ct$df_categories <- rbind(ct$df_categories, input$catsin)
        ct$df_categories <- unique(ct$df_categories)
        write.table(ct$df_categories , file='lists/category.txt', row.names=FALSE, sep='\t')
        updateSelectInput(session, inputId='num', label='Category', choices=ct$df_categories[,1])
      }
      if (input$tagsin!=''){
        tg$df_tags <- rbind(tg$df_tags, input$tagsin)
        tg$df_tags <- unique(tg$df_tags)
        write.table(ct$df_tags , file='lists/tags.txt', row.names=FALSE, sep='\t')
        updateMultiInput(session, inputId='Tags', choices=tg$df_tags[,1])
      }
    }
  })
  

  output$proj<-renderTable(p$df_project, colnames=FALSE, width='100%')
  output$surv<-renderTable(s$df_survey, colnames=FALSE, width='100%')
  output$spp<-renderTable(ss$df_species, colnames=FALSE, width='100%')
  output$cats<-renderTable(ct$df_categories, colnames=FALSE,  width='100%')
  output$mir<-renderTable(tg$df_tags, colnames=FALSE,  width='100%')
  
  
  
  
  # merge logger and gps ----
    # read GPX file
  
  df<- eventReactive(input$ready,{
    if(is.null(input$file1)){
      df<-data.frame(NO_DATA_TO_PREPARE = "Select a file to merge")
      return(df)
    }else{
      logs <- drop_dir('monitoring_app/logs')$path_lower # load the logs from your dropbox
      mon <- read.table('monitoring.txt', header=TRUE, sep='\t',  # create a monitoring file for gpx uploaded. Avoid mixing lines. Put project number, survey and date-time 
                        colClasses = c('numeric', 'numeric', 'POSIXct', 'character','character','character','character','character','numeric'))
      ln_max=1
      for(i in 1:length(logs)){
        pth = tempdir()
        drop_download(logs[i], local_path=pth, overwrite=TRUE)
        localfile = paste0(pth, "/", basename(logs[i]))
        log <- read.table(localfile, header=TRUE, sep='\t', colClasses = c('character', 'character', 'character', 'character','POSIXct','character'),   na.strings ='NA')
        log <- merge_log_gpx(log,input$file1$datapath, ln_max, st_date)
        if(length(df)<2){
          df<-log
          ln_max <- max(log$line)+1
          st_date <- max(log$time)
        }else{
          df<-rbind(df, log)
          ln_max <- max(log$line)+1
          st_date <- max(log$time)
        }
      }
      
      return(df)
    }
  })
  
  observe({
      lon1 <- round(min(df()[,1]),2)
      lon2 <- round(max(df()[,1]),2)
      lat1 <- round(min(df()[,2]),2)
      lat2 <- round(max(df()[,2]),2)
      spss <- unique(df()[,6])
      ###
      updateSliderInput(session, 'lat', min=lat1, max=lat2, value=c(lat1,lat2))
      updateSliderInput(session, 'lon', min=lon1, max=lon2, value=c(lon1,lon2))
      
  })

  output$plot <- suppressWarnings(renderPlot({
    ggplot()+
      geom_path(aes(x=df()[,1], y=df()[,2], group=df()[,9], colour=df()[,7]), size=2, alpha=0.8)+
      scale_colour_manual(name = NULL, values=c('yellow','darkgreen','red'))+
      geom_path(data=nor_map, aes(x=long, y=lat, group=piece))+
      scale_x_continuous(name='longitude', limits=c(min(df()[,1]), max(df()[,1])))+
      scale_y_continuous(name='latitude', limits=c(min(df()[,2]), max(df()[,2])))+
      coord_cartesian(xlim=c(input$lon[1],input$lon[2]), ylim=c(input$lat[1],input$lat[2]))+
      theme_bw()
  }))



  ## append to dataset
  observeEvent(input$append, {
    if(is.null(input$file1)||is.null(df)||ncol(df())<2){
      showModal(modalDialog(
        renderText('Select a file to upload and/or process it for upload'),
        title = NULL,
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
        dt <- df()
        mon <- read.table('monitoring.txt', header=TRUE, sep='\t',
                          colClasses = c('numeric', 'numeric', 'POSIXct', 'character','character','character','character','character','numeric'))
        attr(mon$time, "tzone") <- "UTC"
        mon <- rbind(mon, dt)
        mon <- unique(mon) # remove duplicates
        logsname <- drop_dir('monitoring_app/logs')$name # load the name of first log
        monname <- paste('tracks',substr(logsname[1],1,8), substr(logsname, which(strsplit(logsname[1],"")[[1]]=="_")[2]+1, nchar(logsname[1])-4),
                         substr(logsname, which(strsplit(logsname[length(logsname)],"")[[1]]=="_")[2]+1, nchar(logsname[length(logsname)])-4), sep='-')
        monname <- paste(monname, '.txt', sep='')
        write.table(mon, file=monname, row.names=FALSE, col.names=TRUE,sep='\t')
        drop_upload(monname, path='monitoring_app/logs/')
        file.remove(monname)  # to avoid the files to be stored in the shiny server
        
        showModal(modalDialog(
            renderText('Data has been successfully merge and a file has been created'),
            title = NULL,
            easyClose = TRUE,
            footer = NULL
          ))
    }
    }
  )
  }

# Run the app ----
shinyApp(ui = ui, server = server)