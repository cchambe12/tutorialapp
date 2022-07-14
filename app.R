### Started 24 March 2022 by Cat
## Building Shiny App for Coalition simulations for Mike


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)
library(viridis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(brms)


#setwd("~/Documents/git/tutorialapp/")
source("source/coalitionimpact.R")

brazil <- read.csv("source/clean_brazil_stan.csv")
#brazil <- brazil[!is.na(brazil$intervention),]

load("source/models/lcrate_brms_noadm_10yr_newpathways.Rdata",.GlobalEnv)

#forecasts <- read.csv("source/clean_forecasts_lcrate.csv")


ui <- fluidPage(theme = shinytheme("cosmo"),
                
                navbarPage("Impact Forecasting",
                           tabPanel("Home",
                                    mainPanel(h1("Estimating conservation investment impact on climate change outcomes"),
                                              p(style="text-align: justify;","To progress investments in nature, forecasting impact is essential to effectively implement new conservation strategies. In this Shiny App, we provide a tool where users can consider how conservation investments and various interventions impact climate change mitigation. 
                                              We consider the impacts of various actions on the rate of forest cover change throughout Brazil based on ten major parameters: 1) years of project, 
                                              2) investment per year, 3) number of roads, 4) protected areas, 5) Palmer Drought Severity Index (PDSI), 6) rate of change in PDSI, 
                                                7) mean spring temperature, 8) rate of change in mean spring temperature, 9) elevation and 10) slope. "),
                                              br(),
                                              
                                              h3("We determine effects of parameters on conservation strategy returns on investment "),
                                              h5("see page ``Evaluating project impact''"),
                                              br(),
                                              p(style="text-align: justify;","Under the `Funder information` tab, you can see the breakdown of investments by action track type by each funding organzation included in the data."),
                                              br(),
                                              p(style="text-align: justify;","Under the `Parameters` tab, you can use the sidebar to select the parameters of interest and how they vary across intervention type as well as comparisons between treatment and control groups and also see overall model output."),
                                              br(),
                                              br(),
                                              br(),
                                              h3("Next, we begin to run initial forecasts based on base models"),
                                              h5("see page ``Forecasting parameters''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can use the sidebar to select the parameters of interest and project how they vary across intervention type as well as comparisons between treatment and control groups."),
                                              br(),
                                              h5("see page ``Forecasting investments''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can adjust investments by intervention type and see projected change in forest cover in comparison to control groups."),
                                              br(),
                                              br(),
                                              br(),
                                              h3("Finally, we permit users to predict ROI by adjusting anticipated shifts in parameters"),
                                              h5("see page ``Forecasting impact by user input''"),
                                              br(),
                                              p(style="text-align: justify;","Here, you can adjust changes in parameter values to help select sites and see projected change in forest cover in comparison to control groups."),
                                              br(),
                                              br(),
                                              br(),
                                              br(),
                                              br()
                                              
                                    )
                           ),

                tabPanel("Evaluating project impact",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Funder information", 
                                      selectInput(inputId = "cepf",
                                                  label = "CEPF",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "CEPF"),
                                                  selected="Include"),
                                      selectInput(inputId = "gef",
                                                  label = "GEF",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "GEF"),
                                                  selected="Include"),
                                      selectInput(inputId = "worldbank",
                                                  label = "The World Bank",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "The World Bank"),
                                                  selected="Include"),
                                      selectInput(inputId = "europeaid",
                                                  label = "EuropeAid",
                                                  choices = c("Don't Include", "Include"),
                                                  #choices = c("NA", "EuropeAid"),
                                                  selected="Include"),
                                      actionButton("realrun", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Funder information", plotOutput("realraw"), plotOutput("modoutput"))
                             )
                           )),
                               
                           sidebarLayout(
                             sidebarPanel(
                               tabPanel("Parameters", 
                                        selectInput(inputId = "parameter",
                                                    label = "Parameter breakdown",
                                                    choices = c("---Choose one---",
                                                                "Years of project", "Investment per year",
                                                                "Number of roads", "Protected areas (binary)",
                                                                "Palmer Drought Severity Index (PDSI)",
                                                                "PDSI rate of change", "Mean spring temperature",
                                                                "Mean spring temp rate of change",
                                                                "Elevation", "Slope"),
                                                    selected="---Choose one---"),
                                        actionButton("parameterrun", "View Plots",
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                               )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Parameters", 
                                        h5("Darker lines represent the intervention, lighter lines are control"), 
                                        plotOutput("parameteroutput"))
                             )
                           ))
                                      
                                   ),
                
                
                tabPanel("Forecasting investments",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting impact by intervention investment",
                                      numericInput(inputId = "ccpainvest",
                                                    label = "Total Climate-Critical PAs investments ($ millions)",
                                                    value = 150), 
                                                  #min = 0, max = 500),
                                      numericInput(inputId = "csfinvest",
                                                    label = "Total Climate-Smart Forestry investments ($ millions)",
                                                    value = 100), 
                                                  #min = 0, max = 500),
                                      numericInput(inputId = "csfarminvest",
                                                     label = "Total Climate-Smart Farming investments ($ millions)",
                                                     value = 100), 
                                                  #min = 0, max = 500),
                                      numericInput(inputId = "restoreinvest",
                                                    label = "Total Restoration investments ($ millions)",
                                                    value = 200), 
                                                  #min = 0, max = 500),
                                      actionButton("forecastactiontrackrun", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting impact by intervention investment", 
                                        h5("Please be patient. Forecast data can take a minute to build."),
                                          plotOutput("forecastaction"))
                             )
                             )),
                         
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting parameters",
                                      selectInput(inputId = "forecastparameter",
                                                  label = "Parameter breakdown",
                                                  choices = c("---Choose one---",
                                                              "Years of project", "Investment per year",
                                                              "Number of roads", "Protected areas (binary)",
                                                              "Palmer Drought Severity Index (PDSI)",
                                                              "PDSI rate of change", "Mean spring temperature",
                                                              "Mean spring temp rate of change",
                                                              "Elevation", "Slope"),
                                                  selected="---Choose one---"),
                                      actionButton("runforecastparameter", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting parameters", 
                                        h5("Darker lines represent the intervention, lighter lines are control"), 
                                        plotOutput("forecastparameterplots"))
                             )
                           ))
                         ),
                           
                           
                tabPanel("Forecasting by user input",
                         sidebarLayout(
                           sidebarPanel(
                             tabPanel("Forecasting by user input",
                                      selectInput(inputId = "action",
                                                  label = "Intervention type",
                                                  choices = c("---Choose one---",
                                                              "Climate-Critical PAs",
                                                              "Climate-Smart Forestry",
                                                              "Climate-Smart Farming",
                                                              "Forest and Wetland Restoration"),
                                                  selected="---Choose one---"),
                                      sliderInput(inputId = "investment",
                                                  label = "Total investment ($ millions)",
                                                  value = 10, 
                                                  min = 0, max = 100),
                                      sliderInput(inputId = "start",
                                                  label = "Start year of project",
                                                  value = 2022, 
                                                  min = 2022, max = 2035, sep = ""),
                                      sliderInput(inputId = "end",
                                                  label = "End year of project",
                                                  value = 2027, 
                                                  min = 2023, max = 2045, sep=""),
                                      sliderInput(inputId = "rds",
                                                  label = "Change in number of roads",
                                                  value = 10, 
                                                  min = 0, max = 500),
                                      sliderInput(inputId = "pas",
                                                  label = "Change in number of protected areas (binary)",
                                                  value = 0, 
                                                  min = 0, max = 1),
                                      sliderInput(inputId = "mst",
                                                  label = "Change in mean spring temperature",
                                                  value = 2, 
                                                  min = -10, max = 10),
                                      sliderInput(inputId = "pdsi",
                                                  label = "Change in PDSI",
                                                  value = -2, 
                                                  min = -10, max = 10),
                                      actionButton("runforecastuser", "View Plots",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )),
                           
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Forecasting user inputs",
                                        plotOutput("forecastuser"))
                             )
                           )) 
                )
                )
                
)


server <- function(input, output) {

  
  
  get.realdata <- eventReactive(input$realrun, {
    
    realfunc(if(input$cepf=="Include"){"cepf"}else {"NA"}, 
             if(input$gef=="Include"){"gef"}else {"NA"},
    if(input$worldbank=="Include"){"worldbank"}else {"NA"}, 
    if(input$europeaid=="Include"){"eu"}else {"NA"}, 
    brazil
    )
    
  })
  
  
  get.realparameter <- eventReactive(input$parameterrun, {
    
    parametername <- if(input$parameter=="Years of project"){"yrs.z_txbycont.png"}else if(input$parameter=="Investment per year"){"acc.cost.z_txbycont.png"}else 
      if(input$parameter=="Number of roads"){"rds.z_txbycont.png"}else 
        if(input$parameter=="Protected areas (binary)"){"pas.z_txbycont.png"}else 
          if(input$parameter=="Palmer Drought Severity Index (PDSI)"){"pdsi.z_txbycont.png"}else 
            if(input$parameter=="PDSI rate of change"){"prate.z_txbycont.png"}else 
              if(input$parameter=="Mean spring temperature"){"mst.z_txbycont.png"}else 
                if(input$parameter=="Mean spring temp rate of change"){"mstrate.z_txbycont.png"}else 
                  if(input$parameter=="Elevation"){"elev.z_txbycont.png"}else 
                    if(input$parameter=="Slope"){"slope.z_txbycont.png"}
    
  })
  
  get.foredata <- eventReactive(input$forecastactiontrackrun, {
    
    foreaction(as.numeric(input$ccpainvest),  as.numeric(input$csfinvest), 
               as.numeric(input$csfarm),
               as.numeric(input$restoreinvest), 
               brazil) 
   
   })
  
  get.forecastparameter <- eventReactive(input$runforecastparameter, {
    
    forecastparametername <- if(input$forecastparameter=="Years of project"){"yrs.z"}else if(input$forecastparameter=="Investment per year"){"cost.z"}else 
      if(input$forecastparameter=="Number of roads"){"rds.z"}else 
        if(input$forecastparameter=="Protected areas (binary)"){"pas.z"}else 
          if(input$forecastparameter=="Palmer Drought Severity Index (PDSI)"){"pdsi.z"}else 
            if(input$forecastparameter=="PDSI rate of change"){"prate.z"}else 
              if(input$forecastparameter=="Mean spring temperature"){"mst.z"}else 
                if(input$forecastparameter=="Mean spring temp rate of change"){"mstrate.z"}else 
                  if(input$forecastparameter=="Elevation"){"elev.z"}else 
                    if(input$forecastparameter=="Slope"){"slope.z"}
  })
    
  get.forecastparameterlabel <- eventReactive(input$runforecastparameter, { 
    forecastparameterlabel <- if(input$forecastparameter=="Years of project"){"Years since\nstart of project"}else if(input$forecastparameter=="Investment per yer"){"Investment per year\n($ thousands)"}else 
      if(input$forecastparameter=="Number of roads"){"Number of roads"}else 
        if(input$forecastparameter=="Protected areas (binary)"){"Protected areas\n(binary)"}else 
          if(input$forecastparameter=="Palmer Drought Severity Index (PDSI)"){"PDSI"}else 
            if(input$forecastparameter=="PDSI rate of change"){"Annual PDSI\nrate of change"}else 
              if(input$forecastparameter=="Mean spring temperature"){"Mean spring\ntemperature (ºC)"}else 
                if(input$forecastparameter=="Mean spring temp rate of change"){"Mean spring temp\nrate of change"}else 
                  if(input$forecastparameter=="Elevation"){"Elevation"}else 
                    if(input$forecastparameter=="Slope"){"Slope (*100)"}
    
  })
  
  get.foreuser <- eventReactive(input$runforecastuser, {
    
    foreuser(as.character(input$action), as.numeric(input$investment), as.numeric(input$start), 
             as.numeric(input$end),
             as.numeric(input$rds), as.numeric(input$pas),
             as.numeric(input$mst), as.numeric(input$pdsi),
             brazil) 
    
  })
  
  
  observeEvent(input$parameterrun, {
    parametername <- get.realparameter()#[[1]]
    
    output$parameteroutput <- renderImage({
      
      parameter <- normalizePath(file.path(paste0("figures/", parametername)))
      
      list(src = parameter, width="1000", height="300")
    }, deleteFile = FALSE)
    
    
  })
  
  observeEvent(input$realrun, {
  output$modoutput <- renderImage({
    
    modoutput <- normalizePath(file.path("figures/modoutput_yrs_cost.png"))
    
    list(src = modoutput, width="700", height="400")
  }, deleteFile = FALSE)
  })
  
  
  
  observeEvent(input$realrun, {
    output$realraw <- renderPlot({
      txctl <- get.realdata()[[1]]
      
      intstokeep <- c("climate-critical protected areas", "control climate-critical protected areas", 
                      "climate-smart farming", "control climate-smart farming",
                      "climate-smart forestry: manage", "control climate-smart forestry: manage", 
                      "forest and wetland restoration", "control forest and wetland restoration")
      
      brazil <- brazil[brazil$intervention %in% intstokeep, ]
      txctl <- txctl[txctl$intervention %in% intstokeep, ]
      
      txctl$totcostK <- (ave(txctl$costperyr, txctl$year, 
                             txctl$intervention, FUN = function(x) sum(x, na.rm=TRUE)))/1000
      
      
      cols <-viridis_pal(option="viridis")(4)
      invest <- ggplot(txctl) + 
        geom_smooth(se=TRUE, aes(x=year, y=((totcostK)/1000), col=intervention), 
                    method="auto", level=0.5) +
        theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                             labels=c("climate-critical protected areas"=
                                                        "climate-critical\nprotected areas",
                                                      "climate-smart farming"="cimate-smart\nfarming",
                                                      "climate-smart forestry: manage"="climate-smart\nforestry",
                                                      "forest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Total investments (in millions)") + xlab("")
      
      
      if(FALSE){
      brazil$intnew <- ifelse(brazil$intervention%in%c("climate-critical protected areas", 
                              "control climate-critical protected areas"), "aclimate-critical protected areas",
                              brazil$intervention)
      
      brazil$intnew <- ifelse(brazil$intervention%in%c("climate-smart forestry: manage", 
                              "control climate-smart forestry: manage"), "bclimate-smart forestry: manage",
                              brazil$intnew)
      
      brazil$intnew <- ifelse(brazil$intervention%in%c("climate-smart farming", 
                              "control climate-smart farming"), "cclimate-smart farming",
                              brazil$intnew)
      
      
      brazil$intnew <- ifelse(brazil$intervention%in%c("forest and wetland restoration", 
                              "control forest and wetland restoration"), "dforest and wetland restoration",
                              brazil$intnew)
      
      
      
      growth <- ggplot(brazil, aes(x=year, y=lc_ha_rate, col=intnew)) +
        geom_smooth(method="lm", aes(linetype=as.factor(tx)), level=0.5) + theme_classic() +
        scale_color_manual(name="Intervention", values=cols,
                           labels=c("aclimate-critical protected areas"=
                                        "climate-critical\nprotected areas", 
                                    "cclimate-smart farming"="climate-smart\nfarming",
                                    "bclimate-smart forestry: manage"="climate-smart\nforestry",
                                    "dforest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Annual change in forest cover") + xlab("")  +
        scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                              labels=c("0"="control", "1"="treatment"))
      }
      
      #grid.arrange(invest, growth, ncol=2)
      grid.arrange(invest, ncol=1)
      
    })
    
  })
  
  
  
  
  observeEvent(input$forecastactiontrackrun, {
    output$forecastaction <- renderPlot({
      fore <- get.foredata()[[1]]
      
      preddata <- predict(mod1, fore, allow_new_levels=TRUE, probs=c(0.25, 0.75))
      preddata <- as.data.frame(preddata)
      
      fore <- cbind(fore, preddata)
      fore$lc_ha_rate <- fore$Estimate
      
      controls <- c("control climate-critical protected areas", "control climate-smart forestry: manage", 
                    "control climate-smart farming", "control forest and wetland restoration")
      
      fore$tx <- ifelse(fore$intervention%in%controls, 0, 1)
      
      
      fore$int <- ifelse(fore$intervention %in% c("climate-critical protected areas", 
                                                            "control climate-critical protected areas"),
                             "climate-critical protected areas", NA)
      fore$int <- ifelse(fore$intervention %in% c("climate-smart forestry: manage", 
                                                            "control climate-smart forestry: manage"),
                             "climate-smart forestry: manage", fore$int)
      fore$int <- ifelse(fore$intervention %in% c("climate-smart farming", 
                                                            "control climate-smart farming"),
                             "climate-smart farming", fore$int)
      fore$int <- ifelse(fore$intervention %in% c("forest and wetland restoration", 
                                                            "control forest and wetland restoration"),
                             "forest and wetland restoration", fore$int)
      
      
      
      fore$costperint <- ave(fore$costperyr, fore$int, fore$year, FUN=sum)
      
      cols <-viridis_pal(option="viridis")(4)
      invest.fore <- ggplot(fore) + 
        geom_smooth(se=TRUE, aes(x=year, y=costperint/1000, col=int), 
                    method="auto", level=0.5) +
        theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                             labels=c("climate-critical protected areas"=
                                                        "climate-critical\nprotected areas",
                                                      "climate-smart farming"="climate-smart\nfarming",
                                                      "climate-smart forestry:manage"="climate-smart\nforestry",
                                                      "forest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Total investments ($1K)") + xlab("") +
        theme(legend.position = "none") 
      png("figures/investments_fore.png", width = 640, height = 480, res = 150)
      invest.fore
      dev.off()
      
      
      fore$accumcost.mil <- fore$accum.cost/1000000
      rawinvest <- ggplot(fore[fore$tx==1,], aes(x = accumcost.mil, y = lc_ha_rate, col=intervention)) + 
        geom_smooth(method="lm", level=0.5, aes(fill=intervention)) + theme_classic() + 
        geom_hline(yintercept = 0, linetype="dotted") +
        theme(legend.position = "none") +
        scale_color_manual(name="Intervention",
                           labels=sort(unique(fore$intervention[fore$tx==1])),
                           values=cols) +
        scale_fill_manual(name="Intervention",
                          labels=sort(unique(fore$intervention[fore$tx==1])),
                          values=cols) +
        xlab("Investment (1M USD)") + ylab("Rate of natural land cover change")
      png("figures/rawinvestments_fore.png", width = 640, height = 480, res = 150)
      rawinvest
      dev.off()
      
      rawyears <- ggplot(fore[fore$tx==1,], aes(x = yrsofproject, y = lc_ha_rate, col=intervention)) + 
        geom_smooth(method="lm", level=0.5, aes(fill=intervention)) + theme_classic() + 
        geom_hline(yintercept = 0, linetype="dotted") +
        scale_color_manual(name="Intervention",
                           labels=sort(unique(fore$intervention[fore$tx==1])),
                           values=cols) +
        scale_fill_manual(name="Intervention",
                          labels=sort(unique(fore$intervention[fore$tx==1])),
                          values=cols) +
        xlab("Years of project") + ylab("Rate of natural land cover change")
      png("figures/rawyears_fore.png", width = 640, height = 480, res = 150)
      rawyears
      dev.off()
      
      growth.fore <- ggplot(fore, aes(x=year, y=Estimate, col=int)) +
        geom_smooth(method="lm", aes(linetype=as.factor(tx)), level=0.5) + theme_classic() +
        #geom_ribbon(aes(ymin=Q25, ymax=Q75, col=int, fill=int), 
         #           linetype=0, alpha=0.1) +
        scale_color_manual(name="Intervention", values=cols,
                           labels=c("climate-critical protected areas"=
                                      "climate-critical\nprotected areas",
                                    "climate-smart farming"="climate-smart\nfarming",
                                    "climate-smart forestry:manage"="climate-smart\nforestry",
                                    "forest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Annual change in forest cover") + xlab("")  +
        scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                              labels=c("0"="control", "1"="treatment"))
      
      grid.arrange(invest.fore, rawinvest, rawyears, ncol=3)
    
      
      observeEvent(input$runforecastparameter, {
        output$forecastparameterplots <- renderPlot({
          
        forecastparametername <- get.forecastparameter()#[1]
        forecastparameterlabel<- get.forecastparameterlabel()#[2]
        
        
                bb <- fore
                if(TRUE){
                  bb$itvn <- ifelse(bb$intervention=="control climate-smart forestry: manage", 
                                    "climate-smart forestry: manage (zcontrol)", bb$intervention)
                  bb$itvn <- ifelse(bb$intervention=="control climate-smart farming", 
                                    "climate-smart farming (zcontrol)", bb$itvn)
                  bb$itvn <- ifelse(bb$intervention=="control climate-critical protected areas", 
                                    "climate-critical protected areas (zcontrol)", bb$itvn)
                  bb$itvn <- ifelse(bb$intervention=="control forest and wetland restoration", 
                                    "forest and wetland restoration (zcontrol)", bb$itvn)
                }
                
                
                bb$itvn <- Hmisc::capitalize(bb$itvn)
                
                
                itvns <- sort(unique(bb$itvn))
                predslist <- c("yrs.z", "cost.z", "rds.z", "pas.z",
                               "pdsi.z", "prate.z", "mst.z", "mstrate.z",
                               "elev.z", "slope.z")
                
                
                origparams <- c("yrsofproject", "costperyr", "numrds", "pas",
                                "pdsi", "pdsirate", "mst", "mstrate",
                                "elev", "slope")
                
                
                labs <- c("Years since\nstart of project", "Investment per year\n($ thousands)", "Number of roads",
                          "Protected areas\n(binary)",
                          "PDSI", "Annual PDSI\nrate of change", 
                          "Mean spring\ntemperature (ºC)", "Mean spring temp\nrate of change",
                          "Elevation", "Slope (*100)")
                
                
                itvnnums <- c(1, 3, 5, 7)
                
                bb$costperyr <- bb$costperyr/1000
                
                colz <- rev(brewer.pal(n = 8, name = "Paired"))
                  
                
                   i = forecastparametername 
                   j = forecastparameterlabel
                    for(l in c(1, 3, 5, 7)){ #l=1
                      
                      mylist <- lapply(itvnnums, function(l) {
                        ggplot(bb[bb$itvn %in% itvns[c(l,l+1)],], aes(x=unname(purrr::as_vector(bb[i][bb$itvn %in% itvns[c(l,l+1)],])), 
                                                                      y=bb$Estimate[bb$itvn %in% itvns[c(l,l+1)]], col=bb$itvn[bb$itvn %in% itvns[c(l,l+1)]])) + 
                          geom_smooth(method="lm") + xlab(j) + ggtitle(unique(bb$itvn[bb$itvn %in% itvns[l]])) +
                          ylab("Rate of forest cover change") + theme_classic() + 
                          theme(plot.title = element_text(size=10, face="bold"),
                                legend.position = ifelse(unique(bb$itvn[bb$itvn %in% itvns[l]])=="Forest and wetland restoration", 'right', 'none'), 
                                axis.text=element_text(size=9)) + 
                          scale_y_continuous(expand = c(0, 0)) + 
                          geom_ribbon(aes(ymin=Q25, ymax=Q75, col=itvn, fill=itvn), 
                                      linetype=0, alpha=0.4) +
                          scale_colour_manual(name="Intervention", values=colz[c(l, l+1)],
                                              labels=c("intervention", "control")) +
                          scale_fill_manual(name="Intervention", values=colz[c(l, l+1)],
                                            labels=c("intervention", "control"))
                      })
                    }
                    grid.arrange(grobs = mylist, layout_matrix=rbind(c(1, 1,  2, 2,  3, 3, 4, 4, 4)))
                })
      })
      
    })
      
    })
  
  
  observeEvent(input$runforecastuser, {
    output$forecastuser <- renderPlot({
      foreuser <- get.foreuser()[[1]]
      
      preddata <- predict(mod1, foreuser, allow_new_levels=TRUE, probs=c(0.25, 0.75))
      preddata <- as.data.frame(preddata)
      
      fore <- cbind(foreuser, preddata)
      fore$lc_ha_rate <- fore$Estimate
      
      controls <- c("control climate-critical protected areas", "control climate-smart forestry: manage", 
                    "control climate-smart farming", "control forest and wetland restoration")
      
      fore$tx <- ifelse(fore$intervention%in%controls, 0, 1)
      
      
      fore$int <- ifelse(fore$intervention %in% c("climate-critical protected areas", 
                                                  "control climate-critical protected areas"),
                         "climate-critical protected areas", NA)
      fore$int <- ifelse(fore$intervention %in% c("climate-smart forestry: manage", 
                                                  "control climate-smart forestry: manage"),
                         "climate-smart forestry: manage", fore$int)
      fore$int <- ifelse(fore$intervention %in% c("climate-smart farming", 
                                                  "control climate-smart farming"),
                         "climate-smart farming", fore$int)
      fore$int <- ifelse(fore$intervention %in% c("forest and wetland restoration", 
                                                  "control forest and wetland restoration"),
                         "forest and wetland restoration", fore$int)
      
      
      
      fore$costperint <- ave(fore$costperyr, fore$int, fore$year, FUN=sum)
      
      cols <-viridis_pal(option="viridis")(4)
      invest.fore <- ggplot(fore) + 
        geom_smooth(se=TRUE, aes(x=year, y=costperint/1000, col=int), 
                    method="auto", level=0.5) +
        theme_classic() + scale_color_manual(name="Intervention", values=cols,
                                             labels=c("climate-critical protected areas"=
                                                        "climate-critical\nprotected areas",
                                                      "climate-smart farming"="cimate-smart\nfarming",
                                                      "climate-smart forestry:manage"="climate-smart\nforestry",
                                                      "forest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Total investments ($1K)") + xlab("")
      
      
      fore$accumcost.mil <- fore$accum.cost/1000000
      rawinvest <- ggplot(fore[fore$tx==1,], aes(x = accumcost.mil, y = lc_ha_rate, col=intervention)) + 
        geom_smooth(method="lm", level=0.5, aes(fill=intervention)) + theme_classic() + 
        geom_hline(yintercept = 0, linetype="dotted") +
        scale_color_manual(name="Intervention",
                           labels=sort(unique(fore$intervention[fore$tx==1])),
                           values=cols) +
        scale_fill_manual(name="Intervention",
                          labels=sort(unique(fore$intervention[fore$tx==1])),
                          values=cols) +
        xlab("Investment (1M USD)") + ylab("Rate of natural land cover change")
      
      rawyears <- ggplot(fore[fore$tx==1,], aes(x = yrsofproject, y = lc_ha_rate, col=intervention)) + 
        geom_smooth(method="lm", level=0.5, aes(fill=intervention)) + theme_classic() + 
        geom_hline(yintercept = 0, linetype="dotted") +
        scale_color_manual(name="Intervention",
                           labels=sort(unique(fore$intervention[fore$tx==1])),
                           values=cols) +
        scale_fill_manual(name="Intervention",
                          labels=sort(unique(fore$intervention[fore$tx==1])),
                          values=cols) +
        xlab("Years of project") + ylab("Rate of natural land cover change")
      
      growth.fore <- ggplot(fore, aes(x=year, y=Estimate, col=int)) +
        geom_smooth(method="lm", aes(linetype=as.factor(tx)), level=0.5) + theme_classic() +
        #geom_ribbon(aes(ymin=Q25, ymax=Q75, col=int, fill=int), 
        #           linetype=0, alpha=0.1) +
        scale_color_manual(name="Intervention", values=cols,
                           labels=c("climate-critical protected areas"=
                                      "climate-critical\nprotected areas",
                                    "climate-smart farming"="cimate-smart\nfarming",
                                    "climate-smart forestry:manage"="climate-smart\nforestry",
                                    "forest and wetland restoration"="forest and\nwetland restoration")) +
        ylab("Annual change in forest cover") + xlab("")  +
        scale_linetype_manual(name="Treatment", values=c("dashed", "solid"),
                              labels=c("0"="control", "1"="treatment"))
      
      grid.arrange(invest.fore, ncol=1)
      grid.arrange(rawinvest, rawyears, ncol=2)
      
      
    })
    
  })
  
  
}

shinyApp(ui = ui, server = server)

#runApp("~/Documents/git/ciapp/")