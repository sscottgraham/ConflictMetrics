# Clinical Trials Sponsorship Dashboard -----------------------------------


require(shiny)
require(shinythemes)
require(shinyBS)
require(visNetwork)
require(dplyr)
require(jsonlite)
require(igraph)
require(DT)
require(tidyr)
require(ggplot2)
require(ggsci)
require(streamgraph)
require(stringr)
require(lubridate)
require(egg)
require(curl)

# Define UI for app
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                
                # Header ------------------------------------------------------------------
                # Application title
                div(style="margin-bottom:10px;",titlePanel("Clinical Trials Sponsorship Network Dashboard")),
                HTML("by"), img(src = "http://conflictmetrics.com/img/logo-sm.png", width = 240), 
                tags$hr(style="border-color: black;"),
                tags$style(HTML(".tabbable > .nav > li > a[data-value='Network Help'] {background-color: #29abe0;   color:white}")),
                HTML("This dashboard allows you to explore clinical trials sponsorship networks and related data. To begin, search for a medical topic (e.g. a drug, drug class, device, or condition). Further details are available under Help (below)."),
                tags$hr(style="border-color: black;"),
                fluidRow(
                    div(style="display: inline-block;vertical-align:top; width: 250px; margin-left: 10px;",uiOutput("idquery"))
                    ,div(style="display: inline-block;vertical-align:middle;width: 150px;  margin-top: 25px;",actionButton("go", "Search"))
                    ,tags$hr(style="border-color: black;")
                ),
                
                fluidRow(
                    div(style="display: inline-block;vertical-align:top;margin-left: 10px;margin-bottom:10px;"
                        ,htmlOutput("text_citation")
                    )
                    , tags$head(tags$style(type="text/css", "
                                         #loadmessage {
                                         position: fixed;
                                         top: 0px;
                                         left: 0px;
                                         width: 100%;
                                         padding: 5px 0px 5px 0px;
                                         text-align: center;
                                         font-weight: bold;
                                         font-size: 100%;
                                         color: #000000;
                                         background-color: #335EFF;
                                         z-index: 105;
                                         }
                                         "))
                    #Loading message 
                    ,conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                      tags$div("Loading...",id="loadmessage"))
                ),
                
                # Tab Panel ---------------------------------------------------------------
                fluidRow(
                    tabsetPanel( id="tabs",
                                 
                                 #Network tab
                                 tabPanel("Sponsorship Network",
                                          sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("layout", label = "Select Layout:", 
                                                                       choices = list("Fruchterman-Reingold"=1, 
                                                                                      "Kamada-Kawai"=2, 
                                                                                      "Davidson-Harel"=3,
                                                                                      "GraphOpt"=4, 
                                                                                      "Circle"=5))
                                                           #,div(style="margin-top: -10px;",HTML("<a href='http://conflictmetrics.com/layouts' target='_blank'>Learn more about layout types</a><br />"))
                                                           ,uiOutput("nodeslide")
                                              ),
                                              mainPanel(
                                                  tags$style(type="text/css",
                                                             ".shiny-output-error { visibility: hidden; }",
                                                             ".shiny-output-error:before { visibility: hidden; }"
                                                  ),
                                                  div(id="plot", style="height:400px;"
                                                      ,visNetworkOutput("network_proxy"))))),
                                 
                                 #Top sponsors 
                                 tabPanel("Top Sponsors", dataTableOutput("sponsors")),
                                 
                                 #Sponsorship Timeline 
                                 tabPanel("Sponsorship Timeline", streamgraphOutput("timeline")),
                                 
                                 #Sponsorship Clases
                                 tabPanel("Sponsor Classes"
                                          ,fluidRow(plotOutput("class_donuts"))),
                                 
                                 #At a Glance
                                # tabPanel("Trials at A Glance"
                                 #         ,fluidRow(plotOutput("condint"))),
                                 
                                 #Trials List
                                 tabPanel("Clinical Trials List", dataTableOutput("rcts")),
                                 
                                 #Help
                                 tabPanel("Help", div(style="width:95%;margin-left:10px;", HTML("<b>ClinicalTrials.gov:</b> Clinicaltrials.gov is the U.S. National Library of Medicine's database of clinical trials conducted globally. U.S. federal law requires registration of clinical trials conducted in the U.S. and/or related to any FDA-regulated products. Complete details on legal requirements are available at: <a href='https://clinicaltrials.gov/ct2/manage-recs/fdaaa' target='_blank'>clinicaltrials.gov/ct2/manage-recs/fdaaa</a>.<br /><br /><b>Reading Network Diagrams:</b> In the network diagrams, all layouts are directed graphs, with arrows indicating connections between clinical trials and sponsors. Each arrow indicates a sponsorship relationship with each clinical trial. Each node is color-coded, per the legend. The larger the node, the more trials that entity sponsors. <a href='http://conflictmetrics.com/reading-networks' target='_blank'>Click here for additional information on reading network diagrams.</a><br /><br /><b>Layout Options:</b>  Initial network plots are created using appropriate force-directed algorithms. Force-directed graphing algorithms are designed to plot a readable network through simulating attractive and repulsive forces between nodes in a network. Force-directed algorithms are especially useful when trying to identify more powerful nodes or node clusters in a network. See the <a href='http://conflictmetrics.com/layouts'>complete layout options documentation</a> for additional details.<br /><br /><b>Out Degree Filter:</b> Filtering results by Out Degree will limit the number of sponsors displayed in the network graph by the number of trials sponsored.<br /></br /><b>Additional Tabs:</b> The Top Sponsors, Sponsorship Timeline, and Trials At A Glance, and Clinical Trials List tabs provide additional data on trials and sponsorship.")) 
                                 ))
                )
)
#)

server <- function(input, output, session) {
    
    options(shiny.sanitize.errors = TRUE)
    
    
    
    # Advanced Reactives ------------------------------------------------------
    
    # Render UI search Query, set default value 
    output$idquery <- renderUI({
        textInput("pname", label="Search ClinicalTrials.Gov:", value = "heart attack")
    })
    
    # Observe event; data update 
    observeEvent(input$go, {
        updateTextInput(session, 'pname', label="Search ClinicalTrials.Gov:", value = input$pname)
        
    })
    
    # Send query to ClinicalTirals.Gov API
    pidInput <- eventReactive(input$go, {
        data <- fromJSON(paste0("https://clinicaltrials.gov/api/query/study_fields?expr="
                                ,str_to_lower(str_replace_all(input$pname," ","+"))
                                ,"&fields=NCTId%2CBriefTitle%2CCondition%2CStartDate%2CCompletionDate%2CLeadSponsorClass%2CLeadSponsorName%2CInterventionName%2CCollaboratorName%2CCollaboratorClass&min_rnk=1&max_rnk=1000&fmt=json"))
        
    })
    
    #Parse JSON and sponsor classes 
    rctInput <- eventReactive(input$go, {
        data <- pidInput()
        rcts_df <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>%
            mutate(across(where(is.list),as.character)) %>% 
            rename(CTG_Class = LeadSponsorClass, Name = LeadSponsorName) %>% 
            mutate(
                CM_Class = ifelse(str_detect(Name,"Hospit"), "Hospital",
                               ifelse(CTG_Class == "NIH", "NIH",
                                      ifelse(str_detect(Name,"Universit|Universid|School|Institut|Colleg"), "University",
                                             str_to_sentence(CTG_Class)))),
                CM_Class = ifelse(CM_Class == "Network" | CM_Class == "Other_gov","Other",
                               ifelse(CM_Class == "Fed", "US Gov", CM_Class))
            )
    })
    
    #Parse JSON for collaborator names & classes
    collabInput <- eventReactive(input$go, {
        data <- pidInput()    
        
        #Parse classes
        collab_classes <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>%
            unnest(CollaboratorClass) %>% 
            select(CTG_Class = CollaboratorClass)
        
        
        #Parse names & add CM_Classes
        collabs <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
            unnest(CollaboratorName) %>% 
            cbind.data.frame(collab_classes) %>% 
            rename(Name = CollaboratorName) %>%   
            mutate(across(where(is.list),as.character)) %>% 
            mutate(
                CM_Class = ifelse(str_detect(Name,"Hospit"), "Hospital",
                               ifelse(CTG_Class == "NIH", "NIH",
                                      ifelse(str_detect(Name,"Universit|Universid|School|Institut|Colleg"), "University",
                                             str_to_sentence(CTG_Class)))),
                CM_Class = ifelse(CTG_Class == "NETWORK" | CTG_Class == "OTHER_GOV" | CTG_Class == "UNKNOWN","Other",
                               ifelse(CTG_Class == "Fed", "US Gov", CM_Class)))
    })
    
    #All Sponsors List
    allsponInput <- eventReactive(input$go, {
        collabs <- collabInput() %>% 
            select(-Rank,-LeadSponsorClass,-LeadSponsorName, -CollaboratorClass)
        
        allspon <- rctInput() %>% 
            select(-Rank,-CollaboratorName,-CollaboratorClass) %>% 
            rbind(collabs) %>% 
            add_count(Name, CTG_Class, CM_Class) %>% 
            rename(Sponsorships = n) %>% 
            arrange(desc(Sponsorships))
    })
    
    
    
    # Render UI
    output$nodeslide <- renderUI({
        top_spon <- allsponInput()
        sliderInput("nodedeg", "Filter Nodes by Out-Degree", min=1, max=max(top_spon$Sponsorships), value=ceiling(.2*max(top_spon$Sponsorships)), step =1, round=TRUE)
    })
    
    
    
    
    # Tab 2: Donuts -----------------------------------------------------------
    
    output$class_donuts <- renderPlot({
        allspon <- allsponInput()
        
        
        ctg_sponsors <- allspon %>% 
            select(Class = CTG_Class) %>% 
            count(Class) %>% 
            mutate(percent = n/nrow(allspon),
                   ymax = cumsum(percent),
                   ymin = c(0, head(ymax, n=-1)),
                   labelPosition = (ymax + ymin) / 2,
                   label = ifelse(percent > .03,round(percent*100, digits = 1),NA)
            )
        
        #Make Chart 
        ctgs_plot <- ggplot(ctg_sponsors, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Class)) +
            geom_rect() +
            geom_text( x=2, aes(y=labelPosition, label=label, color=Class), size=6) + # x here controls label position (inner / outer)
            scale_fill_uchicago()+
            scale_color_uchicago()+
            coord_polar(theta="y") +
            xlim(c(-1, 4)) +
            theme_void() +
            theme(legend.position = 'left', legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", vjust = -2))+
            labs(title = "ClinicalTrials.gov \nSponsor Classes")
        
        
        
        # With Custom Sponsor Classes
        mc_sponsors <- allspon%>% 
            select(Class = CM_Class) %>%
            count(Class) %>% 
            mutate(percent = n/nrow(allspon),
                   ymax = cumsum(percent),
                   ymin = c(0, head(ymax, n=-1)),
                   labelPosition = (ymax + ymin) / 2,
                   label = ifelse(percent > .03,round(percent*100, digits = 1),NA)
            )
        
        #Make Chart 
        cms_plot <- ggplot(mc_sponsors, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Class)) +
            geom_rect() +
            geom_text( x=2, aes(y=labelPosition, label=label, color=Class), size=6) + # x here controls label position (inner / outer)
            scale_fill_jco()+
            scale_color_jco()+
            coord_polar(theta="y") +
            xlim(c(-1, 4)) +
            theme_void() +
            theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", vjust = -2))+
            labs(title = "ConflictMetrics.com \nSponsor Classes", caption = "Note: Unlabled Values are < 1%")
        
        
        ggarrange(ctgs_plot, cms_plot, ncol = 2)
    }    ,res = 96)
    
    
    
    # Tab 3: Conditions & Interventions ---------------------------------------
    # This tab is hidden, pending future development. Inconsistent data structures on clinicaltrials.gov are creating a GIGO problem. 
    
    output$condint <- renderPlot({
        data <- pidInput()
        #Interventions Plot
        int_plot <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
            select(InterventionName) %>% 
            unnest(InterventionName) %>% 
            mutate(InterventionName = str_to_lower(InterventionName)) %>%
            count(InterventionName) %>%
            filter(InterventionName != "placebo") %>% 
            mutate(InterventionName = reorder(InterventionName, n), Count= n) %>% 
            slice(1:10) %>% 
            #plot data
            ggplot(aes(x=InterventionName, y=Count)) +
            # Add column geometry 
            geom_col(fill="dodgerblue4") +
            #Remove the x axis labels 
            xlab(NULL) +
            #Flip coodinates so X data is on the Y axis
            coord_flip() +
            theme_minimal()+
            labs(title = "Top Evaluated Interventions") +
            theme(plot.title = element_text(face = "bold"))
        
        #Conditions Plot
        cond_plot <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
            select(Condition) %>% 
            unnest(Condition) %>% 
            mutate(Condition = str_to_lower(Condition)) %>%
            count(Condition) %>%
            mutate(Condition = reorder(Condition, n), Count= n) %>% 
            slice(1:10) %>% 
            #plot data
            ggplot(aes(x=Condition, y=Count)) +
            # Add column geometry 
            geom_col(fill="dodgerblue4") +
            #Remove the x axis labels 
            xlab(NULL) +
            #Flip coodinates so X data is on the Y axis
            coord_flip() +
            theme_minimal()+
            labs(title = "Top Evaluated Conditions") +
            theme(plot.title = element_text(face = "bold"))
        
        #GROB it 
        ggarrange(int_plot, cond_plot, ncol = 2)
    },res = 96, height = 300)
    
    
    # Tab 3: Timeline ---------------------------------------------------------
    output$timeline <- renderStreamgraph({
        allsponInput() %>% 
            select(Name, StartDate,CompletionDate) %>% 
            add_count(Name) %>% 
            arrange(desc(n)) %>% 
            filter(n>=mean(n)) %>% 
            select(Name, StartDate,CompletionDate) %>% 
            pivot_longer(cols = StartDate:CompletionDate) %>% 
            mutate(value = mdy(value),) %>%
            filter(!is.na(value)) %>%   
            group_by(Name) %>% 
            complete(value = seq.Date(from = min(value),
                                      to = max(value),
                                      by = "year")) %>% 
            mutate(Year = year(value)) %>% 
            count(Sponsor= Name,Year) %>%
            streamgraph("Sponsor", "n", "Year",  interpolate="linear") %>% 
            sg_fill_brewer("RdYlBu")
        
        
    })
    
    # Tab 4: Top Sponsors -------------------------------------------------------
    output$sponsors <- DT::renderDataTable({ 
        top_spon <- allsponInput() %>% 
            select(Name, Class = CM_Class, Sponsorships) %>%
            unique() %>% 
            head(20) %>% 
            datatable(.,rownames= FALSE)
        
    }) 
    
    # Tab 5: Clinical Trials Data -------------------------------------------------------
    output$rcts <- DT::renderDataTable({ 
        rcts_df <- rctInput() %>% 
            select(NCTId,BriefTitle,LeadSponsor = Name,Class = CM_Class ,InterventionName,Condition,StartDate,CompletionDate) %>% 
            mutate_all(~replace(., . == "character(0)", NA)) %>% 
            datatable(.,rownames= FALSE)
        
    })
    
    # Tab 1: Network Diagram {kept at end due to length) --------------------------------------------------
    
    output$network_proxy <- renderVisNetwork({
        allspon <- allsponInput()
        
        #validate
        shiny::validate(
            need(nrow(allspon) != 0, "Data not currently available. Try another search.")
            #need(nrow(edges) !=0, "Processing....")
        )
        
        nctids <- allspon %>% 
            select(id = NCTId, label = BriefTitle) %>% 
            mutate(group = "trial") %>% 
            unique()
        
        sponsors <- allspon %>% 
            select(label= Name, group = CM_Class) %>% 
            unique() %>% 
            mutate(id = paste0("sponsor", row_number()))
        
        edges <- allspon %>% 
            select(NCTId, label = Name, group = CM_Class) %>% 
            full_join(sponsors) %>% 
            select(from=id,to=NCTId)
        
        degree <- edges %>% 
            count(from)
        
        nodes <- sponsors %>% 
            rbind.data.frame(nctids) %>% 
            full_join(degree, by = c("id" = "from")) %>% 
            rename(value = n) %>% 
            mutate(value = replace_na(value,0)) %>% 
            filter(value > input$nodedeg | group=="trial") %>% 
            #filter(value > 3 | group=="trial") %>% 
            mutate(label = strtrim(label,30))
        
        edges <- edges %>% 
            filter(from %in% nodes$id & to %in% nodes$id)
        
        nodes <- nodes %>% 
            filter(id %in% edges$from | id %in% edges$to)
        
        
        #Interactive Network display, see the documentation here: https://datastorm-open.github.io/visNetwork/
        
        visNetwork(nodes, edges, height = "600px", width = "100%") %>%  #Control size of the network display
            addFontAwesome() %>% 
            visLegend(addNodes = list(
                list(label = "Industry", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#CD534CFF")),
                list(label = "NIH", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#EFC000FF")),
                list(label = "US Gov", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#8F7700FF")),
                list(label = "Hospital", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#003C67FF")),
                list(label = "University", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#7AA6DCFF")),
                list(label = "Other", shape = "icon", 
                     icon = list(code = "f041", size = 25, color = "#8686886FF"))), 
                useGroups = FALSE) %>% 
            visGroups(groupname = "Industry" , color = "#CD534CFF") %>% #Customize name and color selection for on industry
            visGroups(groupname = "NIH", color = "#EFC000FF") %>% #Customize name and color selection based on level selection
            visGroups(groupname = "US Gov", color = "#8F7700FF") %>% #Customize name and color selection based on level selection
            visGroups(groupname = "Hospital", color = "#003C67FF") %>% #Customize name and color selection based on level selection
            visGroups(groupname = "Other", color = "#8686886FF") %>% #Customize name and color selection based on level selection
            visGroups(groupname = "University", color = "#7AA6DCFF") %>% #Customize name and color selection based on level selection
            visGroups(groupname = "trial", color = "black") %>% 
            visPhysics(stabilization = FALSE) %>% #Using visPhysics() function, you can play with the physics of the network
            
            #Use igraph layout: With visIgraphLayout(), you can use all available layouts in igraph 
            visIgraphLayout(layout = ifelse(input$layout == "1", 
                                            "layout_with_fr",ifelse(input$layout == "2", 
                                                                    "layout_with_kk", ifelse(input$layout == "3", 
                                                                                             "layout_with_dh", ifelse(input$layout == "4", 
                                                                                                                      "layout_with_graphopt", 
                                                                                                                      "layout_in_circle")))) )  %>%
            visEdges(smooth = FALSE) %>%
            visNodes(scaling = list(min = 10, max = 150), color = list(background = "lightblue"
                                                                       , hover = list(background = "green", border = "red")
            )
            ,shadow = list(enabled = TRUE, size = 10)
            ) %>%
            #Custom options are available using visOptions(), such as Highlight nearest, Select by node id, Select by a column, for more info: https://datastorm-open.github.io/visNetwork/options.html
            visOptions(highlightNearest = TRUE) %>% #Optional highligh option by node selection
            #visOptions(selectedBy = "title") %>% #Optional selection by column to allow search by title or author name on the graph
            #visOptions(selectedBy = "group") %>% #Optional selection by group in the graph
            
            #Control the interactions (i.e. dragNodes, hover, zoom, keyboard functions) of the network with visInteraction():
            visInteraction(dragNodes = TRUE, #nable or not the selection and movement of nodes (click on a node, and move your mouse)
                           dragView = TRUE, #enable or not the movement of the full network (click everywhere except node, and move your mouse) 
                           zoomView = TRUE, #enable or not the zoom (use mouse scroll)
                           hover = TRUE, #hover and hoverConnectedEdges : control hover
                           #hoverConnectedEdges = TRUE,
                           #keyboard = TRUE, tooltipDelay = 0, # enable keyboard manipulation rather than mouse (click on network before)
                           navigationButtons = TRUE)  %>% #display navigation Buttons on the graph display.
            visLayout(randomSeed = 123)   %>%
            visEdges ( arrows = 'to' )
        
    })
    
    
}


shinyApp(ui = ui, server = server)
