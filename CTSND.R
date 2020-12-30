#Load Libraries 
require(jsonlite)
require(igraph)
require(dplyr)
require(visNetwork)
require(stringr)
require(tidyr)
require(ggplot2)
require(ggsci)
require(streamgraph)
require(lubridate)
require(egg)


entry <- "insulin"
search_term <- str_to_lower(str_replace_all(entry," ","+"))
degree_filter <- 30


# API query to clinicaltrials.gov -----------------------------------------

data <- fromJSON(paste0("https://clinicaltrials.gov/api/query/study_fields?expr=",search_term,"&fields=NCTId%2CBriefTitle%2CCondition%2CStartDate%2CCompletionDate%2CLeadSponsorClass%2CLeadSponsorName%2CInterventionName%2CCollaboratorName%2CCollaboratorClass&min_rnk=1&max_rnk=1000&fmt=json"))


#Parse JSON and sponsor classes for lead sponsors 

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

#Parse JSON for collaborator classes
collab_classes <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>%
  unnest(CollaboratorClass) %>% 
  select(CTG_Class = CollaboratorClass)

#Parse JSON for collaborator names 
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
                   ifelse(CTG_Class == "Fed", "US Gov", CM_Class))
  )

#All Sponsors List
collabs <- collabs %>% 
  select(-Rank,-LeadSponsorClass,-LeadSponsorName, -CollaboratorClass)

allspon <- rcts_df %>% 
  select(-Rank,-CollaboratorName,-CollaboratorClass) %>% 
  rbind(collabs) %>% 
  add_count(Name, CTG_Class, CM_Class) %>% 
  rename(Sponsorships = n) %>% 
  arrange(desc(Sponsorships))

# Create Donuts Charts ----------------------------------------------------

#With Canonical Classes 
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

#With Custom Sponsor Classes
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

# Create Condition and Intervention plots ---------------------------------

#Total N of interventions 
n_interventions <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
  select(InterventionName) %>% 
  unnest(InterventionName) %>% 
  mutate(InterventionName = str_to_lower(InterventionName)) %>% 
  count(InterventionName) %>%
  filter(InterventionName != "Placebo") %>% 
  nrow()

#Intervention Chart 
int_plot <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
  select(InterventionName) %>% 
  unnest(InterventionName) %>% 
  mutate(InterventionName = str_to_lower(InterventionName)) %>%
  count(InterventionName) %>%
  filter(InterventionName != "placebo") %>% 
  mutate(InterventionName = reorder(InterventionName, n), Count= n) %>% 
  top_n(10) %>% 
  #plot data
  ggplot(aes(x=InterventionName, y=Count)) +
  # Add column geometry 
  geom_col(fill="dodgerblue4") +
  #Remove the x axis labels 
  xlab(NULL) +
  #Flip coodinates so X data is on the Y axis
  coord_flip() +
  theme_minimal()+
  labs(title = "Top Evaluted Interventions") +
  theme(plot.title = element_text(face = "bold"))

#Total N of conditions 
n_conditions <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
  select(Condition) %>% 
  unnest(Condition) %>%
  mutate(Condition = str_to_lower(Condition)) %>%
  count(Condition) %>%
  nrow()

#Condition Chart
cond_plot <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
  select(Condition) %>% 
  unnest(Condition) %>% 
  mutate(Condition = str_to_lower(Condition)) %>%
  count(Condition) %>%
  mutate(Condition = reorder(Condition, n), Count= n) %>% 
  top_n(10) %>% 
  #plot data
  ggplot(aes(x=Condition, y=Count)) +
  # Add column geometry 
  geom_col(fill="dodgerblue4") +
  #Remove the x axis labels 
  xlab(NULL) +
  #Flip coodinates so X data is on the Y axis
  coord_flip() +
  theme_minimal() +
  labs(title="Top Evaluated Conditions") +
  theme(plot.title = element_text(face = "bold"))

ggarrange(int_plot, cond_plot, ncol = 2)

# Prepare network diagram -------------------------------------------------

#Will have to unnest each individually and do a JOIN on NCT ID
collab_classes <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>%
  unnest(CollaboratorClass) %>% 
  select(Class = CollaboratorClass)


collab_names <- as.data.frame(data$StudyFieldsResponse$StudyFields) %>% 
  unnest(CollaboratorName) %>% 
  cbind.data.frame(collab_classes) %>% 
  select(NCTId, label = CollaboratorName, Class) %>%   
  mutate(across(where(is.list),as.character)) %>% 
  mutate(
    group = ifelse(str_detect(label,"Hospit"), "Hospital",
                   ifelse(Class == "NIH", "NIH",
                          ifelse(str_detect(label,"Universit|Universid|School|Institut|Colleg"), "University",
                                 str_to_sentence(Class)))),
    group = ifelse(Class == "NETWORK" | Class == "OTHER_GOV" | Class == "UNKNOWN","Other",
                   ifelse(Class == "Fed", "US Gov", group))
    )
  

nctids <- rcts_df %>% 
  select(id = NCTId, label = BriefTitle) %>% 
  mutate(group = "trial")

collabs <- collab_names %>% 
  select(label, group) %>% 
  unique()

sponsors <- rcts_df %>% 
  select(label= LeadSponsorName, group = Class) %>% 
  rbind(collabs) %>% 
  unique() %>% 
  mutate(id = paste0("sponsor", row_number()))

edges <- rcts_df %>% 
  select(NCTId, label = LeadSponsorName, Class= LeadSponsorClass, group = Class) %>% 
  rbind(collab_names) %>% 
  full_join(sponsors) %>% 
  select(from=id,to=NCTId)

degree <- edges %>% 
  count(from)

nodes <- sponsors %>% 
  rbind.data.frame(nctids) %>% 
  full_join(degree, by = c("id" = "from")) %>% 
  rename(value = n) %>% 
  mutate(value = replace_na(value,0)) %>% 
  filter(value > degree_filter | group=="trial") %>% 
  mutate(label = strtrim(label,30))

edges <- edges %>% 
  filter(from %in% nodes$id & to %in% nodes$id)

nodes <- nodes %>% 
  filter(id %in% edges$from | id %in% edges$to)

#visNetwork(nodes, edges, width = "100%")

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
  #visLegend(width = 0.1, position = "left", main = "test")  %>%   #Display or mute Legend based on groups, using visLegend. Use options to add a title, adjust size or choose the position from the labels
  visGroups(groupname = "Industry" , color = "#CD534CFF") %>% #Customize name and color selection for on industry
  visGroups(groupname = "NIH", color = "#EFC000FF") %>% #Customize name and color selection based on level selection
  visGroups(groupname = "US Gov", color = "#8F7700FF") %>% #Customize name and color selection based on level selection
  visGroups(groupname = "Hospital", color = "#003C67FF") %>% #Customize name and color selection based on level selection
  visGroups(groupname = "Other", color = "#8686886FF") %>% #Customize name and color selection based on level selection
  visGroups(groupname = "University", color = "#7AA6DCFF") %>% #Customize name and color selection based on level selection
    visGroups(groupname = "trial", color = "black") %>% 
  addFontAwesome() %>%
  visPhysics(stabilization = FALSE) %>% #Using visPhysics() function, you can play with the physics of the network
  #Use igraph layout: With visIgraphLayout(), you can use all available layouts in igraph 
  visIgraphLayout(layout = "layout_nicely" )  %>%
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


# Streamgraph -------------------------------------------------------------

stream_df <- rcts_df %>% 
  select(LeadSponsorName, StartDate,CompletionDate) %>% 
  pivot_longer(cols = StartDate:CompletionDate) %>% 
  mutate(value = mdy(value),) %>%
  filter(!is.na(value)) %>%   
  group_by(LeadSponsorName) %>% 
  complete(value = seq.Date(from = min(value),
                                             to = max(value),
                                             by = "year")) %>% 
  mutate(Year = year(value) ) %>% 
  count(LeadSponsorName,Year) %>%
  arrange(desc(n)) %>% 
  head(25)


stream_df %>% streamgraph("LeadSponsorName", "n", "Year",  interpolate="linear") %>% 
  sg_fill_brewer("RdYlBu") %>% 
  sg_legend(show=TRUE, label="Sponsors: ")



# Streamgraph 2.0 ---------------------------------------------------------


rcts_df_sg <- rcts_df %>% 
  select(NCTId, Name, StartDate,CompletionDate)

NCTId_Dates <- rcts_df %>% 
  select(NCTId, StartDate,CompletionDate)

collabs_sg <- collabs %>% 
  inner_join(NCTId_Dates) %>% 
  select(Name, StartDate, CompletionDate)

rcts_df %>% 
  select(Name, StartDate,CompletionDate) %>% 
  rbind(collabs_sg) %>% 
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

