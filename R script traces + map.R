install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("petrinetR")



library(bupaR)

logGoogle$activity <- as.character(logGoogle$activity)
logGoogle$activity
#get the sample data and adjust to event log requirements

gSample <- read.csv('bq-results-20200923-100348-yd39uujtq66q.csv')
gSample$case_id <- as.character(gSample$case_id)
gSample$transaction_time <- as.POSIXct(gSample$transaction_time)

#create bupaR event log

logGoogle <- gSample %>% 
  eventlog(
    case_id = "case_id",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "transaction_time",
    resource_id = "resource"
  )

logGoogle %>% group_by_activity %>%
  n_cases

#create event map

logGoogle %>% 
  filter_activity (c("Checkout", "Product view","Add to cart" "Transaction", "content page", "product selector page", "content page", "homepage")) %>% 
  process_map(type_nodes = frequency('absolute', color_scale = "OrRd"),type_edges = frequency('relative_case'),rankdir = "TB")

#create traces file
tra <- logGoogle %>% traces 
write.csv(tra, file = "VIC traces.csv")


####others
logGoogle %>% activity_frequency(level = "activity")

logGoogle$case_id == '1000254393998610432'

logGoogle %>% 
  trace_explorer(coverage = 0.7)


logGoogle %>% 
  filter_activity (c("Checkout", "Product view","Add to cart" "Transaction", "content page", "product selector page", "content page", "homepage")) %>% 
  process_map(type_nodes = frequency('absolute', color_scale = "OrRd"),type_edges = frequency('relative_case'),rankdir = "TB")



logGoogle %>% 
  filter_activity (c("Add to cart", "Checkout", "Product view", "Session started", "Transaction", "content page", "product selector page", "product results page", "content page", "homepage", "internal search")) %>% 
  activities

logGoogle %>% 
  group_by_case() %>%
  first_n(3) %>%
  trace_explorer(coverage = 0.95)

  
#process map for visualising user flows

logGoogle %>% 
  filter_activity (c("Add to cart", "Checkout", "Product view", "Session started", "Transaction", "content page", "product selector page", "product results page", "content page", "homepage", "internal search")) %>%
  process_map(type_nodes = frequency('absolute', color_scale = "OrRd"),type_edges = frequency('relative_case'),rankdir = "TB")

logGoogle %>% 
  filter_activity_frequency(perc = 0.90) %>%
  process_map(type = frequency('absolute', color_scale = "OrRd"))

logGoogle %>% 
  filter_activity_presence(activities = "Checkout") %>%
  process_map(type = frequency('absolute', color_scale = "OrRd"))

#trace explorer - most popular paths in data 

logGoogle %>% 
  filter_activity(c("Checkout", "Product view", "Session started", "Transaction", "content page", "product selector page", "content page", "homepage")) %>%
  plotly_trace_explorer(type = 'frequent', coverage = 0.80) %>% 
  export_graph("traceExplorer.png")

#precendence matrix - how often one step preceedes the other

logGoogle %>% 
  precedence_matrix(type = "relative-consequent") %>%
  plot
