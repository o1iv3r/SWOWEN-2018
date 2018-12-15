# first vizualizations

##### Init #####

library(data.table)
library(lubridate)
library(stringr)
library(forcats)

##### Load data #####

dm <- fread(file=here::here("data/processed/SWOW-EN.R100.csv")) # ,nrows=1e5


##### Process data #####

dm[,V1:=NULL]

# check classes
sapply(dm,class)

# convert character to factor
# dm[,mget(names(sapply(dm, class)=="character"))]
chr2fct <- function(x) {
  if (class(x)=="character") {
    x=factor(x)
  }
  return (x)
  
}
dm <- lapply(dm,chr2fct)
setDT(dm)

# date should be data
dm[,created_at_date:=as_date(created_at)] # possible to use with google correlate


##### Plotly #####

##### Depth one #####
var_name_start <- "consult"
data2plot <- dm[cue==var_name_start,]
data2plot <- data2plot[,.N,by=R1][order(-N),]

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(var_name_start, as.character(data2plot$R1)),
    # color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = rep(0,data2plot[,.N]),
    target = 1:data2plot[,.N],
    value =  data2plot[,N]
  )
) %>% 
  layout(
    title = paste("First association with ",var_name_start),
    font = list(
      size = 10
    )
  )
p

##### Depth 2 ####

var_name_start <- "consult"
data2plot <- dm[cue==var_name_start,]
data2plot <- data2plot[,.N,by=R1][order(-N),]

# go one further
thresh_for_seq <- 6
next_cue <- data2plot[N>=thresh_for_seq,R1]

dataStep2 <- dm[cue %in% next_cue,]
dataStep2 <- dataStep2[,.N,by=.(cue,R1)][order(-N),]

plotly_label <- c(var_name_start, as.character(data2plot$R1),as.character(dataStep2$R1))

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = plotly_label,
    # color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(rep(0,data2plot[,.N]),rep(1,dataStep2[,.N])),
    target = c(1:data2plot[,.N],56:85),
    value =  c(data2plot[,N],dataStep2[,N])
  )
) %>% 
  layout(
    title = paste("First association with ",var_name_start),
    font = list(
      size = 10
    )
  )
p


##### Limit results -> Other is large ####

# seq_depth <- 2

limit_results <- 30

var_name_start <- "consult"
data2plot <- dm[cue==var_name_start,]
data2plot <- data2plot[,.N,by=R1][order(-N),]

relevant_words <- data2plot[1:limit_results,R1]

data2plot[,relevant:=fct_other(R1,keep=relevant_words)]
data2plot <- data2plot[,.(N=sum(N)),by=relevant]

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(var_name_start, as.character(data2plot$relevant),as.chara),
    # color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = rep(0,data2plot[,.N]),
    target = 1:data2plot[,.N],
    value =  data2plot[,N]
  )
) %>% 
  layout(
    title = paste("First association with ",var_name_start),
    font = list(
      size = 10
    )
  )
p


# # Create a shareable link to your chart
# # Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="sankey-basic-example")
# chart_link


#### Questions ####

# Sequence of first associations?
# Or all associations?
# use arules package
