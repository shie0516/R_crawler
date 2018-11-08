
library(here)
library(rvest)
library(tidyverse)
library(stringr)
library(data.table)
setwd(here())

dir.create("002_tmp")

lastpage <- 161

url <- "https://ck101.com/forum-3419-"
url <- paste0(url,1:lastpage,".html") %>% data.table



for (i in 1:dim(url)[1]) {
  download.file(url = unlist(url[i]), destfile = paste0(".\\002_tmp\\tmp",i,".html"), quiet=TRUE )
  cat(i,"/",dim(url)[1],"\n");Sys.sleep(2)
}


###################################################
##################��z���������###################
###################################################
get_topic_list <- 
  function (toward) {
    i <- toward
    data <- read_html(paste0(".\\002_tmp\\tmp",i,".html"))
    name <- data %>% html_nodes(".xst") %>% html_text() %>% data.table
    author <- apply(name,1,str_match,"�@�̡G(.*)")[2,] %>% data.table
    author <- lapply(author,str_replace_all,"�w����|���ѧ�|�w����|�w����|���姹|�w����|�]|�^|\\(|\\)| ","") %>% unlist %>% data.table
    
    topic <- apply(name,1,str_match,"(]|�j)(.*)(�@�̡G|(.*)�@��:)")[3,] %>% trimws %>% data.table
    link <- data %>% html_nodes(".xst") %>% html_attr("href") %>% data.table
    thread <-link[,lapply(link$.,str_match,"viewthread&tid=(.*)&extra=")][2] %>% t %>%data.table
    page <- data %>% html_nodes(".tps") %>% html_nodes("a") %>% html_attr("href") %>% data.table
    data.page.end <- 
      data.table(
        thread.2 = page[,lapply(.,str_match,"viewthread&tid=(.*)&extra=")][2] %>% unlist,
        page.end = page[,lapply(.,str_match,"page=(.*)")][2] %>% unlist %>% as.numeric %>% data.table
      )[order(thread.2,-page.end..)][!duplicated(thread.2)]
    rm(page)
    
    data.topic <- 
      data.table(thread = thread,name = name,topic = topic,author = author,link = link,
                 thanknum = data %>% html_nodes(".thankNum") %>% html_text() %>% trimws,
                 replaynum = data %>% html_nodes(".replayNum") %>% html_text() %>% trimws,
                 viewnum = data %>% html_nodes(".viewNum") %>% html_text() %>% trimws,
                 lastpost_time = data %>% html_nodes(".lastpost_time") %>% html_text() %>% trimws
      )
    
    data <- merge(data.topic,data.page.end,by.x = "thread.V1",by.y = "thread.2",all.x = T)
    names <- c("thread","name","topic","author","link","thanknum","replaynum","viewnum","lastposttime","pageend")
    setnames(data,names)
    return(data)
  }
###################################################
##################��z���������###################
###################################################

data <- c()
tmpdata <- c()
for (i in 1: lastpage) {
  tmpdata <- get_topic_list(i)
  data <- rbind(data,tmpdata);rm(tmpdata)
  cat(i,"/",lastpage,"\n")
}


data$thanknum <- str_pad(data$thanknum,width = 4,side = "left",pad = "0")