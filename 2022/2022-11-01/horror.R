# Load packages
library(tidyverse)
library(udpipe)

# Read horror movie data and keep only English language titles
horror <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv") %>% 
  mutate(title_lower = tolower(title)) %>% 
  filter(original_language == "en")

# Download UD model for tagging
ud_eng_dl   <- udpipe::udpipe_download_model(language = "english-ewt")
# Load model
ud_eng <- udpipe_load_model("./english-ewt-ud-2.5-191206.udpipe")

# Analyze the horror movie titles and pipe to tibble format
ud_horror <- udpipe_annotate(ud_eng, x = horror$title_lower) %>% 
  as_tibble()

# Function to make horror movie titles
# random=TRUE (default) means random possible words are selected
# random=FALSE means the most frequent words per type are selected
make_title <- function(random=T){
  nouns <- ud_horror %>% 
    filter(upos=="NOUN")
  adjs <- ud_horror %>% 
    filter(upos=="ADJ")
  adps <- ud_horror %>% 
    filter(upos=="ADP")
  dets <- ud_horror %>% 
    filter(upos=="DET")
  if(random){
    ns <- sample(nouns$token, 2)
    n1 <- ns[1]
    n2 <- ns[2]
    a <- sample(adjs$token, 1)
    d <- sample(dets$token, 1)
    p <- sample(adps$token, 1)
  }
  else{
    ns <- nouns %>% 
      count(token, sort=T)
    n1 <- ns[1,]$token
    n2 <- ns[2,]$token
    a <- adjs %>% 
      count(token, sort=T) %>% 
      slice_max(n = 1, order_by = n) %>% 
      select(token)
    d <- dets %>% 
      count(token, sort=T) %>% 
      slice_max(n = 1, order_by = n) %>% 
      select(token)
    p <- adps %>% 
      count(token, sort=T) %>% 
      slice_max(n = 1, order_by = n) %>% 
      select(token)
  }
  return(paste("The", str_to_title(n1), p, d, str_to_title(a), str_to_title(n2), sep = " "))
}

# Function to make horror movie taglines
# random=TRUE (default) means random possible words are selected
# random=FALSE means the most frequent words per type are selected
make_tagline <- function(random=T){
  subjects <- ud_horror %>% 
    filter(xpos %in% c("PRP","NNP")) %>% 
    filter(!is.na(feats)) %>% 
    filter(gsub("Case=Acc","",feats)==feats)
  ings <- ud_horror %>% 
    filter(feats=="VerbForm=Ger")
  if(random){
    subject <- subjects[sample(1:nrow(subjects), 1),]
    ing <- sample(ings$token, 1)
  }
  else{
    subject <- subjects %>% 
      count(feats, token, sort=T) %>% 
      slice_max(n = 1, order_by = n)
    ing <- ings %>% 
      count(token, sort=T) %>% 
      slice_max(n = 1, order_by = n) %>% 
      select(token)
  }
  s <- subject$token
  s_feats <- subject$feats
  verb <- sample(c("is","was","will be"),1)
  if(gsub("Number=Sing","",s_feats)==s_feats){
    verb <- sample(c("are", "were", "will be"),1)
  }
  if(s=="i"){
    verb <- sample(c("am","was","will be"),1)
  }
  return(paste(str_to_title(s), verb, ing, "...", sep = " "))
}

# Fonts
# https://fonts.google.com/specimen/Rubik+Glitch
# https://fonts.google.com/specimen/Rubik+Wet+Paint

# Function to make horror movie poster
make_poster <- function(tagline=T, random=T){
  title_text <- make_title(random = random)
  tagline_text <- ""
  if(tagline){
    tagline_text <- make_tagline(random = random)
  }
  ggplot() +
    geom_text(aes(x=3, y=0, label="Coming November 1st"), size=7, color="grey80", family="Copperplate") +
    labs(title=title_text,
         subtitle=tagline_text,
         caption="Visualization: @c_borstell\nData: TMDB (via Tanya Shapiro & TidyTuesday)\nPackages: {tidyverse,udpipe}") +
    coord_fixed(9/16) +
    theme_void() +
    theme(plot.background = element_rect(color=NA, fill="grey10"),
          plot.title = element_text(family="Rubik Wet Paint Regular", size=16, hjust=.5, color="red", vjust=-1),
          plot.subtitle = element_text(family="Rubik Glitch", size=8, hjust=.8, color="red3", vjust=-10),
          plot.caption = element_text(family="Copperplate", size=5, hjust=.5, color="grey80", vjust=240),
          plot.margin = margin(0,0,-2,0, unit="in"))
}

# Example of calling the function and saving
make_poster()
make_poster(random = F)

# Suggested dimensions for saving
ggsave(width=16, height=9, units="cm", dpi=600)

# Loop 100x and make posters with random titles and taglines
for(n in 1:100){
  make_poster()
  ggsave(paste(path,"/poster_",str_pad(n, 3, pad="0"),".jpg"), width=16, height=9, units="cm", dpi=600)
}
