
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)




# Read data ---------------------------------------------------------------

taylor_album_songs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv")



# Process data ------------------------------------------------------------

taylor_keys <- taylor_album_songs |> 
  select(album_release, album_name, track_name, track_number, key, key_name, duration_ms) |> 
  drop_na() |> 
  arrange(album_release) |> 
  mutate(album_number = consecutive_id(album_name)) |> 
  mutate(duration_z = (duration_ms-mean(duration_ms))/sd(duration_ms)) |> 
  mutate(value = case_when(
    duration_z < -.5 ~ "eighth",
    duration_z > 2 ~ "half",
    .default = "quarter"
  )) |> 
  mutate(sh = case_when(
    duration_z < -.5 ~ 19,
    duration_z > 2 ~ 21,
    .default = 19
  )) |> 
  mutate(key_letter = str_sub(key_name, 1, 1),
         key_tag = str_sub(key_name, 2, 2)) |> 
  mutate(word = gsub(" .*", "", track_name)) |> 
  mutate(word = case_when(
    track_number!=1 & word!="I" ~ tolower(word),
    .default = word
  )) |> 
  mutate(word = case_when(
    nchar(word)>8 ~ paste0(str_sub(word, 1, 4), "-\n", str_sub(word, 5, -1)),
    .default = word
  )) |> 
  mutate(stem_y = case_when(
    key > 6 ~ key-7,
    .default = key+6
  )) |> 
  mutate(stem_x = case_when(
    key > 6 ~ -.1,
    .default = .1
  ))

# Create flags and beams
flags_beams <- taylor_keys |> 
  mutate(beam = (value==lead(value) & stem_x==lead(stem_x)) | (value==lag(value) & stem_x==lag(stem_x)))

flags <- flags_beams |> 
  filter(!beam & value=="eighth") |> 
  mutate(yend = case_when(
    stem_x < 0 ~ stem_y-stem_x*30,
    .default = stem_y
  )) |> 
  mutate(stem_y = case_when(
    stem_x < 0 ~ stem_y,
    .default = stem_y-stem_x*30
  )) |> 
  mutate(x = case_when(
    stem_x > 0 ~ track_number+stem_x+.1,
    .default = track_number+stem_x
  )) |> 
  mutate(xend = case_when(
    stem_x < 0 ~ track_number+stem_x+.1,
    .default = track_number+stem_x
  ))

beams <- flags_beams |> 
  mutate(grouping = consecutive_id(beam), .by = album_name) |> 
  filter(all(stem_x>0)|all(stem_x<0), .by = c(album_name, grouping)) |> 
  filter(beam & value=="eighth") |> 
  mutate(no = row_number(), .by = c(album_name, grouping)) |> 
  select(album_name, album_number, track_number, key, stem_x, stem_y, value, no, grouping) |> 
  mutate(xmin = min(track_number)+stem_x,
         xmax = max(track_number)+stem_x,
         ymin = first(stem_y),
         ymax = last(stem_y), .by = c(album_name, grouping)) |> 
  slice(1, .by = c(album_name, grouping))

# Time signature (most are 4/4)
time <- tibble(x=0, y=c(3.5, 7), label=c("4", "4"))

# Correct a flag stem
taylor_keys[3, ]$stem_y <- taylor_keys[3, ]$stem_y-1.5



# Plot data ---------------------------------------------------------------

t <- ggplot() +
  geom_hline(yintercept=seq(1, 10, by=2), color="grey30") +
  geom_text(data=time, aes(x, y, label=label), family="Trattatello", size=5) +
  geom_point(data=taylor_keys, aes(x=track_number, y=key-1, shape=sh)) +
  geom_segment(data=taylor_keys, aes(x=track_number+stem_x, xend=track_number+stem_x, y=key-1, yend=stem_y)) +
  geom_text(data=taylor_keys, aes(x=track_number, y=-4, label=word), size=1.2) +
  geom_segment(data=beams, aes(x=xmin-.02, xend=xmax+.02, y=ymin, yend=ymax), linewidth=1.5) +
  geom_curve(data=flags, aes(x=x, xend=xend, y=stem_y, yend=yend), curvature=.2, linewidth=.7) +
  geom_text(data=taylor_keys, aes(x=track_number-.3, y=key-.5, label=key_tag), family="Trattatello") +
  scale_shape_identity() +
  ylim(-5, 12) +
  labs(x="", y="") +
  facet_wrap(~fct_reorder(gsub(" \\(Taylor's Version\\)", "", album_name), album_number), scales="free", ncol=1) +
  theme_void(base_family="Trattatello") +
  theme(plot.background = element_rect(color="transparent", fill="#FAFAEE"),
        plot.margin = margin(5, 5, 5, 5, unit="mm"),
        strip.clip = "off")

ggsave(".taylor_albums_sheet.jpg", t, width=5.5, height=8, units="in")

sheet <- ggplotGrob(t)

ggplot() +
  ggtext::geom_textbox(aes(x=2.4, y=5, label="Each album as a single line of sheet music.<br><br>The key of each track as a single note.<br><br>Each note's duration as the relative track duration (z scored) across all albums."), 
                       fill="transparent", 
                       box.color="transparent",
                       family="Trattatello", 
                       width = .37,
                       size=12,
                       color="grey10") +
  labs(title="Taylor's Albums (melodized)",
       subtitle="Data: {taylor} via TidyTuesday | Packages: {tidyverse,ggtext} | Visualization: @c_borstell") +
  annotation_custom(grob=sheet, xmin=5, xmax=10, ymin=0, ymax=10) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme_void(base_family="Trattatello") +
  theme(plot.background = element_rect(color="transparent", fill="#FAFAEE"),
        plot.margin = margin(15, 5, 5, 5, unit="mm"),
        plot.title = element_text(hjust=.5, size=rel(6)),
        plot.subtitle = element_text(hjust=.6, vjust=4, size=rel(1.6), color="grey45"))
ggsave("./taylor_albums.jpg", width=11, height=10, units="in")





# Bonus: listen to it! ----------------------------------------------------

# Requires MuseScore

library(gm)

# Create music object
m <- Music()

# Add the notes
m <- m +
  Meter(4, 4) +
  Line(pitches = as.list(paste0(taylor_keys$key_name, "4")), 
       durations = as.list(taylor_keys$value)) 

# Thanks, I hate it!
show(m, to = c("score", "audio"))

