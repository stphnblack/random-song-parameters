#generate random song parameters
  #title, track length, time signature, tone, bpm, key, main instruments

#######
#setup - read & prep data
#######

#set seeed
set.seed(2020)

#read csv of song parameters
song_gen <- read.csv("/Users/stephenblack/Downloads/song_gen.csv", stringsAsFactors = FALSE)

#create a vector from each column
maj_min <- song_gen$maj_min
maj_min <- maj_min[maj_min != ""]

adjective <- song_gen$adjective
adjective <- adjective[adjective != ""]

noun <- song_gen$noun
noun <- noun[noun != ""]

tone <- song_gen$tone
tone <- tone[tone != ""]

key <- song_gen$key
key <- key[key != ""]

instrument <- song_gen$instrument
instrument <- instrument[instrument != ""]

#######
#functions for pulling rando info
#######
#title
#there are 3 different possibilities for the title (single word adjective, 
#single word noun, and adjective + noun), but i like adj + noun best so i'm
#assigning 3-5 to it to make it more likely
title_func <- function(){
  title_option <- sample(c(1:5), 1)

  switch (title_option,
    "1" = title_string <- paste(sample(adjective, 1)),
    "2" = title_string <- paste(sample(noun, 1)),
    "3" = title_string <- paste(sample(adjective, 1), sample(noun, 1)),
    "4" = title_string <- paste(sample(adjective, 1), sample(noun, 1)),
    "5" = title_string <- paste(sample(adjective, 1), sample(noun, 1))
  )

  return(title_string)
}



#instruments
inst_func <- function(){
  
  inst_sample <- sample(instrument, 2)
  instrument_string <- paste("created with", inst_sample[1], "and notably accompanied by", inst_sample[2])
  
  return(instrument_string)
}


#length
#arguments are min & max in minutes
length_func <- function(min, max){
  
  seconds_sample <- sample(c(0:59), 1)
  minutes <- sample(c(min:max), 1)
  
  if(seconds_sample == 0 | seconds_sample == 1 | seconds_sample == 2 | seconds_sample == 3 | seconds_sample == 4 | seconds_sample == 5 | seconds_sample == 6 | seconds_sample == 7 | seconds_sample == 8 | seconds_sample == 9){
    seconds <- paste("0", seconds_sample, sep="")
    length_string <- paste("(", minutes, ":", seconds, ")", sep="")
    return(length_string)
  }
  else{
    length_string <- paste("(", minutes, ":", seconds_sample, ")", sep="")
    return(length_string)
  }

  
}


#bpm
#arguments are desired min/max bpm values
bpm_func <- function(min, max){
  bpm <- sample(c(min:max), 1)
  return(bpm)
}

#key & major/minor
#in the csv i have the keys C,D,E,F,G,A,B occur 3x more than the sharps/flats to make them more likely
key_func <- function(){
  key_string <- paste(sample(key, 1), sample(maj_min, 1))
  return(key_string)
}

#time signature
time_func <- function(){
  time_string <- sample(c("4/4", "4/4", "4/4", "4/4", "3/4", "3/4", "6/8"), 1)
  return(time_string)
}

#tone
tone_func <- function(){
  tone_string <- sample(tone, 1)
  return(tone_string)
}


#######
#put it all together - run this for song generation
#######

#how many tracks to generate
numtracks <- 5

for(i in 1:numtracks){
  
  line1 <- paste(i, ": ", title_func(), " ", length_func(1,5), sep = "")
  line2 <- paste("a ", time_func(), " ", tone_func(), " track at ", bpm_func(60, 135), " bpm in ", key_func(),  sep="")
  line3 <- paste(inst_func(), "", sep = "\n \n")
  
  cat(paste(line1,line2,line3, sep="\n"))
}

#add album art
library(jpeg)

album_title <- casefold(title_func(), upper = T)
download.file("https://picsum.photos/900", destfile="tmp.jpeg")
my_image <- readJPEG("tmp.jpeg")
plot(1:500, type='n', main="", xlab="", ylab="", bty="n", yaxt="n", xaxt="n")
par(pty="s")
rasterImage(my_image, 
            xleft=0, xright=500, 
            ybottom=0, ytop=500) ; text(250, 250, labels = album_title, adj = 0.5, col = "white")
  
  