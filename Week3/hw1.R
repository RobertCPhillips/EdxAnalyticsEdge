songs <- read.csv("songs.csv")
str(songs)

#- year = the year the song was released
#- songtitle = the title of the song
#- artistname = the name of the artist of the song
#- songID and artistID = identifying variables for the song and artist
#- timesignature and timesignature_confidence = a variable estimating the time signature of the song, and the confidence in the estimate
#- loudness = a continuous variable indicating the average amplitude of the audio in decibels
#- tempo and tempo_confidence = a variable indicating the estimated beats per minute of the song, and the confidence in the estimate
#- key and key_confidence = a variable with twelve levels indicating the estimated key of the song (C, C#, . . ., B), and the confidence in the estimate
#- energy = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudness
#- pitch = a continuous variable that indicates the pitch of the song
#- timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, . . . , timbre_11_min, and timbre_11_max = variables that indicate the minimum/maximum values over all segments for each of the twelve values in the timbre vector (resulting in 24 continuous variables)
#- Top10 = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 if it was in the top 10, and 0 if it was not)
q1 <- sum(songs$year == 2010)
q2 <- sum(songs$artistname == 'Michael Jackson')

q3 <- subset(songs, artistname == 'Michael Jackson' & Top10 == 1)
q3$songtitle

q4 <- unique(songs$timesignature)
table(songs$timesignature)

q5 <- subset(songs, tempo == max(songs$tempo))$songtitle
q5

#----------------------
# part 2
#----------------------
songs.train <- subset(songs, year <= 2009)
songs.test <- subset(songs, year == 2010)

nonvars <- c("year", "songtitle", "artistname","songID","artistID")

songs.train <- songs.train[, !(names(songs.train) %in% nonvars)]
songs.test <- songs.test[, !(names(songs.test) %in% nonvars)]

songs.mod1 <- glm(Top10 ~ ., data=songs.train, family="binomial")
summary(songs.mod1)

#----------------------
# part 3
#----------------------
cor(songs$loudness , songs$energy)

songs.mod2 <- glm(Top10 ~ . - loudness, data=songs.train, family="binomial")
summary(songs.mod2)

songs.mod3 <- glm(Top10 ~ . - energy, data=songs.train, family="binomial")
summary(songs.mod3)

songs.mod3.predict <- predict(songs.mod3, songs.test,type="response")
t1 <- table(songs.test$Top10, songs.mod3.predict >= .45)
t1
songs.mod3.acc <- (t1[1,1]+t1[2,2])/sum(t1)

t2 <- table(songs.test$Top10)
t2
t2[1]/sum(t2)


songs.mod3.sens <- t1[2,2]/(t1[2,2] + t1[2,1])
songs.mod3.spec <- t1[1,1]/(t1[1,1] + t1[1,2])





