suppressPackageStartupMessages(library(proceduralnames))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(magick))

word <- make_english_names(1,1)
word_split<- unlist(strsplit(word,""))

snowman1 <- image_read("SnowmanPictures/Snowman1.png")
snowman2 <- image_read("SnowmanPictures/Snowman2.png")
snowman3 <- image_read("SnowmanPictures/Snowman3.png")
snowman4 <- image_read("SnowmanPictures/Snowman4.png")
snowman5 <- image_read("SnowmanPictures/Snowman5.png")
snowman6 <- image_read("SnowmanPictures/Snowman6.png")
snowman7 <- image_read("SnowmanPictures/Snowman7.png")
snowman8 <- image_read("SnowmanPictures/Snowman8.png")
snowman9 <- image_read("SnowmanPictures/Snowman9.png")
snowman10 <- image_read("SnowmanPictures/Snowman10.png")

checker <- function(letters, guess){
  match <- NA
  if (guess%in%word_split){
    match = TRUE
  } else{
    match = FALSE
  }
  return(match) 
}

snowman_selector <- function(guesses){
  if(guesses == 0) {
    plot(snowman1)
  }else if (guesses == 1){
    plot (snowman2)
  }else if (guesses == 2){
    plot (snowman3)
  }else if (guesses == 3){
    plot (snowman4)
  }else if (guesses == 4){
    plot (snowman5)
  }else if (guesses == 4){
    plot (snowman6)
  }else if (guesses == 6){
    plot (snowman7)
  }else if (guesses == 7){
    plot (snowman8)
  }else if (guesses == 8){
    plot (snowman9)
  }else if (guesses == 9){
    plot (snowman10)
  }
}

word_filler <- function(correct_guesses, word){
  b <- character(length(word))
for (i in 1:length(word)){
  if (word[i] %in% correct_guesses){
    b[i] <- word[i]
  }
}
  return(b)
}

incorrect_guesses <- 0
correct_guesses <- 0
correctly_guessed_letters <- character(0)
plot(snowman1)

print("Welcome to the Snowman Game! Its up to you to correctly guess the secret word and save the snowman from a watery end!")
print("Lets begin!")
print(character(length(word_split)))
while ((correct_guesses < length(unique(word_split))) & (incorrect_guesses < 9)) {
  guess <- readline("Enter your guess: ")
  match <- checker(word_split, guess)
      if (match==FALSE){
        print("incorrect") 
        incorrect_guesses <- incorrect_guesses + 1
        print(word_filler(correctly_guessed_letters, word_split))
        snowman_selector(incorrect_guesses)
    } else {
        print("correct")
        correct_guesses <- correct_guesses + 1
        correctly_guessed_letters <- append(correctly_guessed_letters, guess)
        print(word_filler(correctly_guessed_letters, word_split))
  } 
 }
 if (correct_guesses == length(unique(word_split))){
   print("Congradulations! You've saved the snowman!")
   plot(snowman1)
 } else if (incorrect_guesses == 9) {
   print (paste("Oh No! You've melted the snowman! The secret word was", word))
 }
 
again <- readline("Would you like to play again? Enter yes or no ")
if(again == "yes"){source("Hangman_Test.R")}