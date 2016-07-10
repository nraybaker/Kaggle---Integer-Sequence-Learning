rm(list = ls())
require(nnet)
require(dplyr)
require(tidyr)
require(magrittr)
require(readr)
setwd("~/R/SequenceKaggle/")

test <- read_csv("test.csv")

lst <- lapply(test$Sequence, strsplit, ",")
lst <- sapply(lst, sapply, as.numeric)




df_preds_indep <- data.frame("Id" = test$Id)
df_preds_indep$Last <- NA
df_preds_indep$Independent <- FALSE
# Finding Independents
for(k in 26354:length(lst)){
  while(length(lst[[k]]) < 2){
    df_preds_indep$Last[k] <- 0
    k <- k + 1
  }
  
  df <- data.frame("One" = 0, "Test" = 0)
  x <- 1
  for(i in 1:(length(lst[[k]]))){
    for(j in 0:1){
      df[x, j + 1] <- lst[[k]][i + j]
    }
    x <- x + 1
  }
  nn <- nnet(Test ~ One, data = df[1:(nrow(df) - 2), ], size = 1, linout = TRUE)
  df_preds_indep$Last[k] <- round(predict(nn, df[(nrow(df) - 1), ]), 0)
  
  if (df_preds_indep$Last[k] == df[nrow(df), 1]){
    df_preds_indep$Independent[k] <- TRUE
  }
  
  print(k)
}  


# Finding 1 Recursives
df_preds_one_cursive <- data.frame("Id" = test$Id)
df_preds_one_cursive$Last <- NA
df_preds_one_cursive$OneCursive <- FALSE

for(k in 78169:length(lst)){
  while(length(lst[[k]]) < 3){
    df_preds_one_cursive$Last[k] <- 0
    k <- k + 1
  }
  
  df <- data.frame("One" = 0, "Two" = 0, "Test" = 0)
  x <- 1
  for(i in 1:(length(lst[[k]]) - 1)){
    for(j in 0:2){
      df[x, j + 1] <- lst[[k]][i + j]
    }
    x <- x + 1
  }
  nn <- nnet(Test ~ One + Two, data = df[1:(nrow(df) - 2), ], size = 2, linout = TRUE)
  df_preds_one_cursive$Last[k] <- round(predict(nn, df[(nrow(df) - 1), ]), 0)
  
  if (df_preds_one_cursive$Last[k] == df[nrow(df), 1]){
    df_preds_one_cursive$OneCursive[k] <- TRUE
  }
  
  print(k)
}  

# Finding 2 Recursives
df_preds_two_cursive <- data.frame("Id" = test$Id)
df_preds_two_cursive$Last <- NA
df_preds_two_cursive$TwoCursive <- FALSE

for(k in 1:length(lst)){
  while(length(lst[[k]]) < 4){
    df_preds_two_cursive$Last[k] <- 0
    k <- k + 1
  }
  
  if(k == 26353){
    k <- k + 1
  }
  
  df <- data.frame("One" = 0, "Two" = 0, "Three" = 0, "Test" = 0)
  x <- 1
  for(i in 1:(length(lst[[k]]) - 2)){
    for(j in 0:3){
      df[x, j + 1] <- lst[[k]][i + j]
    }
    x <- x + 1
  }
  nn <- nnet(Test ~ One + Two + Three, data = df[1:(nrow(df) - 2), ], size = 3, linout = TRUE)
  df_preds_two_cursive$Last[k] <- round(predict(nn, df[(nrow(df) - 1), ]), 0)
  
  if (df_preds_two_cursive$Last[k] == df[nrow(df), 1]){
    df_preds_two_cursive$TwoCursive[k] <- TRUE
  }
  
  print(k)
}


# Finals
df_preds <- data.frame("Id" = test$Id)
df_preds$Last <- NA

for(k in 1:length(lst)){
  while(length(lst[[k]]) < 2){
    df_preds$Last[k] <- 0
    k <- k + 1
  }
  
  if(k == 19911 | k == 26353){
    df_preds$Last[k] <- 0
    k <- k + 1
  }
  
  if(isTRUE(df_preds_indep$Independent[k])){
    df <- data.frame("One" = 0, "Test" = 0)
    x <- 1
    for(i in 1:(length(lst[[k]]))){
      for(j in 0:1){
        df[x, j + 1] <- lst[[k]][i + j]
      }
      x <- x + 1
    }
    nn <- nnet(Test ~ One, data = df[1:(nrow(df) - 1), ], size = 1, linout = TRUE)
    df_preds$Last[k] <- round(predict(nn, df[nrow(df), ]), 0)
  }
  
  else if(isTRUE(df_preds_one_cursive$OneCursive[k])){
    df <- data.frame("One" = 0, "Two" = 0, "Test" = 0)
    x <- 1
    for(i in 1:(length(lst[[k]]) - 1)){
      for(j in 0:2){
        df[x, j + 1] <- lst[[k]][i + j]
      }
      x <- x + 1
    }
    nn <- nnet(Test ~ One + Two, data = df[1:(nrow(df) - 1), ], size = 2, linout = TRUE)
    df_preds$Last[k] <- round(predict(nn, df[nrow(df), ]), 0)
  }
  
  else if(isTRUE(df_preds_two_cursive$TwoCursive[k])){
    df <- data.frame("One" = 0, "Two" = 0, "Three" = 0, "Test" = 0)
    x <- 1
    for(i in 1:(length(lst[[k]]) - 2)){
      for(j in 0:3){
        df[x, j + 1] <- lst[[k]][i + j]
      }
      x <- x + 1
    }
    nn <- nnet(Test ~ One + Two + Three, data = df[1:(nrow(df) - 1), ], size = 1, linout = TRUE)
    df_preds$Last[k] <- round(predict(nn, df[nrow(df), ]), 0)
  }
  
  else{
    df <- data.frame("One" = 0, "Test" = 0)
    x <- 1
    for(i in 1:(length(lst[[k]]))){
      for(j in 0:1){
        df[x, j + 1] <- lst[[k]][i + j]
      }
      x <- x + 1
    }
    nn <- nnet(Test ~ One, data = df[1:(nrow(df) - 1), ], size = 1, linout = TRUE)
    df_preds$Last[k] <- round(predict(nn, df[nrow(df), ]), 0)
  }
  
  print(k)
}




  
write_csv(df_preds, "submission.csv")
