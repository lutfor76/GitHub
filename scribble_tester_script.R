rm(list = ls())
library(tidyverse)
lutfor_finished <- readRDS(file = "lutfor_finished.rds")
tom_finished_preclean <- readRDS(file = "tom_pre.rds")
tom_finished <- readRDS(file = "tom_finished.rds")

# how many NAs for Income.Total in each?
lutfor_finished_na <- lutfor_finished %>% 
  filter(Income.Total.Disposable < 0)

tom_finished_preclean__na <- tom_finished_preclean %>% 
  filter(Income.Total < 0)

tom_finished__na <- tom_finished %>% 
  filter(Income.Total < 0) 


# how many NAs for Income.Total in each?
lutfor_finished_na2 <- lutfor_finished %>% 
  filter(Expenditure.Total.Essential.LCFS.MART < 0)

tom_finished_preclean__na2 <- tom_finished_preclean %>% 
  filter(Expenditure.Total < 0)

tom_finished__na2 <- tom_finished %>% 
  filter(Expenditure.Total < 0) 


### subset to constituents of income.total.disposable
lutfor_finished_na <- lutfor_finished_na %>% 
  select(starts_with("Income")) 



########## 
Piece1<- readRDS(file = "Piece1.rds")
Piece2<- readRDS(file = "Piece2.rds")
Piece3<- readRDS(file = "Piece3.rds")
Piece4<- readRDS(file = "Piece4.rds")
Piece5<- readRDS(file = "Piece5.rds")

Piece1_neg <- Piece1 %>% 
  filter(Income.Total < 0)

Piece2_neg <- Piece2 %>% 
  filter(Income.Total < 0)

Piece3_neg <- Piece3 %>% 
  filter(Income.Total < 0)

Piece4_neg <- Piece4 %>% 
  filter(Income.Total < 0)

Piece5_neg <- Piece5 %>% 
  filter(Income.Total < 0)








