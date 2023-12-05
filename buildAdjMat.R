# This code creates the Enron email adjacency matrix

# load packages
library(plyr)
library(tidyverse)
library(lubridate)

# read in clean email data
enron <- read_csv("enronClean.csv")

# adding month/year variable to enron dataset
enron$month = as.numeric(str_sub(enron$date, 6,7))
enron$year = as.numeric(str_sub(enron$date, 1,4))

# lets do just from june 1999 to may 2002
enron36 <- subset(enron, enron$date >= "1999-06-01" & enron$date <= "2002-05-31")

# creating month.num variable
enron36 <- enron36 |> 
  mutate(month.num = (year*12 + month) - (1999*12 + 6) + 1)

# creating matrix of all sender/recipient combinations and number of correspondences
length(unique(enron$sender.eid)); length(unique(enron$recipient.eid)) # different number of senders/recipients
ids <- sort(unique(enron$sender.eid)) #use sender id list to create adj matrix
num.ids = length(ids) # 147 unique senders
enron36 <- subset(enron36, (enron36$sender.eid %in% ids) & (enron36$recipient.eid %in% ids))

# test with just a random month
test.adj.mat = matrix(nrow = num.ids, ncol = num.ids)
colnames(test.adj.mat) <- ids
rownames(test.adj.mat) <- ids

enron36.month15 <- subset(enron36, enron36$month.num==15)

for (i in 1:num.ids) {
  for (j in 1:num.ids) {
    test.adj.mat[i, j] = sum((enron36.month15$sender.eid==ids[i] & 
                                   enron36.month15$recipient.eid==ids[j]) |
                                  (enron36.month15$sender.eid==ids[j] & 
                                     enron36.month15$recipient.eid==ids[i]))
  }
}

# creating a list to store all 36 adjacency matrices
enron.mats <- list()
for (i in 1:max(enron36$month.num)) {
  enron.mats[[i]] <- matrix(numeric(0), num.ids, num.ids, dimnames = list(ids, ids))
}

for (m in 1:max(enron36$month.num)) {
  enron.month <- subset(enron36, enron36$month.num==m)
  for (i in 1:num.ids) {
    for (j in 1:num.ids) {
      enron.mats[[m]][i, j] = sum((enron.month$sender.eid==ids[i] & enron.month$recipient.eid==ids[j]) |
                                    (enron.month$sender.eid==ids[j] & enron.month$recipient.eid==ids[i]))
    }
  }
}

# make the diag of each matrix zero
for (i in 1:length(enron.mats)) {
  diag(enron.mats[[i]]) <- 0
}

saveRDS(enron.mats, "enronAdjMats.RDS")


