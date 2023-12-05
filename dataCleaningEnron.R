# This code performs data cleaning for the Enron dataset

# load pacakges
library(plyr)
library(tidyverse)

# load data
load("enron-mysqldump.RData") #http://www.ahschulz.de/enron-email-data/

# subset employeelist to only include ID and emails
emailID <- employeelist |> 
  select(eid, Email_id, Email2, Email3, EMail4)

# convert from wide to long format
emailIDlong <- gather(emailID, email.num, email, Email_id:EMail4, factor_key=TRUE)
emailIDlong <- subset(emailIDlong, emailIDlong$email!="")
emailIDlong <- emailIDlong |> select(eid, email)

# subset to important variables in message
message <- message |> select(mid, sender, date)

# add eid to message and remove all obs without eid 
messageMerged <- merge(message, emailIDlong, by.x = "sender", by.y = "email", all.x = T)
messageMerged <- subset(messageMerged, !is.na(messageMerged$eid))
messageMerged = rename(messageMerged, sender.eid = eid) # rename eid to sender.eid
messageMerged <- messageMerged |>  select(mid, date, sender.eid) # remove sender (email) field

# adding recipient id to recipientInfo dataset
recipientinfo <- merge(recipientinfo, emailIDlong, by.x = "rvalue", by.y = "email", all.x = T)
recipientinfo2 <- subset(recipientinfo, !is.na(recipientinfo$eid)) # remove all obs without eid
recipientinfo2 = rename(recipientinfo2, recipient.eid = eid) # rename to recipient.eid
recipientinfo2 <- recipientinfo2 |> select(rid, mid, rtype, recipient.eid) # remove rvalue variable

# merge date and sender.eid to recipientinfo2
enron <- merge(recipientinfo2, messageMerged, by = "mid", all.x = T)
enronWdate <- subset(enron, !is.na(enron$date)) # removing all obs without date

# why so many obs without date - seems to be messages with sender.eid not in list
length(unique(messageMerged$mid))
length(unique(recipientinfo2$mid))

# checking mid=176816
mid176816 <- subset(messageMerged, messageMerged$mid=="176816") # empty when looking at messageMerged
mid176816.2 <- subset(message, messageMerged$mid=="176816") # also empty?

# cleaning enronWdate df
enronWdate <- enronWdate |> select(rid, mid, sender.eid, date, recipient.eid, rtype)

# #only include email recipients that have mid in set of mid in message2
# mid.list <- unique(messageMerged$mid)
# recipientinfo2 <- subset(recipientinfo2, recipientinfo2$mid %in% mid.list)
# 
# enron2 <- merge(recipientinfo2, messageMerged, by = "mid", all.x = T)

# # Cleaning Enron Dataset
# col_order <- c("rid", "mid", "sender.eid", "date", "recipient.eid", "rtype")
# enron <- enron2[, col_order]

write.csv(enronWdate, "enronClean.csv", row.names = F)


