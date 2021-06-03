library(dplyr)

features <- read.table("features.txt", col.names = c("n","functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("test//subject_test.txt", col.names = "subject")
x_test <- read.table("test//X_test.txt", col.names = features$functions)
y_test <- read.table("test//y_test.txt", col.names = "code")
subject_train <- read.table("train//subject_train.txt", col.names = "subject")
x_train <- read.table("train//X_train.txt", col.names = features$functions)
y_train <- read.table("train//y_train.txt", col.names = "code")

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
merged <- cbind(subject, Y, X)

colnames(merged)[3:563] <- features[[2]]

tidy <- merged %>% select(subject, code, contains('mean'), contains('std'))
tidy$code <- activities[tidy$code,2]

names(tidy)[2] = "activity"

data <- tidy %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

write.table(data, "data.txt", row.name=FALSE)