rm(list=ls()) # Remove variables 
cat("\014") # Clean workspace

# Q6

load(file = 'spam_ham_emails.RData')
Spam_ham_emails[, -1] <- scale(Spam_ham_emails[, -1])
Spam_ham_emails['spam'] <- as.factor(Spam_ham_emails['spam'] == 1) # Changing from 1->TRUE, 0->FALSE
levels(Spam_ham_emails$spam) <- c("not spam", "spam")
head(Spam_ham_emails)

print(paste("Percentage of spam:", 100*mean(Spam_ham_emails$spam == "spam")))

set.seed(1234)
suppressMessages(library(caret))

train_obs <- createDataPartition(y = Spam_ham_emails$spam, p = .75, list = FALSE)
train <- Spam_ham_emails[train_obs, ]
test <- Spam_ham_emails[-train_obs, ]
# Confirm both training and test are balanced with respect to spam emails
print(paste("Percentage of training data consisting of spam emails:", 
            100*mean(train$spam == "spam")))

print(paste("Percentage of test data consisting of spam emails:", 
            100*mean(test$spam == "spam")))

glm_fit <- glm(spam ~ ., family = binomial, data = train)

y_prob_hat_test <- predict(glm_fit, newdata = test, type = "response")
threshold <- 0.5 # Predict spam if probability > threshold
y_hat_test <- as.factor(y_prob_hat_test > threshold)
levels(y_hat_test) <- c("not spam", "spam")
confusionMatrix(data = y_hat_test, test$spam, positive = "spam")

# Q6.1

confusion_matrix <- table(y_hat_test, test$spam)

# Q6.2

classification_accuracy <- (confusion_matrix[2,2] + confusion_matrix[1,1])/nrow(test)

precision <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[2,1])

recall <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])

specificity <- confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[2,1])

# 6.3

library(pROC)

roc = roc(test$spam, y_prob_hat_test)

plot(roc, print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "cornflowerblue", print.thres = "best")