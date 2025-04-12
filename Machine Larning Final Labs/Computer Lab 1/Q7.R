# Q7

# Q7.1

tree_classifier <- tree(spam ~ ., data = train)

tree_classifier_test <- predict(tree_classifier, newdata = test, type = "class")

confusionMatrix(data = tree_classifier_test, test$spam, positive = "spam")

tree_result <- confusionMatrix(data = tree_classifier_test, test$spam, positive = "spam")

# Q7.2

plot(tree_classifier)
text(tree_classifier, pretty = 0)