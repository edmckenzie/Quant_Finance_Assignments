
library(class)

knn.trial <- knn(normalized_train, normalized_test, cl = train$spam, k = 21)
cm <- table(y_test, knn.trial)

comparison <- knn_manual(normalized_train, normalized_test, y_train, 21)

cm2 <- table(y_test, comparison)
