#SVM二元分类器

library(e1071)
data(cats, package=”MASS”)#MASS包用于统计
inputData <- data.frame(cats[, c(2,3)], response = as.factor(cats$Sex))#response as factor

#线性支持向量机 linear SVM
#kernel指的是支持向量机的类型（可以是线性SVM，多项式SVM，径向SVM或Sigmoid SVM），cost是违反约束时的成本函数，gamma是除了linearSVM外其他SVM都使用的一个参数
#对于分类问题一定要把响应变量作为一个因子

svmfit <- svm(response ~ ., data = inputData, kernel = “linear”, cost = 10, scale = FALSE) # linear svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table(inputData$response, predict(svmfit))# tabulate
mean(inputData$response != predict(svmfit)) # 19.44% misclassification error

#径向支持向量机 radial SVM
#可以通过设置内核参数作为“radial”来使用，当使用一个带有“radial”的内核时，结果中的超平面就不需要是一个linear的，通常定义一个弯曲的区域来界定类别之间的分割同时导致相同的训练数据准确度更高

svmfit <- svm(response ~ ., data = inputData, kernel = "radial", cost = 10, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 18.75% misclassification error

#寻找最优参数
#使用tune.svm()函数来寻找svm()函数的最优参数

# Prepare training and test data
set.seed(100) # for reproducing results产生随机数
rowIndices <- 1 : nrow(inputData) # prepare row indices准备行的索引
sampleSize <- 0.8 * length(rowIndices) # training sample size 训练样本大小
trainingRows <- sample (rowIndices, sampleSize) # random sampling随机抽样
trainingData <- inputData[trainingRows, ] # training data 训练数据
testData <- inputData[-trainingRows, ] # test data测试数据
tuned <- tune.svm(response ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune调参
summary (tuned) # to select best gamma and cost
svmfit <- svm (response ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.001, scale = FALSE) # radial svm, scaling turned OFF
print(svmfit)
plot(svmfit, trainingData)
compareTable <- table (testData$response, predict(svmfit, testData))  # comparison table
mean(testData$response != predict(svmfit, testData)) # 13.79% misclassification error
