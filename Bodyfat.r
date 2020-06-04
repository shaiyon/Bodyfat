# Load in the data
bodyfat <- read.csv("bodyfat.csv", header=FALSE)
colnames(bodyfat) <- c("Density", "Bodyfat", "Age", "Weight", "Height", "NeckC", "ChestC", "AbdomenC", "HipC", "ThighC", "KneeC", "AnkleC", "BicepsC", "ForearmC", "WristC")

# Correlation matrix
cor(bodyfat)

# Remove density
bodyfat <- subset(bodyfat, select = -c(Density))

# Scatterplot matrix
pairs(bodyfat)

# Remove row containg 0 value
bodyfat <- bodyfat[-which(bodyfat$Bodyfat == 0),]

model <- lm(Bodyfat ~ Age + Weight + Height +
              NeckC + ChestC + AbdomenC + HipC +
              ThighC + KneeC + AnkleC + BicepsC + ForearmC + WristC, data=bodyfat)

# Stepwise regression (AIC)
reduced <- lm(Bodyfat ~ 1, data = bodyfat)
step(reduced, scope = list(lower = reduced, upper = model))

newModel <- lm(formula = Bodyfat ~ AbdomenC + Weight + WristC + ForearmC +
                 NeckC + Age + ThighC + HipC, data = bodyfat)

# We will chose newModel as our final model, as it has a higher adjusted R squared.
summary(newModel)
summary(newModel2)

# Regsubsets Function (results agree with AIC model)
library(leaps)
mod <- regsubsets(subset(bodyfat, select=-c(Bodyfat)), bodyfat$Bodyfat)
summary.mod <- summary(mod)
summary.mod$which
summary.mod
summary.mod$adjr2

# Q-Q Plot
qqnorm(newModel$residuals)
qqline(newModel$residuals, col="red")

# Histogram
hist(newModel$residuals, main="Histogram of Residuals", xlab="Residuals")

# Residual vs Fit
plot(x=newModel$fitted.values, y=newModel$residuals, main="Residual vs Fit", xlab="Fitted values", ylab="Residuals")
abline(0,0, col="red")

# Boxcox
library(MASS)
bc <- boxcox(newModel)
lambda <- bc$x[which(bc$y == max(bc$y))]
lambda

# F-test of overall significance
anova(model, newModel)

# Extract equation of final model
coefficients <- newModel$coefficients
(eqn <- paste("Bodyfat =", paste(round(coefficients[1],2), paste(round(coefficients[-1],2), names(coefficients[-1]), sep=" * ", collapse=" + "), sep=" + "), "+ e"))

# Figure 8 code
cat("Range of Age:", range(bodyfat$Age), "    Variance:", var(bodyfat$Age))
cat("Range of Forearm circumference:", range(bodyfat$ForearmC), "   Variance:", var(bodyfat$ForearmC))
cat("Range of Thigh circumference:", range(bodyfat$ThighC), "   Variance:", var(bodyfat$ThighC))
