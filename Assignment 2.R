library("tidyverse")

# a) read dataset into a variable
videos <- read_csv("videos.csv")

x<-videos$`Song Length`
y<-videos$`File Size`
#Question 1 a
cor(videos$`File Size`,videos$`Song Length`)

#Question 1 b
summary(lm(videos$`File Size` ~ videos$`Song Length`))

# with ggplot
videos %>% 
  ggplot(aes(x = `Song Length`, y = `File Size`)) +
  geom_point(colour = "red") +
  geom_smooth(method = "lm", fill = NA)

# with Base R
plot(videos$`Song Length`, videos$`File Size`,
     main = "Scatter diagram for file size against song \n length with fitted regression line",
     pch = 20,col = "navyblue",
     xlab = 'Song length (seconds)',
     ylab = 'File size (MB)',
     xlim = c(0, 450),
     ylim = c(0, 70)) + abline(lm(videos$`File Size` ~ videos$`Song Length`), col = "navyblue")

#Question 2

# Subset the data set by excluding the Ohne Dich.
x_od <- subset(videos$`Song Length`, videos$`Song Title` != "Ohne Dich")
y_od <- subset(videos$`File Size`, videos$`Song Title` != "Ohne Dich")

# Calculate the sample correlation coefficient.
cor(x_od, y_od)

# Fit an OLS regression line to the data.
lrm_od <- lm(y_od ~ x_od, data = videos)
summary(lrm_od) 

coef(lrm_od)

(beta1hat_od <- coef(lrm_od)[1])

(beta2hat_od <- coef(lrm_od)[2])

# Determine an estimate for sigma^2.
(sigmasq_od <- sigma(lrm_od) ^ 2)

# Calculate the variances of the parameter estimates
# and the covariance between the parameter estimates.
vcov(lrm_od)
(var_beta1hat_od <- vcov(lrm_od)[1,1])
(var_beta2hat_od <- vcov(lrm_od)[2,2])

(cov_beta1hat_beta2hat_od <- vcov(lrm_od)[1,2])

# Calculate R-squared. 
(R2_od <- summary(lrm_od)$r.squared) 

# Draw a scatter diagram with the fitted regression line added.
plot(x_od, y_od,
     main = "Scatter diagram for file size against song \n length with fitted regression line",
     sub = "(Ohne Dich by Rammstein excluded)",
     pch = 20,
     col = "royalblue",
     xlab = 'Song length (seconds)',
     ylab = 'File size (MB)',
     xlim = c(0, 450),
     ylim = c(0, 40)) + abline(lrm_od, col = "royalblue") 
#Question 4
(n <- nrow(videos)) # (a) #
(sum_x <- sum(x)) # (b) #
(sum_y <- sum(y)) # (c) #
(sum_x2 <- sum(x ^ 2)) # (d) #
(sum_xy <- sum(x * y)) # (e) #
(xbar <- sum_x / n) # (f) #
(ybar <- sum_y / n) # (g) #
(betahat2 <- (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x ^ 2)) # (h) #
(betahat1 <- ybar - betahat2 * xbar) # (i) #
(sum_xd2 <- sum((x - xbar) ^ 2)) # (j) #
(tss <- sum((y - ybar) ^ 2)) # (k) #
(ess <- betahat2 ^ 2 * sum_xd2) # (l) #
(rss <- sum((y - betahat1 - betahat2 * x) ^ 2)) # (m) #
(r2 <- ess / tss) # (n) #
(mse <- rss / (n - 2)) # (o) #
(se_beta1 <- sqrt((mse * sum_x2) / (n * sum_xd2))) # (p) #
(se_beta2 <- sqrt(mse / sum_xd2)) # (q) #
(covbetas <- -xbar * se_beta2 ^ 2) # (r) # 



