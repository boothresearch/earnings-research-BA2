earnings <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv")

# File to write output of lm
write2file <- function(fname, regformula, regdata){
    fitted.model <- lm(formula = regformula, data= regdata)
    sink(file = fname, append = FALSE)
    print(Sys.time(), quote = FALSE)
    print(summary(fitted.model))
    closeAllConnections()
    fitted.model
}

# [1] Regress earnings on height
reg01 <- function() {
    fitted.model <- write2file("reg01.txt", earn ~ height, earnings)

    png("reg01.png")
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab= "earnings")
    abline(fitted.model, col= "red")
    dev.off()
}

# [2] Regress earnings on height controlling for male
reg02 <- function() {
    fitted.model <- write2file("reg02.txt", earn ~ height + male, earnings)

    png("reg02.png")
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab= "earnings")
    abline(fitted.model, col= "red")
    dev.off()
}

# [3] Regress earnings on height controlling for male
reg03 <- function() {
    fitted.model <- write2file("reg03.txt", earn ~ height + male + height:male, earnings)

    png("reg03.png")
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab= "earnings")
    abline(fitted.model, col= "red")
    dev.off()
}

reg01()
reg02()
reg03()