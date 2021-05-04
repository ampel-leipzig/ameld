eldr <- read.csv("laboratory-value-reference.csv")
sex <- c(male = 0, female = 1, both = 2)
eldr$Sex <- factor(
    sex[eldr$Sex], levels = sex, labels = c("male", "female", "both")
)
save(eldr, file = file.path("..", "data", "eldr.rda"), compress = "xz")
