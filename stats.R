anonymous_height <- c(72, 64, 69, 72, 69, 68, 73, 66.5, 68, 81, 74, 63.5, 70, 74, 73, 66, 72, 70, 62, 63, 70, 70, 69, 74, 69, 67, 65, 73, 73.5, 68, 71, 72, 70, 71, 76)
anonymous_weight <- c(150, 145, 120, 132, 230, 169, 135, 236, 205, 150, 140, 175, 150, 164, 190, 140, 160, 185, 190, 176, 129, 271, 136, 115, 160, 161, 200, 140, 180, 155, 105, 106, 160, 150, 115)
public_height <- c(67, 72, 72, 72, 74, 70, 73, 70, 64, 71, 67, 72, 69, 75, 73, 69, 71, 66, 72, 71, 66, 66, 69.5, 68, 64, 65, 66, 68.5, 68, 69, 64, 70, 69, 68, 74, 70, 70, 70, 66, 71)
public_weight <- c(130, 125, 132, 168, 125, 128, 125, 151, 190, 155, 130, 185, 190, 150, 260, 226, 184, 155, 145, 190, 140, 171, 150, 140, 175, 190, 210, 152, 120, 145, 110, 210, 150, 290, 240, 160, 130, 150, 170, 185)

findMin <- function(arg){
  min = 100
  for(i in 1:length(arg)){
    if(arg[i] < min){
      min = arg[i]
    }
  }
  return(min)
} 
findMax <- function(arg){
  max = 0
  for(i in 1:length(arg)){
    if(arg[i] > max){
      max = arg[i]
    }
  }
  return(max)
}

hist(anonymous_height, freq=F, breaks=10, xlim=range(findMin(anonymous_height - 5), findMax(anonymous_height) + 5), xlab="Anonymous Height (in.)")
lines(density(anonymous_height), col="blue", lwd=1)

hist(public_height, freq=F, breaks=10, xlim=range(findMin(public_height) - 5, findMax(public_height) + 5), xlab="Public Height (in.)")
lines(density(public_height), col="blue", lwd=1)

hist(anonymous_weight, freq=F, breaks=20, xlim=range(findMin(anonymous_weight - 50), findMax(anonymous_weight) + 50), xlab="Anonymous Weight (lbs.)")
lines(density(anonymous_weight), col="blue", lwd=1)

hist(public_weight, freq=F, breaks=20, xlim=range(findMin(public_weight) - 50, findMax(public_weight) + 50), xlab="Public Weight (lbs.)")
lines(density(public_weight), col="blue", lwd=1)

print(median(anonymous_height))
print(median(public_height))
print(median(anonymous_weight))
print(median(public_weight))

print(mean(anonymous_height))
print(mean(public_height))
print(mean(anonymous_weight))
print(mean(public_weight))

print(sd(anonymous_height))
print(sd(public_height))
print(sd(anonymous_weight))
print(sd(public_weight))

print(length(anonymous_height) + length(anonymous_weight) + length(public_height) + length(public_weight))


# Here begins the physics lab calculations
ln_weight_array <- c(-4.9, -4.21, -3.8, -3.51, -3.29)
ln_velocity_array <- c(-.243, 0.0488, .255, .372, .482)
ln_b_array <- c()

graph <- plot(ln_weight_array, ln_velocity_array)
graph1 <- plot(ln_velocity_array, ln_weight_array, xlab="ln(v) (m/s)", ylab="ln(Fw) (N)")
fit <- lm(ln_velocity_array ~ ln_weight_array)
fit2 <- lm(ln_weight_array ~ ln_velocity_array)
abline(fit2)
summary(fit2)
summary(fit)


#Physics lab
length <- 0.231 #m

hangingMass = c(.050, .050, .050, .050, .100, .120, .070)

#Remember that this is the time it takes to make 20 revolutions
time_vector <- c(8.95, 8.88, 8.77, 8.54, 6.17, 5.78, 6.76)
circumference_vector <- c()
theta_in_degrees <- c(80, 79, 77.5, 80, 84, 86, 82)
theta_vector <- theta_in_degrees * (pi/180)
radius_vector <- length * sin(theta_vector)

trials = length(time_vector)
for(i in 1:trials){
  circumference_vector[i] = 2 * pi * radius_vector[i]
}

velocity_vector <- 20 * circumference_vector / (time_vector)

stopper_vector <- (hangingMass*9.8*radius_vector*sin(theta_vector))/((velocity_vector)^2)

coefficient_vector <- c(0, 0, 0, 0, 0, 0, 0)

hanging_mass_force <- hangingMass * 9.8


coefficient_vector = (velocity_vector^2)/(radius_vector * sin(theta_vector))

plot(coefficient_vector, hanging_mass_force, xlab="Coefficient (v^2)/(rsin(theta))", ylab="Hanging Mass Force (N)")
fit <- lm(hanging_mass_force ~ coefficient_vector)

table(hangingMass, circumference_vector, time_vector, theta_in_degrees, radius_vector, velocity_vector, stopper_vector)
colnames(c("Time (s)", "Degrees", "Hanging Mass (kg)", "Radius (m)", "Circumference (m)", "Velocity (m/s)"), do.NULL = FALSE)
table <- matrix(c(time_vector, theta_in_degrees, hangingMass, radius_vector, circumference_vector, velocity_vector), ncol=6, nrow=7, byrow=FALSE)
table <- as.table(table, row.names = c("Time (s)", "Degrees", "Hanging Mass (kg)", "Radius (m)", "Circumference (m)", "Velocity (m/s)"))

##############################

Delta_X <- c(0, 0.028, 0.06, 0.092, 0.12, 0.151)
Weight <- c(0, 0.098, 0.196, 0.294, 0.392, 0.49)

plot(Delta_X, Weight, xlab="Delta X (m)", ylab="Weight (N)", main="F(x) = 3.20x", xlim=c(0, 0.16), ylim=c(0, 0.5))
fit <- lm(weight ~ delX)
fit


abline(fit)
#polygon(Delta_X, y=3.200837*Delta_X, col="black", density=300)

##################################################
# Physics bouncy ball lab #
##################################################
initialHeights <- c(.3, .6, .9, 1.2, 1.5)
bounceHeightsTennis <- c(.20375, .4075, .6075, .72125, .97)
bounceHeightsWiffle <- c(.16675, .33375, .44, .55, .6725)

idealX <- c(1, 2)
idealY <- c(1, 2)

fitTennis <- lm(bounceHeightsTennis ~ initialHeights)
fitWiffle <- lm(bounceHeightsWiffle ~ initialHeights)

plot(initialHeights, bounceHeightsTennis, xlim=c(0,2), ylim=c(0,1), main="Tennis Ball (y = 0.651x + 0.0281)", xlab="Initial Height (m)", ylab="Final Height (m)")
abline(fitTennis)
abline(lm(idealY ~ idealX))

plot(initialHeights, bounceHeightsWiffle, xlim=c(0,2), ylim=c(0,1), main="Wiffle Ball (y = 0.409x + 0.0643)", xlab="Initial Height (m)", ylab="Final Height (m)")
abline(fitWiffle)
abline(lm(idealY ~ idealX))


