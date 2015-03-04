##################################################################
# This is a simulation of power function of the Friedman Test and 
# the modified version of Kruskal-Wallis Test.
# The result of this simulation can be erroneous but, the method
# used for simulation might be used as a reference
#
# Note: need to run "Simulator.R" before executing the simulation
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################


#### Demonstration

windows(10,12); require("animation")
difference = seq(1, 3.5, length = 10); power = seq(0, 1, length = 10)
plot(difference, power, type = "n", main = "Friedman test vs Kruskal test")
legend(1, 1, c("Kruskal", "Friedman"), lty = 1, col = c("red", "blue"), lwd = 3)

#### Mark points and lines to the graph

## Point 1
p1 = pointgenerator(0)/500
dev.set(2); points(1, p1[1], pch = 16, col = "blue"); points(1, p1[2], pch = 16, col = "red")
## Point 2
p2 = pointgenerator(0.5)/500
dev.set(2); points(1.5, p2[1], pch = 16, col = "blue"); points(1.5, p2[2], pch = 16, col = "red")
lines(c(1, 1.5), c(p1[1], p2[1]), type = "l", col = "blue", lwd = 3); lines(c(1, 1.5), c(p1[2], p2[2]), col = "red", lwd = 3)
## Point 3
p3 = pointgenerator(1)/500
dev.set(2); points(2, p3[1], pch = 16, col = "blue"); points(2, p3[2], pch = 16, col = "red")
lines(c(1.5, 2), c(p2[1], p3[1]), type = "l", col = "blue", lwd = 3); lines(c(1.5, 2),c(p2[2], p3[2]),col = "red", lwd = 3)
## Point 4
p4 = pointgenerator(1.5)/500
dev.set(2); points(2.5, p4[1], pch = 16, col = "blue"); points(2.5, p4[2], pch = 16, col = "red")
lines(c(2, 2.5),c(p3[1], p4[1]), type = "l",col = "blue",lwd = 3); lines(c(2, 2.5), c(p3[2], p4[2]), col = "red", lwd = 3)
## Point 5
p5 = pointgenerator(2)/500
dev.set(2); points(3, p5[1], pch = 16, col = "blue"); points(3, p5[2], pch = 16, col = "red")
lines(c(2.5, 3), c(p4[1], p5[1]), type = "l", col = "blue", lwd = 3); lines(c(2.5, 3), c(p4[2], p5[2]), col = "red", lwd = 3)
## Point 6
p6 = pointgenerator(2.5)/500
dev.set(2); points(3.5, p6[1], pch = 16, col =" blue"); points(3.5, p6[2], pch = 16, col = "red")
lines(c(3, 3.5), c(p5[1], p6[1]), type = "l", col = "blue", lwd = 3); lines(c(3, 3.5), c(p5[2], p6[2]), col = "red", lwd = 3)
