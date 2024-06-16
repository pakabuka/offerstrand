#read data
source = "/Users/frederickliu/Documents/Law School Applications/lsdata.csv"
data = read.csv(source)
attach(data)

#Logit Model: Acceptance ~ LSAT + UGPA:

chanceme_cycle = function(schoolname, mylsat, mygpa, whichcycle){
  newdata = subset(data, school_name == schoolname & cycle_id == whichcycle)
  sta <- c("Accepted", "WL, Accepted", "Accepted, Withdrawn", "Rejected", "WL, Accepted, Withdrawn", "WL, Rejected")
  newdata = subset(newdata, simple_status %in% unique(sta))
  binary_status = c()
  count = 1
  for (i in newdata$simple_status){
    if (i == "Accepted"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted"){
      binary_status[count] = 1
    } else if (i == "Accepted, Withdrawn"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted, Withdrawn"){
      binary_status[count] = 1
    } else {
      binary_status[count] = 0
    }
    count = count + 1
  }
  newlsat = newdata$lsat
  newgpa = newdata$gpa
  model = glm(binary_status ~ newlsat + newgpa, family = binomial(link = "logit"))
  stargazer::stargazer(model, type = "latex")
  prob = predict(model, newdata = data.frame(newlsat = mylsat, newgpa = mygpa), type = "response",se.fit=T)
  print(paste0("Model sample size N = ", length(binary_status)))
  print(paste0("Your probability of acceptance at ", schoolname, " is: ", 100*prob$fit, "%"))
  return(prob$fit)
}

chanceme = function(schoolname, mylsat, mygpa, urmstatus, internationalornot){
  newdata = subset(data, school_name == schoolname & urm == urmstatus & is_international == internationalornot)
  sta <- c("Accepted", "WL, Accepted", "Accepted, Withdrawn", "Rejected", "WL, Accepted, Withdrawn", "WL, Rejected")
  newdata = subset(newdata, simple_status %in% unique(sta))
  binary_status = c()
  count = 1
  for (i in newdata$simple_status){
    if (i == "Accepted"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted"){
      binary_status[count] = 1
    } else if (i == "Accepted, Withdrawn"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted, Withdrawn"){
      binary_status[count] = 1
    } else {
      binary_status[count] = 0
    }
    count = count + 1
  }
  newlsat = newdata$lsat
  newgpa = newdata$gpa
  model = glm(binary_status ~ newlsat + newgpa, family = binomial(link = "logit"))
  prob = predict(model, newdata = data.frame(newlsat = mylsat, newgpa = mygpa), type = "response",se.fit=T)
  print(paste0("Model sample size N = ", length(binary_status)))
  print(paste0("Your probability of acceptance at ", schoolname, " is: ", 100*prob$fit, "%"))
  return(prob$fit)
}


chancemeDETAILED = function(schoolname, mylsat, mygpa, mysofts, urmstatus, internationalornot){
  newdata = subset(data, school_name == schoolname & urm == urmstatus & 
                     softs == mysofts & is_international == internationalornot)
  sta <- c("Accepted", "WL, Accepted", "Accepted, Withdrawn", "Rejected", "WL, Accepted, Withdrawn", "WL, Rejected")
  newdata = subset(newdata, simple_status %in% unique(sta))
  binary_status = c()
  count = 1
  for (i in newdata$simple_status){
    if (i == "Accepted"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted"){
      binary_status[count] = 1
    } else if (i == "Accepted, Withdrawn"){
      binary_status[count] = 1
    } else if (i == "WL, Accepted, Withdrawn"){
      binary_status[count] = 1
    } else {
      binary_status[count] = 0
    }
    count = count + 1
  }
newlsat = newdata$lsat
newgpa = newdata$gpa
model = glm(binary_status ~ newlsat + newgpa, family = binomial(link = "logit"))
prob = predict(model, newdata = data.frame(newlsat = mylsat, newgpa = mygpa), type = "response",se.fit=T)
print(paste0("Model sample size N = ", length(binary_status)))
print(paste0("Your probability of acceptance at ", schoolname, " is: ", 100*prob$fit, "%"))
return(prob$fit)
}

chancemeDETAILED("Yale University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Harvard University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Stanford University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Columbia University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("University of Chicago", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("University of Pennsylvania", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("New York University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("University of California—Berkeley", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Duke University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Northwestern University", 173, 3.99, "T3", "FALSE", "FALSE")
chancemeDETAILED("Cornell University", 173, 3.99, "T3", "FALSE", "FALSE")

chanceme_cycle("Stanford University", 173, 3.99, 22)
chanceme_cycle("University of Chicago", 173, 3.99, 22)
chanceme_cycle("Columbia University", 173, 3.99, 22)
chanceme_cycle("Harvard University", 173, 3.99, 22)

chanceme("Yale University", 173, 3.99, "FALSE", "TRUE")
chanceme("Columbia University", 173, 3.99, "FALSE", "TRUE")
chanceme("University of Chicago", 173, 3.99, "FALSE", "TRUE")
chanceme("University of Pennsylvania", 173, 3.99, "FALSE", "TRUE")

drawschoolgraph = function(name, mylsat, mygpa){
  chicagoaccepted = user_id[which(simple_status == "Accepted" & school_name == name)]
  chicagorejected = user_id[which(simple_status == "Rejected" & school_name == name)]
  totallength = length(chicagorejected) + length(chicagoaccepted)
  
  median(lsat[which(simple_status == "Accepted" & school_name == name)], na.rm = T)
  plot(lsat[which(simple_status == "Rejected" & school_name == name)], 
       gpa[which(simple_status == "Rejected" & school_name == name)], xlim = c(150, 180), 
       xlab = "LSAT", ylab = "UGPA", main = paste(name, "2001-2023"), pch=20, col = "grey", sub = paste("#Observations:", totallength))
  
  points(lsat[which(simple_status == "Accepted" & school_name == name & urm == "TRUE")], 
         gpa[which(simple_status == "Accepted" & school_name == name & urm == "TRUE")], col = "#162D76", pch=15) 
  
  points(lsat[which(simple_status == "Accepted" & school_name == name & urm == "FALSE")], 
         gpa[which(simple_status == "Accepted" & school_name == name & urm == "FALSE")], col='green', pch=20) 

  lsatquantile = quantile(lsat[which(simple_status == "Accepted" & school_name == name)], c(.25, .50, .75), na.rm = T) 
  gpaquantile = quantile(gpa[which(simple_status == "Accepted" & school_name == name)], c(.25, .50, .75), na.rm = T) 
  abline(v = lsatquantile[1], col="orange", lwd=3, lty=2)
  text(x=lsatquantile[1]-0.5, y=2.3, srt=90, lsatquantile[1], col = "red")
  abline(v = lsatquantile[2], col="orange", lwd=3, lty=2)
  text(x=lsatquantile[2]-0.5, y=2.3, srt=90, lsatquantile[2], col = "red")
  abline(v = lsatquantile[3], col="orange", lwd=3, lty=2)
  text(x=lsatquantile[3]-0.5, y=2.3, srt=90, lsatquantile[3], col = "red")
  abline(h = gpaquantile[1], col="orange", lwd=3, lty=2)
  text(x=160, y=gpaquantile[1]+0.05, gpaquantile[1], col = "red")
  abline(h = gpaquantile[2], col="orange", lwd=3, lty=2)
  text(x=160, y=gpaquantile[2]+0.05, gpaquantile[2], col = "red")
  abline(h = gpaquantile[3], col="orange", lwd=3, lty=2)
  text(x=160, y=gpaquantile[3]+0.05, gpaquantile[3], col = "red")
  
  #points(mylsat, mygpa, pch = 17, col = "red")
  
  # legend("bottomleft", 
  #        legend = c("URM Accepted", "nonURM Accepted", "Rejected", "You"), 
  #        col = c("#162D76", 
  #                "green", "grey", "red"), 
  #        pch = c(15,20, 20, 17), 
  #     
  #        pt.cex = 0.8, 
  #        cex = 0.8, 
  #        text.col = "black", 
  #        horiz = F, 
  #        inset = c(0.1, 0.1, 0.1, 0.1), bg = "yellow")
  
  legend("bottomleft", 
         legend = c("URM Accepted", "nonURM Accepted", "Rejected"), 
         col = c("#162D76", 
                 "green", "grey"), 
         pch = c(15,20, 20), 
         
         pt.cex = 0.8, 
         cex = 0.8, 
         text.col = "black", 
         horiz = F, 
         inset = c(0.1, 0.1, 0.1), bg = "yellow")
}

drawschoolgraph("University of North Carolina", 1, 1)
drawschoolgraph("University of Chicago", 173, 3.99)
drawschoolgraph("Stanford University", 173, 3.99)
drawschoolgraph("Harvard University", 173, 3.99)
drawschoolgraph("Yale University", 173, 3.99)
drawschoolgraph("Columbia University", 173, 3.99)
drawschoolgraph("Northwestern University", 173, 3.99)
drawschoolgraph("Cornell University", 173, 3.99)
drawschoolgraph("University of California—Berkeley", 173, 3.99)
drawschoolgraph("Duke University", 173, 3.99)
drawschoolgraph("University of Pennsylvania", 173, 3.99)
drawschoolgraph("New York University", 173, 3.99)

drawschoolgraph("University of California—Berkeley", 169, 3.84)
drawschoolgraph("Duke University", 173, 3.99)
drawschoolgraph("Columbia University", 169, 3.84)


