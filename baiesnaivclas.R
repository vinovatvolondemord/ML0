play <- matrix(c("sun","yes",
 "sun", "yes",
 "sun", "no",
 "sun", "yes",
 "sun", "no",
 "rain", "yes",
 "rain",	"yes",
 "rain", "no",
 "rain", "no",
 "rain", "no",
 "cloud",	"yes",
 "cloud",	"yes",
 "cloud",	"yes",
 "cloud",	"yes"),ncol=2,byrow=TRUE)
 colnames(play) <- c("weather","play")
 play <- as.table(play)
play
my2condition<- "cloud"
my2condition
my1condition<- "yes"
my1condition

con12cnt1<- 0
con12cnt2<- 0
con1cnt1<- 0
con1cnt2<- 14
for(s in 1:14){
	if(my2condition == play[s,1]){
		con12cnt1<- con12cnt1+1	
		if(my1condition == play[s,2]){
			con12cnt2<- con12cnt2+1
		}
	}
	else if(my2condition == play[s,2]){
		con12cnt1<- con12cnt1+1	
		if(my1condition == play[s,1]){
			con12cnt2<- con12cnt2+1
		}
	}
	if(my1condition == play[s,1]){
		con1cnt1<- con1cnt1+1
	}
	else if(my1condition == play[s,2]){
		con1cnt1<- con1cnt1+1
	}
}
con2 <- con12cnt1/14
con2
con1cnt1
con1 <- con1cnt1/con1cnt2
con1
con12<- con12cnt2/con12cnt1
con12
baies<- con12*con2/con1
baies
