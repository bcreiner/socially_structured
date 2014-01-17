#####################
#####################
##### Functions #####
#####################
#####################

Neighbors<-function(loc){
	locx=floor((loc-1)/CityLength)+1
	locy=(loc-1)%%CityLength+1
	Nei=c(locx,locy)
	for (i in -1:1){
		for (j in -1:1){
			Nei=rbind(Nei,c(locx+i,locy+j))
		}
	}
	Nei=Nei[-1,]
	for (i in length(Nei[,1]):1){
		if (Nei[i,1]<1) Nei=Nei[-i,]
		else if (Nei[i,2]<1) Nei=Nei[-i,]
		else if (Nei[i,1]>CityWidth) Nei=Nei[-i,]
		else if (Nei[i,2]>CityLength) Nei=Nei[-i,]
	}
	NumNei=Nei[,2]+CityLength*(Nei[,1]-1)
	return(NumNei)
}


Neighbors2<-function(loc){
	locx=floor((loc-1)/CityLength)+1
	locy=(loc-1)%%CityLength+1
	Nei=c(locx,locy)
	for (i in -2:2){
		for (j in -2:2){
			Nei=rbind(Nei,c(locx+i,locy+j))
		}
	}
	Nei=Nei[-1,]
	for (i in length(Nei[,1]):1){
		if (Nei[i,1]<1) Nei=Nei[-i,]
		else if (Nei[i,2]<1) Nei=Nei[-i,]
		else if (Nei[i,1]>CityWidth) Nei=Nei[-i,]
		else if (Nei[i,2]>CityLength) Nei=Nei[-i,]
	}
	NumNei=Nei[,2]+CityLength*(Nei[,1]-1)
	return(NumNei)
}

HousesleNAway<-function(loc,distance){
	if (distance==0) return(loc)
	else {
	locx=floor((loc-1)/CityLength)+1
	locy=(loc-1)%%CityLength+1
	Nei=c(locx,locy)
	for (i in -distance:distance){
		for (j in -distance:distance){
			Nei=rbind(Nei,c(locx+i,locy+j))
		}
	}
	Nei=Nei[-1,]
	for (i in length(Nei[,1]):1){
		if (Nei[i,1]<1) Nei=Nei[-i,]
		else if (Nei[i,2]<1) Nei=Nei[-i,]
		else if (Nei[i,1]>CityWidth) Nei=Nei[-i,]
		else if (Nei[i,2]>CityLength) Nei=Nei[-i,]
	}
	NumNei=Nei[,2]+CityLength*(Nei[,1]-1)
	return(NumNei)
	}
}
HousesOnlyNAway<-function(loc,distance){
	if (distance==0) return(loc)
	else {
		return(setdiff(HousesleNAway(loc,distance),HousesleNAway(loc,distance-1)))
	}
}

#####################
#####################
##### Main Code #####
#####################
#####################

Samples=rep(-2,19*3)
### IN governs the number of houses an individual moves to from their social group (including their home)
### In general, kappa = IN - 1
### OUT governs the number of random houses they go to
IN <- 5
OUT <- 1

### ForrayList allows for values of kappa between 4 and 5
ForrayList <- 10 * (1:10)
### Forray will indicate what percent of the population goes to one location outside their social group when IN=1
### kappa = IN - 1  + (1 - Forray)
###
### Note: This order and location of for loops will result in every simulation being run on a different configuration of people and social groups
### Move it to the simulation section to run simulations on the same grid
###
for (Forray in ForrayList){
### Spreads governs what percent of the risk from infectious mosquitoes leaves the home
Spreads=(0:8)/9
for (SPREAD in 1:9){
### TI tracks infections per day
### Take care reading the TI code. Depdending on the order of the above for loops, the rows that should be selected for a given scenario may be different than expected
TI={}
for (RUN in 1:300){


################## For space purposes, I'm going to use Geometric distributionns instead of Exponential, so events only happen on the hour if they are going to happen.....
#### 12 hour days
#### Keep track of Mosquitoes at each stage

### Constants
### Sets simulation grid at 20x20 homes
CityWidth=20
CityLength=20

AvgPerHouse=6
PercentHomeMin=.4
PercentHomeMax=.75
MinInSocial=IN # Home counts as an element of the Social
MaxInSocial=IN
MinOutOfSocial=0
MaxOutOfSocial=OUT
### Sets the percent of people who leave their social group
DistOutOfSocial=c(1-(0.01*Forray),0.01*Forray)
SmallestSocial=max(MinInSocial,1)
LargestSocial=IN
if (MaxInSocial==1)LargestSocial=1

####### Variables defined below
# Population - Total population size, equals number of houses * avg people per house (integer)
# Homex - home location x coordinate (vector)
# Homey - home location y coordinate (vector)
# NumInSocial - number of core Social sites (excluding home) each person visits (vector)
# NumNotInSocial - number of non-core Social sites each person visits (vector)
# PercentInSocial - percent of time each person spends within their core Social (vector)
# PercentHome - percent of time each person spends at home (vector)
# PercentMove - percent of time spent at each location they can go to (matrix) First column is Percent Home
# Socials - which homes are part of which Social groups (matrix)
# Locations - all places a person can go. First element is their home, the next 2:MaxInSocial are Social sites (but probably not all of them...), the remaining are 'other homes' (array)
# SocialNumber - which Social each person is a member of (vector)
# SocialHouses - which 'core Social' homes a person can (but wont if the percent move to that location =0) go to
# OtherHouses - which other locations a person can (but wont if the percent move to that location =0) go to (matrix)
# SocialLocations - where the sites within each Social are located  (array)


####### Description
# This section defines what the Socials are, where people go and what percent of their time people spend at those locations


### Home house
### Commented code is used when there are exactly the same number of people in every house
# Total Population Size
# Population=AvgPerHouse*CityWidth*CityLength
# MinHouse=2
# CurrentMin=0
# while (CurrentMin<MinHouse){
#	Locations of each persons 'home'
##	Homex=sample(1:CityWidth,replace=T,Population)
##	Homey=sample(1:CityLength,replace=T,Population)
	# Homex=rep(1:CityWidth,AvgPerHouse*CityLength)
	# Homey=sort(rep(1:CityLength,AvgPerHouse*CityWidth))
##	Number of people per 'home'
	# Homemat=matrix(0,CityLength,CityWidth)
	# for (i in 1:length(Homex)){
		# Homemat[Homey[i],Homex[i]]=Homemat[Homey[i],Homex[i]]+1
	# }
	# CurrentMin=min(Homemat)
# }
### Below code is used when the number of people per house should match observed patterns in Iquitos (Scenario 2)
Homemat=matrix(0,CityLength,CityWidth)
Homey<-{}
Homex<-{}
for (i in 1:CityWidth){
	for (j in 1:CityLength){
		Homemat[j,i]=rnbinom(1,mu=6.16,size=9.01)
		Homey<-c(Homey,rep(j,Homemat[j,i]))
		Homex<-c(Homex,rep(i,Homemat[j,i]))
	}
}
Population=sum(Homemat)



## Specifying which houses are in which 'social group'
# Random order of homes that will be slowly cut into pieces
SocialOrder=sample(1:(CityWidth*CityLength))
# Setting up Social matrix
Socials=rep(0,LargestSocial)
# Length of first Social group
FirstLength=max(sample(SmallestSocial:LargestSocial,1),SmallestSocial)
# Defining first Social group
Socials[1:FirstLength]=SocialOrder[1:FirstLength]
# Removing first Social group homes from list of homes that still need to be assigned to a Social group
SocialOrder=SocialOrder[-(1:FirstLength)]
while (length(SocialOrder)>LargestSocial){
	Length=max(sample(SmallestSocial:LargestSocial,1),SmallestSocial)
	nextSocial=SocialOrder[1:Length]
	if (Length<LargestSocial) nextSocial=c(nextSocial,rep(0,LargestSocial-Length))
	Socials=rbind(Socials,nextSocial)
	SocialOrder=SocialOrder[-(1:Length)]
}
if (length(SocialOrder)<LargestSocial) SocialOrder=c(SocialOrder,rep(0,LargestSocial-length(SocialOrder)))
Socials=rbind(Socials,SocialOrder)

if (LargestSocial>SmallestSocial){
lastlength=LargestSocial-length(which(Socials[length(Socials[,1]),]==0))
while (lastlength<SmallestSocial){
	SocialOrder=sample(1:(CityWidth*CityLength))
	Socials=rep(0,LargestSocial)
	FirstLength=max(sample(SmallestSocial:LargestSocial,1),SmallestSocial)
	Socials[1:FirstLength]=SocialOrder[1:FirstLength]
	SocialOrder=SocialOrder[-(1:FirstLength)]
	while (length(SocialOrder)>LargestSocial){
		Length=max(sample(SmallestSocial:LargestSocial,1),SmallestSocial)
		nextSocial=SocialOrder[1:Length]
		if (Length<LargestSocial) nextSocial=c(nextSocial,rep(0,LargestSocial-Length))
		Socials=rbind(Socials,nextSocial)
		SocialOrder=SocialOrder[-(1:Length)]
	}
	if (length(SocialOrder)<LargestSocial) SocialOrder=c(SocialOrder,rep(0,LargestSocial-length(SocialOrder)))
	Socials=rbind(Socials,SocialOrder)
	lastlength=LargestSocial-length(which(Socials[length(Socials[,1]),]==0))
}
}

Locations=array(0,dim=c(Population,MaxInSocial+MaxOutOfSocial,2))
Locations[,1,1]=Homex
Locations[,1,2]=Homey
SocialNumber=rep(0,Population)
# Number of houses each person vistis within their 'social group' (must be less than the total number of homes in their social group) Currently uniform distribution
NumInSocial=SocialNumber
# Number of houses each person vistis ouside their 'social group'. Currently uniform distribution
if (MinOutOfSocial==MaxOutOfSocial){
	NumNotInSocial=rep(MinOutOfSocial,Population)
}else {
	NumNotInSocial=sample(MinOutOfSocial:MaxOutOfSocial,replace=T,prob=DistOutOfSocial,Population)
}
SocialHouses=matrix(0,Population,LargestSocial)
SocialHouses[,1]=Homey+CityLength*(Homex-1)
OtherHouses=matrix(0,Population,MaxOutOfSocial)
for (i in 1:Population){
	home=Homey[i]+CityLength*(Homex[i]-1)
	SocialNumber[i]=which(Socials==home,arr.ind=T)[1]
	good=which(Socials[SocialNumber[i],]!=0)[-which(Socials==home,arr.ind=T)[2]]
	NumInSocial[i]=max(sample(MinInSocial:min(MaxInSocial,length(good)+1),replace=T,1),MinInSocial)
	Order=sample(1:length(good))
	if (MaxInSocial>1 & length(good)>0){
		if (length(Order)>MaxInSocial) Order=Order[1:(MaxInSocial-1)]#### Makes it so people won't travel to every home in their social group (whenever MaxClsuterSize>MaxPersonalSocialSize)
		Locations[i,2:(length(Order)+1),1]=(floor((Socials[SocialNumber[i],good]-1)/CityLength)+1)[Order]
		Locations[i,2:(length(Order)+1),2]=((Socials[SocialNumber[i],good]-1)%%CityLength+1)[Order]
		SocialHouses[i,2:(length(good)+1)]=Socials[SocialNumber[i],good]
		for (k in 2:MaxInSocial){
			if (Locations[i,k,1]==0) Locations[i,k,2]=0
		}
	}
	if (NumNotInSocial[i]>0){
			OtherHouse=sample((1:(CityWidth*CityLength))[-Socials[SocialNumber[i],]],NumNotInSocial[i])
			Locations[i,(1+MaxInSocial):(MaxInSocial+NumNotInSocial[i]),1]=floor((OtherHouse-1)/CityLength)+1
			Locations[i,(1+MaxInSocial):(MaxInSocial+NumNotInSocial[i]),2]=(OtherHouse-1)%%CityLength+1
			if (NumNotInSocial[i]<MaxOutOfSocial) OtherHouse=c(OtherHouse,rep(0,MaxOutOfSocial-NumNotInSocial[i]))
			OtherHouses[i,] = OtherHouse
	}
}
		
SocialLocations=array(0,dim=c(length(Socials[,1]),LargestSocial,2))
for (i in 1:length(Socials[,1])){
	SocialLocations[i,,1]=floor((Socials[i,]-1)/CityLength)+1
	SocialLocations[i,,2]=(Socials[i,]-1)%%CityLength+1
	for (k in 1:LargestSocial){
		if (SocialLocations[i,k,1]==0) SocialLocations[i,k,2]=0
	}
}


# Percent of time spent at 'home'
PercentHome=runif(Population,min=PercentHomeMin,max=PercentHomeMax)
######## making 15% of people not move (Scenario 3)
HomeBodies<-sample(1:Population,round(.15*Population))
PercentHome[HomeBodies]=1
# Matrix of percent time spent at each locations (within and without social group)
PercentMove=matrix(0,Population,MaxInSocial+MaxOutOfSocial)
# Setting 'home' percent time
PercentMove[,1]=PercentHome
for (i in 1:Population){
	Split=runif(MaxInSocial+NumNotInSocial[i]-1)
	PercentMove[i,2:(MaxInSocial+NumNotInSocial[i])]=(1-PercentHome[i])*Split/sum(Split)
}


ShortestAvgTime=2
LongestAvgTime=5
TotalDays=360
DayLength=12
TotalTime=TotalDays*DayLength

MoveLambda=runif(Population,min=ShortestAvgTime,max=LongestAvgTime)
MoveRate=1/MoveLambda



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
#############  Movement



####### Variables defined below
# MoveLambda - geometric distribution parameter governing time between moving between sites (in terms of hours) (vector)
# Moverate - geometric distribution rate (1/lambda) (vector)
# Initial
# Paths
# MoveTimes





#Cumulative sums of percents for determing where a person moves
TransitionTester=t(apply(PercentMove,1,cumsum))
#Initial location of each person
CurrentLoc=matrix(0,Population,2)
NumLocations=matrix(0,Population,MaxInSocial+MaxOutOfSocial)
for (i in 1:Population){
	Start=sample(1:(MaxInSocial+MaxOutOfSocial),1,prob=PercentMove[i,])
	CurrentLoc[i,]=Locations[i,Start,]
	ActuallyGoTo=which(PercentMove[i,]>0)
	NumLocations[i,ActuallyGoTo]=Locations[i,ActuallyGoTo,2]+CityLength*(Locations[i,ActuallyGoTo,1]-1)
}
NumCurrentLoc=CurrentLoc[,2]+CityLength*(CurrentLoc[,1]-1)


####################
#### Simulation ####
####################






## Disease Dynamics ##



Spread=Spreads[SPREAD]
tempSample={}
######## Within host dynamics
HostIncubationMin=4*DayLength
HostIncubationMax=7*DayLength
HostInfectiveMin=4*DayLength
HostInfectiveMax=5*DayLength
HtoVTransmissionProbMin=.5
HtoVTransmissionProbMax=.75
InfectionInfo=matrix(0,Population,5)
######## Vector dynamics
VectorIncubationMin=8*DayLength
VectorIncubationMax=10*DayLength
### Bites per person per hour
BitesPerHourMin=0
BitesPerHourMax=5


VectorLambdaMin=rep(10*DayLength,DayLength*TotalDays)
VectorLambdaMax=rep(18*DayLength,DayLength*TotalDays)
### Risk a single infective mosquito adds to house per hour
RiskToHouseMin=5e-5
RiskToHouseMax=5e-4
NodeStatus=matrix(0,CityWidth*CityLength,TotalTime)


N=1
#Toss in N infected person(s)
Infected=which(InfectionInfo[,4]==1)
Infective=which(InfectionInfo[,4]==2)
SpreadOut=0
while (SpreadOut<N){
	InfectionInfo=matrix(0,Population,5)
	for (Seed in 1:N){
		Resivoir=runif(1)
		if (Resivoir<1){
			Unlucky=sample(which(InfectionInfo[,4]==0),1)
			InfectionInfo[Unlucky,1]=1
			InfectionInfo[Unlucky,2]=InfectionInfo[Unlucky,1]+sample(HostIncubationMin:HostIncubationMax,1)
			InfectionInfo[Unlucky,3]=InfectionInfo[Unlucky,2]+sample(HostInfectiveMin:HostInfectiveMin,1)
			InfectionInfo[Unlucky,4]=1
			InfectionInfo[Unlucky,5]=NumCurrentLoc[Unlucky]+1e5
		}
	}
	SpreadOut=length(unique(SocialNumber[which(InfectionInfo[,1]==1)]))
}
PercentInfectedNow=0
PercentInfectedBefore=0
PercentInfected=0
PercentTreshold=1
newSample=rep(-1,19*3)	
for (Day in 1:TotalDays){
	### The Day
	for (Hour in 1:DayLength){
		t=DayLength*(Day-1)+Hour
		PercentInfectedBefore=PercentInfectedNow
		PercentInfectedNow=length(which(InfectionInfo[,4]>0))/Population
		PercentInfected=c(PercentInfected,PercentInfectedNow)
		### Conduct cluster experiment ?
		if (PercentInfectedBefore<(PercentTreshold/20) & PercentInfectedNow>=(PercentTreshold/20) & PercentTreshold<20){
			PercentTreshold=PercentTreshold + 1
			CLUSneg=0
			CLUSpos=0
			if (length(which(InfectionInfo[,4]!=2))>0 & length(which(InfectionInfo[,4]==2))>0){
				while (CLUSneg == CLUSpos){
					if (length(which(InfectionInfo[,4]!=2))==1) DENVneg=which(InfectionInfo[,4]!=2)
					else DENVneg=sample(which(InfectionInfo[,4]!=2),1)
					if (length(which(InfectionInfo[,4]==2))==1) DENVneg=which(InfectionInfo[,4]==2)
					else DENVpos=sample(which(InfectionInfo[,4]==2),1)
					CLUSneg=SocialNumber[DENVneg]
					CLUSpos=SocialNumber[DENVpos]
				}
				#### DENV positive cluster
				PeopleAtHome=which(NumLocations[,1]==NumLocations[DENVpos,1])
				NotSampledPerson=setdiff(PeopleAtHome,DENVpos)
				PeopleAtHomeWhoComply=sample(NotSampledPerson,round(.55*length(NotSampledPerson)),replace=FALSE)
				for (i in PeopleAtHomeWhoComply){
					vec=c(t,PercentTreshold-1,1,NumLocations[DENVpos,1],NumLocations[DENVpos,1],InfectionInfo[i,])
					tempSample=rbind(tempSample,vec)
				}
				ContactNetwork=setdiff(NumLocations[DENVpos,-1],0)
				if (length(ContactNetwork)>0){
					YesList=rep(0,length(ContactNetwork))
					for (k in 1:length(ContactNetwork)){
						PeopleWhoLiveThere=which(NumLocations[,1]==ContactNetwork[k])
						PeopleWhoComply=sample(PeopleWhoLiveThere,round(.55*length(PeopleWhoLiveThere)),replace=FALSE)
						for (i in PeopleWhoComply){
							vec=c(t,PercentTreshold-1,1,NumLocations[DENVpos,1],ContactNetwork[k],InfectionInfo[i,])
							tempSample=rbind(tempSample,vec)
						}
					}
				} else { YesList=c(-10,-10)}
				#### Use -10 as an indication something didn't work this round
				newSample[PercentTreshold-1]=sum(YesList)/length(YesList)
				#### DENV negative cluster
				PeopleAtHome=which(NumLocations[,1]==NumLocations[DENVneg,1])
				NotSampledPerson=setdiff(PeopleAtHome,DENVneg)
				PeopleAtHomeWhoComply=sample(NotSampledPerson,round(.55*length(NotSampledPerson)),replace=FALSE)
				for (i in PeopleAtHomeWhoComply){
					vec=c(t,PercentTreshold-1,0,NumLocations[DENVneg,1],NumLocations[DENVneg,1],InfectionInfo[i,])
					tempSample=rbind(tempSample,vec)
				}
				ContactNetwork=setdiff(NumLocations[DENVneg,-1],0)
				if (length(ContactNetwork)>0){
					YesList=rep(0,length(ContactNetwork))
					for (k in 1:length(ContactNetwork)){
						PeopleWhoLiveThere=which(NumLocations[,1]==ContactNetwork[k])
						PeopleWhoComply=sample(PeopleWhoLiveThere,round(.55*length(PeopleWhoLiveThere)),replace=FALSE)
						for (i in PeopleWhoComply){
							vec=c(t,PercentTreshold-1,0,NumLocations[DENVneg,1],ContactNetwork[k],InfectionInfo[i,])
							tempSample=rbind(tempSample,vec)
						}
					}
				} else { YesList=c(-10,-10)}
				#### Use -10 as an indication something didn't work this round
				newSample[19+PercentTreshold-1]=sum(YesList)/length(YesList)
			}
			newSample[19*2+PercentTreshold-1]=t
		}			
		TranChance=sum(NodeStatus[,t:length(NodeStatus[1,])])
		if (t<TotalTime&length(which(InfectionInfo[,4]<3))>0&(TranChance>0|length(which(InfectionInfo[,4]==1|InfectionInfo[,4]==2))>0)){	
			### Host E->I
			Infected=which(InfectionInfo[,4]==1)
			for (i in Infected){
				if (InfectionInfo[i,2]<=t)InfectionInfo[i,4]=2
			}
			### Host I->R
			Infective=which(InfectionInfo[,4]==2)
			for (i in Infective){
				if (InfectionInfo[i,3]<=t)InfectionInfo[i,4]=3
			}
			### Host S->E
			for (k in 1:(CityLength*CityWidth)){
				AtHouseAndSusceptible=which(NumCurrentLoc==k & InfectionInfo[,4]==0)
				if (length(AtHouseAndSusceptible)>0){
					for (i in AtHouseAndSusceptible){
						BadLuck=runif(1)
						if (BadLuck<NodeStatus[k,t]){
							InfectionInfo[i,1]=t
							InfectionInfo[i,2]=InfectionInfo[i,1]+sample(HostIncubationMin:HostIncubationMax,1)
							InfectionInfo[i,3]=InfectionInfo[i,2]+sample(HostInfectiveMin:HostInfectiveMax,1)
							InfectionInfo[i,4]=1
							InfectionInfo[i,5]=k
						}
					}
				}
			}
			### Vec Increase Risk
			if (length(Infective)>0){
				for (i in Infective){
					CurrentLocation=NumCurrentLoc[i]
					Bites=sample(BitesPerHourMin:BitesPerHourMax,1)
					InfectedVector=round(Bites*runif(1,min=HtoVTransmissionProbMin,max=HtoVTransmissionProbMax))
					if (InfectedVector>0){
						Incubation=rep(0,InfectedVector)
						Death=Incubation
						for (j in 1:InfectedVector){
							Incubation[j]=sample(VectorIncubationMin:VectorIncubationMax,1)
							Deathlambda=runif(1,min=VectorLambdaMin[t],max=VectorLambdaMax[t])
							Death[j]=1+rgeom(1,prob=1/Deathlambda)
							if (Death[j]>Incubation[j]){
								Contribution=runif(1,min=RiskToHouseMin,max=RiskToHouseMax)
								##########################
								#### Mosquito Movement ###
								##########################
								#### For nearest neighbours, use 1 in 'HousesleNAway'
								#### For nearest neighbours plus houses 2 away use 2 (Scenario 4)
								SurroundingHouses=setdiff(HousesleNAway(CurrentLocation,2),CurrentLocation)
								HomeContribution=Contribution*(1-Spread)
								NeighborContribution=Spread*Contribution/length(SurroundingHouses)
								NodeStatus[CurrentLocation,min((t+(Incubation[j]+1)),TotalTime):(min(t+Death[j],TotalTime))]=
									pmin(1,NodeStatus[CurrentLocation,min((t+(Incubation[j]+1)),TotalTime):(min(t+Death[j],TotalTime))]+HomeContribution)
								for (Neighs in SurroundingHouses){
									NodeStatus[Neighs,min((t+(Incubation[j]+1)),TotalTime):(min(t+Death[j],TotalTime))]=
										pmin(1,NodeStatus[Neighs,min((t+(Incubation[j]+1)),TotalTime):(min(t+Death[j],TotalTime))]+NeighborContribution)
								}
							}
						}
					}
				}
			}
			### Host Movement
			for (i in 1:Population){
				MoveChance=runif(1)
				#### Makes everybody spend on average 2 hours before "moving" (though can "move" to same spot)
				if (MoveChance<.5){
					MoveTo=sample(1:(MaxInSocial+MaxOutOfSocial),1,prob=PercentMove[i,])
					CurrentLoc[i,]=Locations[i,MoveTo,]
					NumCurrentLoc[i]=CurrentLoc[i,2]+CityLength*(CurrentLoc[i,1]-1)
				}
			}
		}
	}
}

	
Days=matrix(1:TotalTime,TotalTime/12,12,byrow=T)
TotalInfection=rep(0,TotalTime/12)
for (t in 1:(TotalTime/12)){
	for (hour in Days[t,]){
		Potential=which(InfectionInfo[,1]==hour & InfectionInfo[,5]<1e5)
		TotalInfection[t]=TotalInfection[t]+length(Potential)
	}
}
text=sprintf("TI_AT_%i_%i.csv",Forray,SPREAD)
TI=rbind(TI,TotalInfection)
write.csv(TI,text)
text=sprintf("Sample_AT_%i%i_%i.csv",RUN,Forray,SPREAD)
write.csv(tempSample,text)
}
}
}
