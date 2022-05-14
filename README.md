# Projet - UE Apprentissage statistique

### Fait par : Maroua Lachhab & Nabil Zaita

## I. Cas général
### 1. Fonction estimateur
```
EstimNaif=function(x,Data,h){
  X=subset(Data, select = -label) # X represente toute les donnees sans le label
  label=subset(Data, select = label) # On definit le label
  Distances=sqrt(rowSums((x-X)^2)) # On calcul la distance a x
  labelh=label[Distances<=h] # On selectionne ceux qui sont dans la boule de rayon h autour de x
  return(labelh[which.max(labelh)]) # On décompte le nombre de chaque label et on prend le plus represente
}
```
### 2. Simulation de données
```
SimulData1=function(Nobs=100,Npt=2){
  #simulation de Xi
  X=matrix(0,ncol=Npt,nrow=Nobs)
  for (i in 1:Npt){
    m=rnorm(Npt,0.5,1)
    sd=rep(1,Npt)
    X[,i]=rnorm(Nobs,m[i],sd[i])
  }
  #simulation du label
  label=sample(0:2,Nobs, replace=T)
  #concatenation
  cbind(X,label)
}
```
```
SimulData2=function(Nobs,mx=c(-5,0,5),my=c(-7,7),sd=1)
	{
  #taille du premier echantillon
	N1=rbinom(1,Nobs,0.5)
	#taille du deuxieme echantillon
	N2=Nobs-N1
	i1=sample(1:length(mx),Nobs,replace=TRUE)
	j1=sample(1:length(my),Nobs,replace=TRUE)
	Data=cbind(rnorm(Nobs,mx[i1],sd),rnorm(Nobs,my[j1],sd)) #Simulation de X
	Y1=rep(0,N1)
	Y2=rep(1,N2)
	label=c(Y1,Y2) #simulation du label
  return(cbind(Data,label))
}
```
### 3. Plot de la classification obtenue
```
DataSimul1=SimulData1()

plot(DataSimul1[,1],DataSimul1[,2],col=c("red","blue","green"),xlab="X1", 
     ylab="X2",main="Repartition des labels selon les donnees 1")
legend("bottomleft",title="Valeur du label", legend = c("0","1","2"),
       fill=c("red","blue","green"))
```
![Plot de la classification obtenue](https://raw.githubusercontent.com/NabilZaita/Apprentissage-Stat/main/img1.png)
```
DataSimul2=SimulData2()

plot(DataSimul2[,1],DataSimul2[,2],col=c("red","blue","green"),xlab="X1", 
     ylab="X2",main="Repartition des labels selon les donnees 2")
legend("bottomleft",title="Valeur du label", legend = c("0","1","2"),
       fill=c("red","blue","green"))
```
