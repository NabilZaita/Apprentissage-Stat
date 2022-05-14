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
SimulData=function(Nobs=100,Npt=2){
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
### 3. Plot de la classification obtenue
```
DataSimul=SimulData()

plot(DataSimul[,1],DataSimul[,2],col=c("red","blue","green"),xlab="X1", 
     ylab="X2",main="Repartition des labels selon les donnees")
legend("bottomleft",title="Valeur du label", legend = c("0","1","2"),
       fill=c("red","blue","green"))
```
