#Pregunta2

library(markovchain)

me01<- matrix(c(0.5,0.5,0.5,0.5, 0.25,0.5,0.25, 0.25,0.25,0.5), byrow=TRUE, nrow=3)
matrix.trans01<- new('markovchain', transitionMatrix=me01, states= c('Superavit', ' Deficit' ,'Saldo 0'), name='Cadena de Markov')
matrix.trans01
plot(matrix.trans01)


library("markovchain")




me01<- matrix(c(0.5, 0.5,0.75,0.25), byrow=TRUE, nrow=2)
matrix.trans01<- new('markovchain', transitionMatrix=me01, states= c('Superavit', 'Deficit'), name='Cadena de Markov')
matrix.trans01
plot(matrix.trans01)

#En el largo plazo
steadyStates(matrix.trans01)


#Partiendo de la situación inicial de déficit la probabilidad de seguir en déficit para el siguiente mes
vector01<- c(0,1)
etapas01<- 2
final01<- vector01*(matrix.trans01^etapas01)
final01
final01[1,2]


#Partiendo de la situaci´no inicial de superávit la probabilidad de seguir en superávit para el siguiente mes
vector02<- c(1,0)
etapas02<- 2
final02<- vector02*(matrix.trans01^etapas02)
final02
final02[1,1]


#La proporción de llegar con sus cuentas a fin de mes a 0 supondrá la equiparación entre sus cuentas en superavit con las de déficit
vector03<- c(1,1)
etapas03<- 2
final03<- vector03*(matrix.trans01^etapas02)
final03
final03[1,1]


#También podemos ver ls simulación para el superavit y para el déficit según el comando rmarkovchain para un fenómeno aleatorio de n=1000 eventos 
simulacion_superavit <- rmarkovchain(n = 1000, object =matrix.trans01 , t0 = "Superavit")
simulacion_superavit


simulacion_deficit <- rmarkovchain(n = 1000, object =matrix.trans01 , t0 = "Deficit")
simulacion_deficit


#Estimador maximo verosimilitud (mle=maximum likelihood estimator)
emvclima <- markovchainFit(data = simulacion_superavit, method = "mle", name = "EMVclima")                                 
#markovchainFit, devuelve una CM de una secuencia dada.

emvclima$estimate

emvclima$standardError



