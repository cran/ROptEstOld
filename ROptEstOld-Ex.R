pkgname <- "ROptEstOld"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ROptEstOld')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BinomFamily")
### * BinomFamily

flush(stderr()); flush(stdout())

### Name: BinomFamily
### Title: Generating function for Binomial families
### Aliases: BinomFamily
### Keywords: models

### ** Examples

(B1 <- BinomFamily(size = 25, prob = 0.25))
plot(B1)
FisherInfo(B1)
checkL2deriv(B1)



cleanEx()
nameEx("ContIC-class")
### * ContIC-class

flush(stderr()); flush(stdout())

### Name: ContIC-class
### Title: Influence curve of contamination type
### Aliases: ContIC-class CallL2Fam<-,ContIC-method cent cent,ContIC-method
###   cent<- cent<-,ContIC-method clip clip,ContIC-method clip<-
###   clip<-,ContIC-method lowerCase lowerCase,ContIC-method lowerCase<-
###   lowerCase<-,ContIC-method neighborRadius neighborRadius,ContIC-method
###   neighborRadius<- neighborRadius<-,ContIC-method stand
###   stand,ContIC-method stand<- stand<-,ContIC-method
###   generateIC,ContNeighborhood,L2ParamFamily-method show,ContIC-method
### Keywords: classes

### ** Examples

IC1 <- new("ContIC")
plot(IC1)



cleanEx()
nameEx("ContIC")
### * ContIC

flush(stderr()); flush(stdout())

### Name: ContIC
### Title: Generating function for ContIC-class
### Aliases: ContIC
### Keywords: robust

### ** Examples

IC1 <- ContIC()
plot(IC1)



cleanEx()
nameEx("ContNeighborhood-class")
### * ContNeighborhood-class

flush(stderr()); flush(stdout())

### Name: ContNeighborhood-class
### Title: Contamination Neighborhood
### Aliases: ContNeighborhood-class
### Keywords: classes models

### ** Examples

new("ContNeighborhood")



cleanEx()
nameEx("ContNeighborhood")
### * ContNeighborhood

flush(stderr()); flush(stdout())

### Name: ContNeighborhood
### Title: Generating function for ContNeighborhood-class
### Aliases: ContNeighborhood
### Keywords: models

### ** Examples

ContNeighborhood()

## The function is currently defined as
function(radius = 0){ 
    new("ContNeighborhood", radius = radius) 
}



cleanEx()
nameEx("EvenSymmetric-class")
### * EvenSymmetric-class

flush(stderr()); flush(stdout())

### Name: EvenSymmetric-class
### Title: Class for Even Functions
### Aliases: EvenSymmetric-class
### Keywords: classes

### ** Examples

new("EvenSymmetric")



cleanEx()
nameEx("EvenSymmetric")
### * EvenSymmetric

flush(stderr()); flush(stdout())

### Name: EvenSymmetric
### Title: Generating function for EvenSymmetric-class
### Aliases: EvenSymmetric
### Keywords: robust

### ** Examples

EvenSymmetric()

## The function is currently defined as
function(SymmCenter = 0){ 
    new("EvenSymmetric", SymmCenter = SymmCenter) 
}



cleanEx()
nameEx("ExpScaleFamily")
### * ExpScaleFamily

flush(stderr()); flush(stdout())

### Name: ExpScaleFamily
### Title: Generating function for exponential scale families
### Aliases: ExpScaleFamily
### Keywords: models

### ** Examples

(E1 <- ExpScaleFamily())
plot(E1)
Map(L2deriv(E1)[[1]])
checkL2deriv(E1)



cleanEx()
nameEx("FixRobModel-class")
### * FixRobModel-class

flush(stderr()); flush(stdout())

### Name: FixRobModel-class
### Title: Robust model with fixed (unconditional) neighborhood
### Aliases: FixRobModel-class neighbor<-,FixRobModel-method
###   show,FixRobModel-method
### Keywords: classes models

### ** Examples

new("FixRobModel")



cleanEx()
nameEx("FixRobModel")
### * FixRobModel

flush(stderr()); flush(stdout())

### Name: FixRobModel
### Title: Generating function for FixRobModel-class
### Aliases: FixRobModel
### Keywords: models

### ** Examples

(M1 <- FixRobModel())

## The function is currently defined as
function(center = ParamFamily(), neighbor = ContNeighborhood()){
    new("FixRobModel", center = center, neighbor = neighbor)
}



cleanEx()
nameEx("FunSymmList-class")
### * FunSymmList-class

flush(stderr()); flush(stdout())

### Name: FunSymmList-class
### Title: List of Symmetries for a List of Functions
### Aliases: FunSymmList-class
### Keywords: classes

### ** Examples

new("FunSymmList", list(NonSymmetric(), EvenSymmetric(SymmCenter = 1), 
                        OddSymmetric(SymmCenter = 2)))



cleanEx()
nameEx("FunSymmList")
### * FunSymmList

flush(stderr()); flush(stdout())

### Name: FunSymmList
### Title: Generating function for FunSymmList-class
### Aliases: FunSymmList
### Keywords: robust

### ** Examples

FunSymmList(NonSymmetric(), EvenSymmetric(SymmCenter = 1), 
            OddSymmetric(SymmCenter = 2))

## The function is currently defined as
function (...){
    new("FunSymmList", list(...))
}



cleanEx()
nameEx("GammaFamily")
### * GammaFamily

flush(stderr()); flush(stdout())

### Name: GammaFamily
### Title: Generating function for Gamma families
### Aliases: GammaFamily
### Keywords: models

### ** Examples

distrExOptions("EupperTruncQuantile" = 1e-15) # problem with q(Gamma())(1) = NaN
(G1 <- GammaFamily())
FisherInfo(G1)
checkL2deriv(G1)
distrExOptions("EupperTruncQuantile" = 0) # default



cleanEx()
nameEx("GumbelLocationFamily")
### * GumbelLocationFamily

flush(stderr()); flush(stdout())

### Name: GumbelLocationFamily
### Title: Generating function for Gumbel location families
### Aliases: GumbelLocationFamily
### Keywords: models

### ** Examples

distrExOptions("ElowerTruncQuantile" = 1e-15) # problem with 
                                            # non-finite function value
(G1 <- GumbelLocationFamily())
plot(G1)
Map(L2deriv(G1)[[1]])
checkL2deriv(G1)
distrExOptions("ElowerTruncQuantile" = 0) # default



cleanEx()
nameEx("IC-class")
### * IC-class

flush(stderr()); flush(stdout())

### Name: IC-class
### Title: Influence curve
### Aliases: IC-class CallL2Fam CallL2Fam,IC-method CallL2Fam<-
###   CallL2Fam<-,IC-method checkIC,IC,missing-method
###   checkIC,IC,L2ParamFamily-method evalIC,IC,numeric-method
###   evalIC,IC,matrix-method infoPlot,IC-method plot,IC,ANY-method
###   show,IC-method
### Keywords: classes

### ** Examples

IC1 <- new("IC")
plot(IC1)



cleanEx()
nameEx("IC")
### * IC

flush(stderr()); flush(stdout())

### Name: IC
### Title: Generating function for IC-class
### Aliases: IC
### Keywords: robust

### ** Examples

IC1 <- IC()
plot(IC1)

## The function is currently defined as
IC <- function(name, Curve = EuclRandVarList(RealRandVariable(Map = list(function(x){x})), 
               Domain = Reals()), Risks, Infos, CallL2Fam = call("L2ParamFamily")){
    if(missing(name))
        name <- "square integrable (partial) influence curve"
    if(missing(Risks))
        Risks <- list()
    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                     dimnames=list(character(0), c("method", "message")))
    return(new("IC", name = name, Curve = Curve, Risks = Risks,
               Infos = Infos, CallL2Fam = CallL2Fam))
}



cleanEx()
nameEx("InfRobModel-class")
### * InfRobModel-class

flush(stderr()); flush(stdout())

### Name: InfRobModel-class
### Title: Robust model with infinitesimal (unconditional) neighborhood
### Aliases: InfRobModel-class neighbor<-,InfRobModel-method
###   show,InfRobModel-method
### Keywords: classes models

### ** Examples

new("InfRobModel")



cleanEx()
nameEx("InfRobModel")
### * InfRobModel

flush(stderr()); flush(stdout())

### Name: InfRobModel
### Title: Generating function for InfRobModel-class
### Aliases: InfRobModel
### Keywords: models

### ** Examples

(M1 <- InfRobModel())

## The function is currently defined as
function(center = L2ParamFamily(), neighbor = ContNeighborhood()){
    new("InfRobModel", center = center, neighbor = neighbor)
}



cleanEx()
nameEx("InfluenceCurve-class")
### * InfluenceCurve-class

flush(stderr()); flush(stdout())

### Name: InfluenceCurve-class
### Title: Influence curve
### Aliases: InfluenceCurve-class addInfo<- addInfo<-,InfluenceCurve-method
###   addRisk<- addRisk<-,InfluenceCurve-method Curve
###   Curve,InfluenceCurve-method Domain,InfluenceCurve-method Infos
###   Infos,InfluenceCurve-method Infos<- Infos<-,InfluenceCurve-method
###   Map,InfluenceCurve-method name,InfluenceCurve-method
###   name<-,InfluenceCurve-method Range,InfluenceCurve-method Risks
###   Risks,InfluenceCurve-method Risks<- Risks<-,InfluenceCurve-method
###   show,InfluenceCurve-method
### Keywords: classes

### ** Examples

new("InfluenceCurve")



cleanEx()
nameEx("InfluenceCurve")
### * InfluenceCurve

flush(stderr()); flush(stdout())

### Name: InfluenceCurve
### Title: Generating function for InfluenceCurve-class
### Aliases: InfluenceCurve
### Keywords: robust

### ** Examples

InfluenceCurve()

## The function is currently defined as
InfluenceCurve <- function(name, Curve = EuclRandVarList(EuclRandVariable(Domain = Reals())), 
                           Risks, Infos){
    if(missing(name))
        name <- "influence curve"
    if(missing(Risks))
        Risks <- list()
    if(missing(Infos))
        Infos <- matrix(c(character(0),character(0)), ncol=2,
                     dimnames=list(character(0), c("method", "message")))
    
    return(new("InfluenceCurve", name = name, Curve = Curve, 
               Risks = Risks, Infos = Infos))
}



cleanEx()
nameEx("L2ParamFamily-class")
### * L2ParamFamily-class

flush(stderr()); flush(stdout())

### Name: L2ParamFamily-class
### Title: L2 differentiable parametric family
### Aliases: L2ParamFamily-class FisherInfo FisherInfo,L2ParamFamily-method
###   L2deriv L2deriv,L2ParamFamily-method L2derivSymm
###   L2derivSymm,L2ParamFamily-method L2derivDistr
###   L2derivDistr,L2ParamFamily-method L2derivDistrSymm
###   L2derivDistrSymm,L2ParamFamily-method
###   checkL2deriv,L2ParamFamily-method
###   E,L2ParamFamily,EuclRandVariable,missing-method
###   E,L2ParamFamily,EuclRandMatrix,missing-method
###   E,L2ParamFamily,EuclRandVarList,missing-method
###   plot,L2ParamFamily,ANY-method
### Keywords: classes models

### ** Examples

F1 <- new("L2ParamFamily")
plot(F1)



cleanEx()
nameEx("L2ParamFamily")
### * L2ParamFamily

flush(stderr()); flush(stdout())

### Name: L2ParamFamily
### Title: Generating function for L2ParamFamily-class
### Aliases: L2ParamFamily
### Keywords: models

### ** Examples

F1 <- L2ParamFamily()
plot(F1)



cleanEx()
nameEx("LnormScaleFamily")
### * LnormScaleFamily

flush(stderr()); flush(stdout())

### Name: LnormScaleFamily
### Title: Generating function for lognormal scale families
### Aliases: LnormScaleFamily
### Keywords: models

### ** Examples

(L1 <- LnormScaleFamily())
plot(L1)
Map(L2deriv(L1)[[1]])
checkL2deriv(L1)



cleanEx()
nameEx("NonSymmetric-class")
### * NonSymmetric-class

flush(stderr()); flush(stdout())

### Name: NonSymmetric-class
### Title: Class for Non-symmetric Functions
### Aliases: NonSymmetric-class
### Keywords: classes

### ** Examples

new("NonSymmetric")



cleanEx()
nameEx("NonSymmetric")
### * NonSymmetric

flush(stderr()); flush(stdout())

### Name: NonSymmetric
### Title: Generating function for NonSymmetric-class
### Aliases: NonSymmetric
### Keywords: robust

### ** Examples

NonSymmetric()

## The function is currently defined as
function(){ new("NonSymmetric") }



cleanEx()
nameEx("NormLocationFamily")
### * NormLocationFamily

flush(stderr()); flush(stdout())

### Name: NormLocationFamily
### Title: Generating function for normal location families
### Aliases: NormLocationFamily
### Keywords: models

### ** Examples

(N1 <- NormLocationFamily())
plot(N1)
L2derivDistr(N1)



cleanEx()
nameEx("NormLocationScaleFamily")
### * NormLocationScaleFamily

flush(stderr()); flush(stdout())

### Name: NormLocationScaleFamily
### Title: Generating function for normal location and scale families
### Aliases: NormLocationScaleFamily
### Keywords: models

### ** Examples

(N1 <- NormLocationScaleFamily())
plot(N1)
FisherInfo(N1)
checkL2deriv(N1)



cleanEx()
nameEx("NormScaleFamily")
### * NormScaleFamily

flush(stderr()); flush(stdout())

### Name: NormScaleFamily
### Title: Generating function for normal scale families
### Aliases: NormScaleFamily
### Keywords: models

### ** Examples

(N1 <- NormScaleFamily())
plot(N1)
FisherInfo(N1)
checkL2deriv(N1)



cleanEx()
nameEx("OddSymmetric-class")
### * OddSymmetric-class

flush(stderr()); flush(stdout())

### Name: OddSymmetric-class
### Title: Class for Odd Functions
### Aliases: OddSymmetric-class
### Keywords: classes

### ** Examples

new("OddSymmetric")



cleanEx()
nameEx("OddSymmetric")
### * OddSymmetric

flush(stderr()); flush(stdout())

### Name: OddSymmetric
### Title: Generating function for OddSymmetric-class
### Aliases: OddSymmetric
### Keywords: robust

### ** Examples

OddSymmetric()

## The function is currently defined as
function(SymmCenter = 0){ 
    new("OddSymmetric", SymmCenter = SymmCenter) 
}



cleanEx()
nameEx("ParamFamParameter-class")
### * ParamFamParameter-class

flush(stderr()); flush(stdout())

### Name: ParamFamParameter-class
### Title: Parameter of a parametric family of probability measures
### Aliases: ParamFamParameter-class length,ParamFamParameter-method main
###   main,ParamFamParameter-method main<- main<-,ParamFamParameter-method
###   nuisance nuisance,ParamFamParameter-method nuisance<-
###   nuisance<-,ParamFamParameter-method show,ParamFamParameter-method
###   trafo trafo,ParamFamParameter-method trafo<-
###   trafo<-,ParamFamParameter-method
### Keywords: classes

### ** Examples

new("ParamFamParameter")



cleanEx()
nameEx("ParamFamParameter")
### * ParamFamParameter

flush(stderr()); flush(stdout())

### Name: ParamFamParameter
### Title: Generating function for ParamFamParameter-class
### Aliases: ParamFamParameter
### Keywords: robust

### ** Examples

ParamFamParameter(main = 0, nuisance = 1, trafo = diag(c(1,2)))

## The function is currently defined as
function(name, main = numeric(0), nuisance, trafo){
    if(missing(name))
        name <- "parameter of a parametric family of probability measures"
    if(missing(nuisance))
        nuisance <- NULL
    if(missing(trafo))
        trafo <- diag(length(main)+length(nuisance))

    return(new("ParamFamParameter", name = name, main = main, 
               nuisance = nuisance, trafo = trafo))
}



cleanEx()
nameEx("ParamFamily-class")
### * ParamFamily-class

flush(stderr()); flush(stdout())

### Name: ParamFamily-class
### Title: Parametric family of probability measures.
### Aliases: ParamFamily-class main,ParamFamily-method
###   nuisance,ParamFamily-method param,ParamFamily-method
###   plot,ParamFamily,ANY-method show,ParamFamily-method
###   trafo,ParamFamily-method
### Keywords: classes models

### ** Examples

F1 <- new("ParamFamily") # prototype
plot(F1)



cleanEx()
nameEx("ParamFamily")
### * ParamFamily

flush(stderr()); flush(stdout())

### Name: ParamFamily
### Title: Generating function for ParamFamily-class
### Aliases: ParamFamily
### Keywords: distribution

### ** Examples

F1 <- ParamFamily()
plot(F1)

## The function is currently defined as
function(name, distribution = Norm(), main = 0, nuisance, 
         trafo, param, props = character(0)){
    if(missing(name)) 
        name <- "parametric family of probability measures"
    if(missing(distrSymm)) distrSymm <- NoSymmetry()
    if(missing(param)) 
        param <- ParamFamParameter(name = paste("parameter of", name), 
                        main = main, nuisance = nuisance, trafo = trafo)
    return(new("ParamFamily", name = name, distribution = distribution, 
               distrSymm = distrSymm, param = param, props = props))
}



cleanEx()
nameEx("PoisFamily")
### * PoisFamily

flush(stderr()); flush(stdout())

### Name: PoisFamily
### Title: Generating function for Poisson families
### Aliases: PoisFamily
### Keywords: models

### ** Examples

(P1 <- PoisFamily(lambda = 4.5))
plot(P1)
FisherInfo(P1)
checkL2deriv(P1)



cleanEx()
nameEx("TotalVarIC-class")
### * TotalVarIC-class

flush(stderr()); flush(stdout())

### Name: TotalVarIC-class
### Title: Influence curve of total variation type
### Aliases: TotalVarIC-class CallL2Fam<-,TotalVarIC-method clipLo
###   clipLo,TotalVarIC-method clipLo<- clipLo<-,TotalVarIC-method clipUp
###   clipUp,TotalVarIC-method clipUp<- clipUp<-,TotalVarIC-method
###   lowerCase,TotalVarIC-method lowerCase<-,TotalVarIC-method
###   neighborRadius,TotalVarIC-method neighborRadius<-,TotalVarIC-method
###   show,TotalVarIC-method stand,TotalVarIC-method
###   stand<-,TotalVarIC-method
###   generateIC,TotalVarNeighborhood,L2ParamFamily-method
### Keywords: classes

### ** Examples

IC1 <- new("TotalVarIC")
plot(IC1)



cleanEx()
nameEx("TotalVarIC")
### * TotalVarIC

flush(stderr()); flush(stdout())

### Name: TotalVarIC
### Title: Generating function for TotalVarIC-class
### Aliases: TotalVarIC
### Keywords: robust

### ** Examples

IC1 <- TotalVarIC()
plot(IC1)



cleanEx()
nameEx("TotalVarNeighborhood-class")
### * TotalVarNeighborhood-class

flush(stderr()); flush(stdout())

### Name: TotalVarNeighborhood-class
### Title: Total variation neighborhood
### Aliases: TotalVarNeighborhood-class
### Keywords: classes models

### ** Examples

new("TotalVarNeighborhood")



cleanEx()
nameEx("TotalVarNeighborhood")
### * TotalVarNeighborhood

flush(stderr()); flush(stdout())

### Name: TotalVarNeighborhood
### Title: Generating function for TotalVarNeighborhood-class
### Aliases: TotalVarNeighborhood
### Keywords: models

### ** Examples

TotalVarNeighborhood()

## The function is currently defined as
function(radius = 0){ 
    new("TotalVarNeighborhood", radius = radius) 
}



cleanEx()
nameEx("asBias-class")
### * asBias-class

flush(stderr()); flush(stdout())

### Name: asBias-class
### Title: Standardized Asymptotic Bias
### Aliases: asBias-class
### Keywords: classes

### ** Examples

new("asBias")



cleanEx()
nameEx("asBias")
### * asBias

flush(stderr()); flush(stdout())

### Name: asBias
### Title: Generating function for asBias-class
### Aliases: asBias
### Keywords: robust

### ** Examples

asBias()

## The function is currently defined as
function(){ new("asBias") }



cleanEx()
nameEx("asCov-class")
### * asCov-class

flush(stderr()); flush(stdout())

### Name: asCov-class
### Title: Asymptotic covariance
### Aliases: asCov-class
### Keywords: classes

### ** Examples

new("asCov")



cleanEx()
nameEx("asCov")
### * asCov

flush(stderr()); flush(stdout())

### Name: asCov
### Title: Generating function for asCov-class
### Aliases: asCov
### Keywords: robust

### ** Examples

asCov()

## The function is currently defined as
function(){ new("asCov") }



cleanEx()
nameEx("asHampel-class")
### * asHampel-class

flush(stderr()); flush(stdout())

### Name: asHampel-class
### Title: Asymptotic Hampel risk
### Aliases: asHampel-class bound bound,asHampel-method
###   show,asHampel-method
### Keywords: classes

### ** Examples

new("asHampel")



cleanEx()
nameEx("asHampel")
### * asHampel

flush(stderr()); flush(stdout())

### Name: asHampel
### Title: Generating function for asHampel-class
### Aliases: asHampel
### Keywords: robust

### ** Examples

asHampel()

## The function is currently defined as
function(bound = Inf){ new("asHampel", bound = bound) }



cleanEx()
nameEx("asMSE-class")
### * asMSE-class

flush(stderr()); flush(stdout())

### Name: asMSE-class
### Title: Asymptotic mean square error
### Aliases: asMSE-class
### Keywords: classes

### ** Examples

new("asMSE")



cleanEx()
nameEx("asMSE")
### * asMSE

flush(stderr()); flush(stdout())

### Name: asMSE
### Title: Generating function for asMSE-class
### Aliases: asMSE
### Keywords: robust

### ** Examples

asMSE()

## The function is currently defined as
function(){ new("asMSE") }



cleanEx()
nameEx("asUnOvShoot-class")
### * asUnOvShoot-class

flush(stderr()); flush(stdout())

### Name: asUnOvShoot-class
### Title: Asymptotic under-/overshoot probability
### Aliases: asUnOvShoot-class width width,asUnOvShoot-method
###   show,asUnOvShoot-method
### Keywords: classes

### ** Examples

new("asUnOvShoot")



cleanEx()
nameEx("asUnOvShoot")
### * asUnOvShoot

flush(stderr()); flush(stdout())

### Name: asUnOvShoot
### Title: Generating function for asUnOvShoot-class
### Aliases: asUnOvShoot
### Keywords: robust

### ** Examples

asUnOvShoot()

## The function is currently defined as
function(width = 1.960){ new("asUnOvShoot", width = width) }



cleanEx()
nameEx("checkIC")
### * checkIC

flush(stderr()); flush(stdout())

### Name: checkIC
### Title: Generic Function for Checking ICs
### Aliases: checkIC
### Keywords: robust

### ** Examples

IC1 <- new("IC")
checkIC(IC1)



cleanEx()
nameEx("checkL2deriv")
### * checkL2deriv

flush(stderr()); flush(stdout())

### Name: checkL2deriv
### Title: Generic function for checking L2-derivatives
### Aliases: checkL2deriv
### Keywords: robust

### ** Examples

F1 <- new("L2ParamFamily")
checkL2deriv(F1)



cleanEx()
nameEx("fiBias-class")
### * fiBias-class

flush(stderr()); flush(stdout())

### Name: fiBias-class
### Title: Finite-sample Bias
### Aliases: fiBias-class
### Keywords: classes

### ** Examples

new("fiBias")



cleanEx()
nameEx("fiBias")
### * fiBias

flush(stderr()); flush(stdout())

### Name: fiBias
### Title: Generating function for fiBias-class
### Aliases: fiBias
### Keywords: robust

### ** Examples

fiBias()

## The function is currently defined as
function(){ new("fiBias") }



cleanEx()
nameEx("fiCov-class")
### * fiCov-class

flush(stderr()); flush(stdout())

### Name: fiCov-class
### Title: Finite-sample covariance
### Aliases: fiCov-class
### Keywords: classes

### ** Examples

new("fiCov")



cleanEx()
nameEx("fiCov")
### * fiCov

flush(stderr()); flush(stdout())

### Name: fiCov
### Title: Generating function for fiCov-class
### Aliases: fiCov
### Keywords: robust

### ** Examples

fiCov()

## The function is currently defined as
function(){ new("fiCov") }



cleanEx()
nameEx("fiHampel-class")
### * fiHampel-class

flush(stderr()); flush(stdout())

### Name: fiHampel-class
### Title: Finite-sample Hampel risk
### Aliases: fiHampel-class bound,fiHampel-method show,fiHampel-method
### Keywords: classes

### ** Examples

new("fiHampel")



cleanEx()
nameEx("fiHampel")
### * fiHampel

flush(stderr()); flush(stdout())

### Name: fiHampel
### Title: Generating function for fiHampel-class
### Aliases: fiHampel
### Keywords: robust

### ** Examples

fiHampel()

## The function is currently defined as
function(bound = Inf){ new("fiHampel", bound = bound) }



cleanEx()
nameEx("fiMSE-class")
### * fiMSE-class

flush(stderr()); flush(stdout())

### Name: fiMSE-class
### Title: Finite-sample mean square error
### Aliases: fiMSE-class
### Keywords: classes

### ** Examples

new("fiMSE")



cleanEx()
nameEx("fiMSE")
### * fiMSE

flush(stderr()); flush(stdout())

### Name: fiMSE
### Title: Generating function for fiMSE-class
### Aliases: fiMSE
### Keywords: robust

### ** Examples

fiMSE()

## The function is currently defined as
function(){ new("fiMSE") }



cleanEx()
nameEx("fiUnOvShoot-class")
### * fiUnOvShoot-class

flush(stderr()); flush(stdout())

### Name: fiUnOvShoot-class
### Title: Finite-sample under-/overshoot probability
### Aliases: fiUnOvShoot-class width,fiUnOvShoot-method
###   show,fiUnOvShoot-method
### Keywords: classes

### ** Examples

new("fiUnOvShoot")



cleanEx()
nameEx("fiUnOvShoot")
### * fiUnOvShoot

flush(stderr()); flush(stdout())

### Name: fiUnOvShoot
### Title: Generating function for fiUnOvShoot-class
### Aliases: fiUnOvShoot
### Keywords: robust

### ** Examples

fiUnOvShoot()

## The function is currently defined as
function(width = 1.960){ new("fiUnOvShoot", width = width) }



cleanEx()
nameEx("infoPlot")
### * infoPlot

flush(stderr()); flush(stdout())

### Name: infoPlot
### Title: Plot absolute and relative information
### Aliases: infoPlot
### Keywords: robust

### ** Examples

N <- NormLocationScaleFamily(mean=0, sd=1) 
IC1 <- optIC(model = N, risk = asCov())
infoPlot(IC1)



cleanEx()
nameEx("ksEstimator")
### * ksEstimator

flush(stderr()); flush(stdout())

### Name: ksEstimator
### Title: Generic Function for the Computation of the Kolmogorov Minimum
###   Distance Estimator
### Aliases: ksEstimator ksEstimator-methods
###   ksEstimator,numeric,Binom-method ksEstimator,numeric,Pois-method
###   ksEstimator,numeric,Norm-method ksEstimator,numeric,Lnorm-method
###   ksEstimator,numeric,Gumbel-method ksEstimator,numeric,Exp-method
###   ksEstimator,numeric,Gammad-method
### Keywords: robust

### ** Examples

x <- rnorm(100, mean = 1, sd = 2)
ksEstimator(x=x, distribution = Norm()) # estimate mean and sd
ksEstimator(x=x, distribution = Norm(mean = 1), param = "sd") # estimate sd
ksEstimator(x=x, distribution = Norm(sd = 2), param = "mean") # estimate mean
mean(x)
median(x)
sd(x)
mad(x)



cleanEx()
nameEx("leastFavorableRadius")
### * leastFavorableRadius

flush(stderr()); flush(stdout())

### Name: leastFavorableRadius
### Title: Generic Function for the Computation of Least Favorable Radii
### Aliases: leastFavorableRadius leastFavorableRadius-methods
###   leastFavorableRadius,L2ParamFamily,UncondNeighborhood,asGRisk-method
### Keywords: robust

### ** Examples

N <- NormLocationFamily(mean=0, sd=1) 
leastFavorableRadius(L2Fam=N, neighbor=ContNeighborhood(),
                     risk=asMSE(), rho=0.5)



cleanEx()
nameEx("lowerCaseRadius")
### * lowerCaseRadius

flush(stderr()); flush(stdout())

### Name: lowerCaseRadius
### Title: Computation of the lower case radius
### Aliases: lowerCaseRadius lowerCaseRadius-methods
###   lowerCaseRadius,L2ParamFamily,ContNeighborhood,asMSE-method
###   lowerCaseRadius,L2ParamFamily,TotalVarNeighborhood,asMSE-method
### Keywords: robust

### ** Examples

lowerCaseRadius(BinomFamily(size = 10), ContNeighborhood(), asMSE())
lowerCaseRadius(BinomFamily(size = 10), TotalVarNeighborhood(), asMSE())



cleanEx()
nameEx("optIC")
### * optIC

flush(stderr()); flush(stdout())

### Name: optIC
### Title: Generic function for the computation of optimally robust ICs
### Aliases: optIC optIC-methods optIC,L2ParamFamily,asCov-method
###   optIC,InfRobModel,asRisk-method optIC,InfRobModel,asUnOvShoot-method
###   optIC,FixRobModel,fiUnOvShoot-method
### Keywords: robust

### ** Examples

B <- BinomFamily(size = 25, prob = 0.25) 

## classical optimal IC
IC0 <- optIC(model = B, risk = asCov())
plot(IC0) # plot IC
checkIC(IC0, B)



cleanEx()
nameEx("optRisk")
### * optRisk

flush(stderr()); flush(stdout())

### Name: optRisk
### Title: Generic function for the computation of the minimal risk
### Aliases: optRisk optRisk-methods optRisk,L2ParamFamily,asCov-method
###   optRisk,InfRobModel,asRisk-method
###   optRisk,FixRobModel,fiUnOvShoot-method
### Keywords: robust

### ** Examples

optRisk(model = NormLocationScaleFamily(), risk = asCov())



cleanEx()
nameEx("radiusMinimaxIC")
### * radiusMinimaxIC

flush(stderr()); flush(stdout())

### Name: radiusMinimaxIC
### Title: Generic function for the computation of the radius minimax IC
### Aliases: radiusMinimaxIC radiusMinimaxIC-methods
###   radiusMinimaxIC,L2ParamFamily,UncondNeighborhood,asGRisk-method
### Keywords: robust

### ** Examples

N <- NormLocationFamily(mean=0, sd=1) 
radiusMinimaxIC(L2Fam=N, neighbor=ContNeighborhood(), 
                risk=asMSE(), loRad=0.1, upRad=0.5)



cleanEx()
nameEx("trAsCov-class")
### * trAsCov-class

flush(stderr()); flush(stdout())

### Name: trAsCov-class
### Title: Trace of asymptotic covariance
### Aliases: trAsCov-class
### Keywords: classes

### ** Examples

new("trAsCov")



cleanEx()
nameEx("trAsCov")
### * trAsCov

flush(stderr()); flush(stdout())

### Name: trAsCov
### Title: Generating function for trAsCov-class
### Aliases: trAsCov
### Keywords: robust

### ** Examples

trAsCov()

## The function is currently defined as
function(){ new("trAsCov") }



cleanEx()
nameEx("trFiCov-class")
### * trFiCov-class

flush(stderr()); flush(stdout())

### Name: trFiCov-class
### Title: Trace of finite-sample covariance
### Aliases: trFiCov-class
### Keywords: classes

### ** Examples

new("trFiCov")



cleanEx()
nameEx("trFiCov")
### * trFiCov

flush(stderr()); flush(stdout())

### Name: trFiCov
### Title: Generating function for trFiCov-class
### Aliases: trFiCov
### Keywords: robust

### ** Examples

trFiCov()

## The function is currently defined as
function(){ new("trFiCov") }



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
