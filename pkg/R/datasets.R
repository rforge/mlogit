#' Stated preference data for the choice of electricity suppliers
#' 
#' panel data
#' 
#' \emph{number of observations} : 4308
#' 
#' \emph{observation} : households
#' 
#' \emph{country} : United States
#' 
#' @name Electricity
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{choice}{the choice of the individual, one of 1, 2, 3, 4,}
#' \item{id}{the individual index,}
#' \item{pfi}{fixed price at a stated cents per kWh, with the price varying
#' over suppliers and experiments, for scenario i=(1, 2, 3, 4),}
#' \item{cli}{the length of contract that the supplier offered, in years (such
#' as 1 year or 5 years.) During this contract period, the supplier guaranteed
#' the prices and the buyer would have to pay a penalty if he/she switched to
#' another supplier. The supplier could offer no contract in which case either
#' side could stop the agreement at any time. This is recorded as a contract
#' length of 0}
#' \item{loci}{is the supplier a local company,}
#' \item{wki}{is the supplier a well-known company}
#' \item{todi}{a time-of-day rate under which the price is 11 cents
#' per kWh from 8am to 8pm and 5 cents per kWh from 8pm to 8am. These
#' TOD prices did not vary over suppliers or experiments: whenever the
#' supplier was said to offer TOD, the prices were stated as above.}
#' \item{seasi}{a seasonal rate under which the price is 10
#' cents per kWh in the summer, 8 cents per kWh in the winter, and 6 cents per
#' kWh in the spring and fall. Like TOD rates, these prices did not vary.  Note
#' that the price is for the electricity only, not transmission and
#' distribution, which is supplied by the local regulated utility.}
#' }
#' @references Kenneth Train's home page :
#' \url{http://elsa.berkeley.edu/~train/}.
#' @source
#' 
#' Hubert J, Train K (2001) \dQuote{On the similarity of classical and Bayesian
#' estimates of individual mean pathworths}, \emph{Marketing Letters}, 12,
#' 259-269.
#' 
#' Revelt D, Train K (2000) \dQuote{Customer-specific taste parameters and
#' mixed logit}, Working Paper no. E00-274, Department of Economics, University
#' of California, Berkeley.
#' 
#' @keywords datasets
NULL


#' Choice of Fishing Mode
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 1182
#' 
#' \emph{observation} : individuals
#' 
#' \emph{country} : United States
#' 
#' @name Fishing
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{mode}{recreation mode choice, one of : beach, pier, boat and
#' charter}
#' \item{price.beach}{price for beach mode}
#' \item{price.pier}{price for pier mode}
#' \item{price.boat}{price for private boat mode}
#' \item{price.charter}{price for charter boat mode}
#' \item{catch.beach}{catch rate for beach mode}
#' \item{catch.pier}{catch rate for pier mode}
#' \item{catch.boat}{catch rate for private boat mode}
#' \item{catch.charter}{catch rate for charter boat mode}
#' \item{income}{monthly income}
#' }
#' @references Cameron, A.C.  and P.K.  Trivedi (2005) \emph{Microeconometrics
#' : methods and applications}, Cambridge, pp. 463--466, 486 and 491--495.
#' @source Herriges, J. A.  and C. L.  Kling (1999) \dQuote{Nonlinear Income
#' Effects in Random Utility Models}, \emph{Review of Economics and
#' Statistics}, \bold{81}, 62-72.
#' @keywords datasets
NULL


#' Ranked data for gaming platforms
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 91
#' 
#' \emph{observation} : individuals
#' 
#' \emph{country} : Netherlands
#' 
#' The data are also provided in long format (use in this case
#' \code{data(Game2)}). In this case, the alternative and the choice
#' situation are respectively indicated in the \code{platform} and
#' \code{chid} variables.
#' 
#' @name Game
#' @aliases Game Game2
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{ch.Platform}{where \code{platform} is one of \code{Xbox},
#' \code{PlayStation}, \code{PSPortable}, \code{GameCube},
#' \code{GameBoy} and \code{PC}. This variables contain the ranking of
#' the platforms from 1 to 6,}
#' \item{own.Platform}{these 6 variables are dummies which indicate
#' whether the given plaform is already owned by the respondent,}
#' \item{age}{the age of the respondent,}
#' \item{hours}{hours per week spent on gaming.}
#' }
#' @references Journal of Applied Econometrics data archive :
#' \url{http://jae.wiley.com/jae/}.
#' @source
#' 
#' Denis Fok, Richard Paap, and Bram van Dijk (2010) \dQuote{A Rank-Ordered
#' Logit Model with Unobserved Heterogeneity in Ranking Capabilities}, Journal
#' of Applied Econometrics, forthcoming
#' @keywords datasets
NULL

#' Heating and Cooling System Choice in Newly Built Houses in California
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 250
#' 
#' \emph{observation} : households
#' 
#' \emph{country} : California
#' 
#' @name HC
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{depvar}{heating system, one of gcc (gas central heat with
#' cooling), ecc (electric central resistence heat with cooling), erc
#' (electric room resistence heat with cooling), hpc (electric heat
#' pump which provides cooling also), gc (gas central heat without
#' cooling, ec (electric central resistence heat without cooling), er
#' (electric room resistence heat without cooling)}
#' \item{ich.z}{installation cost of the heating portion of the
#' system}
#' \item{icca}{installation cost for cooling}
#' \item{och.z}{operating cost for the heating portion of the system}
#' \item{occa}{operating cost for cooling}
#' \item{income}{annual income of the household}
#' }
#' @references Kenneth Train's home page :
#' \url{http://elsa.berkeley.edu/~train/}.
#' @keywords datasets
NULL

#' Heating System Choice in California Houses
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 900
#' 
#' \emph{observation} : households
#' 
#' \emph{country} : California
#' 
#' 
#' @name Heating
#' @docType data
#' @format A dataframe containing : \describe{
#'
#' \item{idcase}{id}
#' \item{depvar}{heating system, one of gc (gas central), gr (gas room), ec
#' (electric central), er (electric room), hp (heat pump)}
#' \item{ic.z}{installation cost for heating system z (defined for the
#' 5 heating systems)}
#' \item{oc.z}{annual operating cost for heating system z (defined for
#' the 5 heating systems)}
#' \item{pb.z}{ratio oc.z/ic.z }
#' \item{income}{annual income of the household} \item{agehed}{age of
#' the household head} \item{rooms}{numbers of rooms in the house}
#' }
#' @references Kenneth Train's home page :
#' \url{http://elsa.berkeley.edu/~train/}.
#' @keywords datasets
NULL

#' Japanese Foreign Direct Investment in European Regions
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 25764
#' 
#' \emph{country} : Europe
#' 
#' 
#' @name JapaneseFDI
#' @docType data
#' @format A dataframe containing : \describe{
#'
#' \item{firm}{the investment id}
#' \item{country}{the country}
#' \item{region}{the region (nuts1 nomenclature)}
#' \item{choice}{a dummy indicating the chosen region }
#' \item{choice.c}{the chosen country}
#' \item{wage}{wage rate in the region}
#' \item{unemp}{unemployment rate in the region}
#' \item{elig}{is the country eligible to european subsidies}
#' \item{area}{the area of the region}
#' \item{scrate}{ social charge rate (country level)}
#' \item{ctaxrate}{corporate tax rate (country level)}
#' \item{gdp}{regional gdp}
#' \item{harris}{harris' market potential}
#' \item{krugman}{krugman's market potential}
#' \item{domind}{domestic industry count}
#' \item{japind}{japan industry count}
#' \item{network}{network count}
#' }
#' @references Head, Keith and Thierry Mayer (2004) \dQuote{Market potential
#' and the location of Japanese investment in the european union}, \emph{the
#' Review of Economics and Statistics}, \bold{86(4)}, 959-972.
#' @source .
#' @keywords datasets
NULL

#' Mode Choice
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 453
#' 
#' \emph{observation} : individuals
#' 
#' 
#' @name Mode
#' @docType data
#' @format A dataframe containing :
#' \describe{
#' \item{choice}{one of car, carpool, bus or rail}
#' \item{cost.z}{cost of mode z}
#' \item{time.z}{time of mode z}
#' }
#' @references Kenneth Train's home page :
#' \url{http://elsa.berkeley.edu/~train/}.
#' @keywords datasets
NULL

#' Mode Choice for the Montreal-Toronto Corridor
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 3880
#' 
#' \emph{observation} : individuals
#' 
#' 
#' @name ModeCanada
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{case}{the individual index}
#' \item{alt}{the alternative, one of train, car, bus and air,}
#' \item{choice}{one if the mode is chosen, zero otherwise,}
#' \item{cost}{monetary cost,}
#' \item{ivt}{in vehicule time,}
#' \item{ovt}{out vehicule time,}
#' \item{frequency}{frequency,}
#' \item{income}{income,}
#' \item{urban}{urban,}
#' \item{noalt}{the number of alternatives available.} }
#' @references
#' 
#' Bhat, Chandra R. (1995) \dQuote{A heteroscedastic extreme value model of
#' intercity travel mode choice}, \emph{Transportation Research Part B},
#' \bold{29(6)}, 471-483.
#' 
#' Koppelman Franck S.  and Chieh-Hua Wen (2001) \dQuote{The paired
#' combinatorial logit model:properties, estimation and application},
#' \emph{Transportation Research Part B}, 75-89.
#' 
#' Wen, Chieh-Hua and Franck S. Koppelman (2001) \dQuote{The generalized nested
#' logit model}, \emph{Transportation Research Part B}, 627-641.
#' @keywords datasets
#' @examples
#' data("ModeCanada", package = "mlogit")
#' bususers <- with(ModeCanada, case[choice == 1 & alt == "bus"])
#' ModeCanada <- subset(ModeCanada, ! case %in% bususers)
#' ModeCanada <- subset(ModeCanada, noalt == 4)
#' ModeCanada <- subset(ModeCanada, alt != "bus")
#' ModeCanada$alt <- ModeCanada$alt[drop = TRUE]
#' KoppWen00 <- mlogit.data(ModeCanada, shape='long', chid.var = 'case',
#'                          alt.var = 'alt', choice='choice',
#'                          drop.index=TRUE)
#' pcl <- mlogit(choice~freq+cost+ivt+ovt, KoppWen00, reflevel='car',
#'               nests='pcl', constPar=c('iv.train.air'))
#' 
#' #use the subset of users who have the four alternatives available and who
#' #don't take the bus
#' 
#' bususers <- with(ModeCanada, case[choice == 1 & alt == "bus"])
#' ModeCanada <- subset(ModeCanada, ! case %in% bususers)
#' ModeCanada <- subset(ModeCanada, noalt == 4)
#' ModeCanada <- subset(ModeCanada, alt != "bus")
#' ModeCanada$alt <- ModeCanada$alt[drop = TRUE]
#' KoppWen00 <- mlogit.data(ModeCanada, shape='long', chid.var = 'case',
#'                           alt.var = 'alt', choice='choice',
#'                           drop.index=TRUE)
#' mlogit(choice~freq+cost+ivt+ovt, KoppWen00, reflevel='car')
#' z <- mlogit(choice~freq+cost+ivt+ovt, KoppWen00, reflevel='car', nests="pcl")
#' 
#' 
NULL

#' Technologies to reduce NOx emissions
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 9480
#' 
#' \emph{country} : United States
#' 
#' \emph{economic topic} : microeconomics
#' 
#' \emph{econometrics topic} : discrete choice
#' 
#' 
#' @name NOx
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{chid}{the plant id,}
#' \item{alt}{the alternative,}
#' \item{id}{the owner id,}
#' \item{choice}{the
#' chosen alternative,}
#' \item{available}{a dummy indicating that the alternative is
#' available,}
#' \item{env}{the regulatory environment, one of \code{'regulated'},
#' \code{'deregulated'} and \code{'public'},}
#' \item{post}{dummy for post-combustion polution control technology,}
#' \item{cm}{dummy for combustion modification technology,}
#' \item{lnb}{dummy for low NOx burners technology,}
#' \item{age}{age of the plant (in deviation from the mean age).}
#' \item{vcost}{variable cost,}
#' \item{kcost}{capital cost,}
#' }
#' @references Fowlie, Meredith (2010) \dQuote{Emissions Trading, Electricity
#' Restructuring, and Investment in Pollution Abatement}, \emph{American
#' Economic Review}, \bold{100(3)}, 837-869.
#' @source American Economic Association data archive :
#' \url{http://aeaweb.org/aer/}.
#' @keywords datasets
NULL

#' Risky Transportation Choices
#' 
#' a cross-section
#' 
#' \emph{number of observations} : 5405
#' 
#' \emph{country} : Sierra Leone
#' 
#' 
#' @name RiskyTransport
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{id}{individual id}
#' \item{choice}{1 for the chosen mode}
#' \item{mode}{one of \code{Helicopter},\code{WaterTaxi}, \code{Ferry}
#' and \code{Hovercraft}}
#' \item{cost}{the generalised cost of the transport mode}
#' \item{risk}{the fatality rate, numbers of death per 100,000 trips}
#' \item{weight}{weights}
#' \item{seats}{}
#' \item{noise}{}
#' \item{crowdness}{}
#' \item{convloc}{}
#' \item{clientele}{}
#' \item{chid}{choice situation id}
#' \item{african}{\code{yes} if born in Africa, \code{no} otherwise}
#' \item{lifeExp}{declared life expectancy}
#' \item{dwage}{declared hourly wage}
#' \item{iwage}{imputed hourly wage}
#' \item{educ}{level of education, one of \code{low} and \code{high}}
#' \item{fatalism}{self-ranking of the degree of fatalism}
#' \item{gender}{gender, one of \code{female} and \code{male}}
#' \item{age}{age}
#' \item{haveChildren}{\code{yes} if the traveler has children,
#' \code{no} otherwise}
#' \item{swim}{\code{yes} if the traveler knows how to swim, \code{no}
#' itherwise} }
#'
#' @references Gianmarco, Leon and Edward Miguel (2017) \dQuote{Risky
#' Transportation Choices and the Value of a Statistical Life}, \emph{American
#' Economic Journal: Applied Economics}, \bold{9(1)}, 202-28.
#' @source American Economic Association data archive :
#' \url{http://aeaweb.org/aer/}.
#' @keywords datasets
NULL


#' Stated Preferences for Train Traveling
#' 
#' a cross-section from 1987
#' 
#' \emph{number of observations} : 2929
#' 
#' \emph{observation} : individuals
#' 
#' \emph{country} : Netherland
#' 
#' 
#' @name Train
#' @docType data
#' @format A dataframe containing : \describe{
#' \item{id}{individual identifient}
#' \item{choiceid}{choice identifient}
#' \item{choice}{one of 'A' or 'B'}
#' \item{price_z}{price of proposition z (z = 'A', 'B') in cents of
#' guilders}
#' \item{time_z}{travel time of proposition z (z = 'A', 'B') in
#' minutes}
#' \item{comfort_z}{comfort of proposition z (z = 'A', 'B'), 0, 1 or
#' 2 in decreasing comfort order}
#' \item{change_z}{number of changes for proposition z (z = 'A', 'B')} }
#' @references Journal of Applied Econometrics data archive :
#' \url{http://jae.wiley.com/jae/}.
#' @source Ben--Akiva, M., D.  Bolduc and M.  Bradley (1993) \dQuote{Estimation
#' of travel choice models with randomly distributed values of time},
#' \emph{Transportation Research Record}, \bold{1413}, 88--97.
#' 
#' Meijer, Erik and Jan Rouwendal (2006) \dQuote{Measuring welfare effects in
#' models with random coefficients}, \emph{Journal of Applied Econometrics},
#' \bold{21}(2), 227-244.
#' @keywords datasets
NULL

#' mlogit package: estimation of random utility discrete choice models
#' by maximum likelihood
#'
#' mlogit provides a model description interface (enhanced
#' formula-data), a very versatile estimation function and a testing
#' infrastructure to deal with random utility models.
#'
#' @name mlogit-package
#' @docType package
#' @details For a gentle and comprehensive introduction to the
#'     package, see the package's vignettes.
NULL
