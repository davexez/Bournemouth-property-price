### Insight into property prices in Bournemouth County, England
library(ggplot2)
library(latex2exp)
library(GGally)
library(car)
library(lmtest)
library(dplyr)

#' read data into R
Bournemouth <-
	read.csv("BOURNEMOUTH.csv", header = T, sep = ",")
str(data)

####'* what about the areas and affordability?* ####

unique(Bournemouth$district)   # just one district, same name as the county

unique(Bournemouth$locality)
sum(is.na(Bournemouth$locality)) / nrow(Bournemouth)   ## too many missing values

table(Bournemouth$towncity, useNA = "ifany")    ## variability is too low

table(Bournemouth$lad11nm, useNA = "ifany")     ## same as local authority label

table(Bournemouth$rgn11nm, useNA = "ifany")

table(Bournemouth$CONSTITUENCY_LABEL, useNA = "ifany")

##' since the choices are scarce by looking at the location variable, it is 
##' better to get the prefix of the postcode and do an evaluation from there

Bournemouth$postcode_prefix <- 
	sapply(Bournemouth$postcode,function(x){strsplit(x,split=' ')[[1]][1]})
sort(table(Bournemouth$postcode_prefix))

#' SO30 belongs to another county, there is a mistake in the dataset, so it is
#' better to remove that unit from the dataset
Bournemouth <- 
	Bournemouth %>% filter(postcode_prefix %in% paste0(rep("BH",11),1:11))
 
####' Model exploratory analysis and fitting  ####

use.var <-
	c(
		"price",
		"priceper",
		"tfarea",
		"numberrooms",
		"CURRENT_ENERGY_EFFICIENCY",
		"ENVIRONMENT_IMPACT_CURRENT",
		"propertytype",
		"propertytype_epc",
		"BUILT_FORM"
	)

#' for the energy variables we do not include the one called "potential..."
#'  because they represent the energetic availability
#' the current energy is by far more informative in this study
#' 
#' co2 emission variables are disregarded because the environment rating
#' accounts for this. the higher the rating, the lower the emissions
#' 
#' Furthermore, variables like energy "consumption" are not included; 
#' the efficiency already gathers the usage for their cost
#' water, heating, lightning specific variables are excluded since 
#' they are all summarized in the efficiency 

price.data <- Bournemouth[, use.var]    #subset of data with cols of interest

#' check for missing data
colSums(is.na(price.data))

str(price.data)
#' observe "priceper" * "tfarea" gives "price" column
all(round(price.data$priceper * price.data$tfarea) == price.data$price)

summary(price.data)

#' we have to choose one between the categorical variables that are similar
#' let's see which are their components
apply(price.data[,7:9], MARGIN = 2, FUN = unique)
#' it seems like "BUILT_FORM" is not extremely worthy to consider
#' it regards structure if part of a housing complex

round(
	table(price.data$propertytype, price.data$propertytype_epc) / nrow(price.data),
			3)
#' better to choose just one
#' cross table suggest the categories are really dependent between one another
#' i.e they display more or less the same picture.
#' In order to make the choice, let's compare effect on the main variables
#' of our question of interest
#' Make boxplots varying for each level of the categories
g <- ggplot(price.data)
g + geom_boxplot(aes(price)) + facet_grid(propertytype ~ .)
g + geom_boxplot(aes(price)) + facet_grid(propertytype_epc ~ .)
g + geom_boxplot(aes(tfarea)) + facet_grid(propertytype ~	.)
g + geom_boxplot(aes(tfarea)) + facet_grid(propertytype_epc ~ .)
#' Decision: choose "propertytype_epc" , the differences in the levels
#' are more balanced and the outliers are equally spread


##' Visualize numerical variables to gain insight on variability
par(mfrow = c(3, 2))
boxplot(
	price.data$price,
	main = "Property Price",
	horizontal = T,
	xlab = "?"
)
boxplot(
	price.data$priceper,
	main = "Price per square metre",
	horizontal = T,
	xlab = "?"
)
boxplot(
	price.data$tfarea,
	main = "Total Floor Area",
	horizontal = T,
	xlab = TeX("$\\m^2$")
)
boxplot(price.data$numberrooms,
				main = "Number of Rooms",
				horizontal = T)
boxplot(
	price.data$CURRENT_ENERGY_EFFICIENCY,
	main = "Current Energy Efficiency",
	horizontal = T
)
boxplot(
	price.data$ENVIRONMENT_IMPACT_CURRENT,
	main = "Environmental Impact Rating",
	horizontal = T
)
#' looking at the boxplots there is no issue regarding low variability
par(mfrow = c(1,1))


##' Check correlation
ggpairs(price.data[, 1:6])
#' looking at the data, price as a response is more appropriate
#' because the linearity with the predictors  is more evident rather than for
#' "priceper"

#' also number of rooms and tfarea are highly correlated, 
#' just like energy efficiency and environmental impact
#' we might check for multicollinearity later using VIF


#' Let's plot the distribution of the response variable
#' to get what's the best option of modelling
ggplot(price.data, aes(price)) +
	geom_histogram(aes(y = after_stat(density)),
								 fill = "khaki4",
								 color = 1,
								 alpha = 0.4) +
	geom_density(
		color = "red",
		fill =  "peru",
		alpha = 0.5,
		lwd = 0.8,
		linetype = 3
	)+
	theme_light()+
	ggtitle("Distribution of Sale Price") + xlab("£")
#' right-skew distribution, normal assumption not adequate to the data
#' Gamma distribution seems the best option


##' Make categorical variable as a factor
price.data$propertytype_epc <-
	factor(price.data$propertytype_epc,
				 levels = unique(price.data$propertytype_epc))

#' FIT THE MODEL
#' We choose interaction between:
#' tfarea and n of rooms since price changes according to the sq. meters
#' However, presence of additional rooms implies different infrastructure 
#' which must change for houses with same tfarea but a difference in number of rooms
#' 
#' Also the type of building interacts with tfarea since spaces are organised 
#' differently and houses which are allowed to have storeys for sure will have
#' different prices in the market than flats even at the same area coverage 
price.mod <-
	glm(
		price ~ tfarea * numberrooms + tfarea * propertytype_epc +
			CURRENT_ENERGY_EFFICIENCY + ENVIRONMENT_IMPACT_CURRENT,
		family = Gamma(link = "log"),
		data = price.data
	)

summary(price.mod)
vif(price.mod)

vif(update(price.mod, . ~ . - tfarea:numberrooms - tfarea:propertytype_epc))
#' By removing the interactions (which of course inflated the VIF  for the main effects) 
#' we can see the situations is under control so we can keep all 
#' the variables in the model and make general observations on the regression 
#' coefficients without risking multicollinearity

step(price.mod)

mod2 <-
	update(price.mod, . ~ . - tfarea:numberrooms - tfarea:propertytype_epc - tfarea)
summary(mod2)
cbind(price.mod$aic, mod2$aic)
#' even though mod2 has less predictors, mod1 shows lower AIC.
#' Therefore, it is better to stick with the original model

par(mfrow = c(2, 2))
plot(price.mod)
mtext("DIAGNOSTICS FOR THE MODEL", side = 3, line = -12, outer = T)
plot(mod2)
#' also the diagnostic checks perform better for the complete model
#' scale location doesn't have a weird pattern

lrtest(update(price.mod, . ~ . - (tfarea:propertytype_epc)), price.mod)
#' interaction term of the tfarea and the type of building is significant
#' so the interaction has clearly an influence on the model

#' Interpretation of the model coefficients
exp(price.mod$coefficients)

#### PLOTS ####
##' Constituency_label is the best option among all the possible
##' variables expressing areas of the county because there's a fair partition
##' among East and West of Bournemouth, even if the other three categories
##' (East Dorset, Eastleigh and Poole) are still irrelevant if compared with
##' the former.
##' this is the best option because the other variables showed almost all observation
##' in a single level.

ggplot(Bournemouth, aes(price)) +
	geom_density(
		aes(fill = CONSTITUENCY_LABEL, color = CONSTITUENCY_LABEL),
		alpha = 0.2,
		linewidth = 0.4
	) +
	scale_fill_manual(values = c("hotpink", 
																				"mediumpurple3",
																				rep("whitesmoke", 3))) +
	scale_color_manual(values = c("hotpink2",
																					"mediumpurple3",
																					"gray68",
																					"green",
																					"gray45")) +
																						theme_minimal()



## to plot in the poster show only part about the two main curves
ggplot(subset(
	Bournemouth,
	CONSTITUENCY_LABEL == "Bournemouth East" | 
		CONSTITUENCY_LABEL == "Bournemouth West"
)
, aes(price)) +
	geom_density(
		aes(fill = CONSTITUENCY_LABEL, color = CONSTITUENCY_LABEL),
		alpha = 0.2,
		linewidth = 0.4
	) +
	scale_fill_manual(values = c("hotpink","mediumpurple"),
										name = NULL) +
	scale_color_manual(values = c("hotpink2","mediumpurple3"),
										 name = NULL) +
	theme_minimal()+
	theme(legend.position = c(0.8,0.8),
				legend.background = element_rect(fill = "white",
																				 colour = "whitesmoke"))+
	ggtitle("Property prices for Constituencies")+
	xlab("?")
	
