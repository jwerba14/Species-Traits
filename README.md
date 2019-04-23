R scripts (as April 23, 2019):
ODE_attempt1.R - has full ODE including no3 and ceriodaphnia, runs, described in script

transfer_functions.R - transfer functions including hollings II, michaelis-menten, logistic growth, runs, described in script

nls_feeding.R- fit hollings type II or type I to chlorophyll data by adult, juvenile daphnia and ceriodaphnia, as well as to extracted data from literature, needs controls added, runs, described in script

nutrient_air.R - fits linear model over time for NO3 adn NH4, doesn't run due to bblme not working with current version of R, unclear if correct method, described in script

data_change.R - not clear what this script does, mle2 doesn't run, fits michaelis menten to algae but not well, not described in script

nh4_excretion.R - trying to get parameters for NH4 but doesn't complete it and doesn't fully run, not described in script

daphnia_life_params.R - summarized fecundity, growth and death of daphnia magna, simple first look at data, runs, not described in script

basic_cerio_pop_curves.R - lots of plots of ceriodaphnia growth curves, some attempts with writing equations for growth curves but not complete, runs, not described in script

nh4_prac.R - gets parameter for chlorophyll uptake, needs to be gone through, runs, not described in script

cerio_param_fit.R - based on nh4_prac.R, doesn't finish but reasonable start, runs, not descried in script

nh4_param_predictions.R - set up for sum of squares of predicted vs actual, doesn't run, not described in script

ode_pred_algae.R - graphs predicted chlorophyll vs actual chlorophyll for single rep, runs, described in script

Data files
Feeding_lit_extraction.csv - data from Kersting and Van der Leeuw Hydrobiologia Vol 49, 1976

Daphnia_large_Feeding_Nov11.csv - adult daphnia uptake chlorophyll and NH4 for 7 different concentrations of chlorophyll, my experiment

Ceriodaphnia_Feeding_Nov07_2017.csv - ceriodaphnia feeding on chlorophyll and Nh4 excretion, 5 treatments, my experiment

Small_Daph_Feeding.csv - juvenile daphnia feeding on chlorophyll and Nh4 excretion, my experiment

NO3_Air.csv - 5 treatments of start to finish NO3 over 4 days my experiment

NH4_Air.csv - 5 treatments of NH4 change over 5 days, my experiment

Algae_Nutrient.csv - 6 treatments of starting chlorophyll/NH4 over 11 days, my experiment

daphnia_lifetime.csv - full data for daphnia magna life (birth, death, and growth), my experiment

cerio_pop.csv - data from ceriodaphnia population growth, my experiment

Algae_Nh4.csv - chlorophyll and nh4 change over 2-3 days with only algae, my experiment

Feeding_lit_cerio.csv - ceriodaphnia feeding from Obrlen_DeNoyelles 1974, should be combined with D. magna Feeding_lit_extraction.csv

Algae_No3_uptake_werba.xls - chlorophyll and No3 over time, missing data? my experiment

algae_uptake.xls - algae uptake of Nh4 in one day, my experiment

cerio_high_feeding.xls - 2 high chlorophyll treatments in single day, my experiment

Data_Sheet_FeedExpt - juvenile daphnia feeding experiment so both chlorophyll and nh4


