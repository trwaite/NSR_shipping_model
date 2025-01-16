<!-------------------------->
<!-------------------------->
# <a name="Introduction"></a>Introduction
<!-------------------------->
<!-------------------------->

`NSR_shipping_model` is a repository containing the R code used to project the economic competition between shipping goods along the Northern Sea Route (NSR) versus the Suez canal route (SCR) considering changing sea ice thickness and fuel costs. The repository includes a script to project theoretical route choice shares (NSR vs SCR) for shipments between Murmansk, Russia and Dongjiakou, China (`route_choice_murmansk_dongjiakou.R`) used in Brelsford et al. (in prep) as well as a script projecting quantity of shipments, shipping traffic along the NSR, and resulting CO2 emissions from the case study of shipping oil produced in the offshore Russian Arctic to Dongjiakou (`shipping_model_arctic_oil`) used in Waite et al. (in prep). 


The repository also contains code to generate gridded emissions data (netCDF files) along the NSR corresponding with the projected CO2 emissions from shipping.

<br>

<!-------------------------->
<!-------------------------->
# <a name="Methods"></a>Methods
<!-------------------------->
<!-------------------------->

## Shipping Model

### NSR navigability
The NSR, from the port of Murmansk to the Bering Strait, is partitioned into 36 subzones (Cheaitou et al. (2019). Gridded sea ice thickness projections from Phase 6 of the Coupled Model Intercomparison Project (CMIP6) are used to calculate monthly average sea ice thickness in each subzone under different climate change scenarios. Sea ice thickness thresholds corresponding to each vessel type (1A and 1AS) are applied to determine navigability and navigation constraints. These include, listed from thickest ice to thinnest ice, a navigability threshold (above which the vessel cannot cross the subzone), an ice breaker escort threshold (above which an ice breaker escort is needed and vessel speed is limited to the speed of the ice breaker), and a speed reduction threshold (above which the vessel can travel unaccompanied but its speed is reduced). When sea ice thickness is below the speed reduction threshold, the vessel is free to navigate at its maximum speed.

### Transit costs
The cost for a vessel to traverse the NSR consists of (1) the cost of fuel consumed in each NSR subzone crossed, (2) the cost of fuel consumed outside of the NSR, (3) the cost of an ice breaker escort if needed, (4) the cost of operations, (5) the suez canal fee, and (6) the cost of emissions in the case of a low carbon transition scenario. These costs are determined as described below:
1. Fuel consumption is a function of vessel speed $`S`$, subzone length $`l`$, the vessel's maximum speed $`D`$, and the vessel's daily fuel consumption at maximum speed $`f`$:
   
   $`F=\frac{l}{24*S}*f*\frac{S}{D}`$

   The cost of fuel is calculated as the fuel consumption multiplied by the fuel price, which can vary over time.
   
3. Vessels are assumed to travel at their maximum speed outside of the NSR (in the absence of sea ice) and consume fuel accordingly.
4. Ice breaker escort fees follow the tariff levels published by the Russian government, which depend on the season and the number of NSR zones in which an escort is needed.
5. Operation fees depend on the duration of the transit and a constant daily operation cost (a parameter).
6. The Suez canal fee is applied for the Suez Canal Route only.
7. In low carbon transition scenarios, the corresponding carbon price is applied to the total shipping CO2 emissions, calculated as explained below.

### CO2 Emissions
CO2 emissions from a shipment traversing a given route are calculated based on the total fuel consumed to traverse the route multiplied by a fixed CO2 emissions coefficient (a parameter). For NSR subzones in which an ice breaker escort is used, ice breaker emissions are calculated based on sea ice thickness (which impacts fuel consumption) and the same CO2 emissions coefficient.

### Route choice
When any subzone of the NSR has sea ice thickness above the navigability threshold in a given month, all shipments are assumed to traverse the SCR. Otherwise, the proportion of shipments traversing the NSR ($`P_{NSR}`$) versus the SCR is projected using a logit choice function applied to the comparative costs of shipping over the NSR ($`C_{NSR}`$) and the SCR ($`C_{SCR}`$). The logit exponent parameter $`\beta`$ can be varied to represent different elasticities as in Brelsford et al. (in prep):

$`P_{NSR}=\frac{\exp{\left(-\beta*C_{NSR}\right)}}{\exp{\left(\beta*C_{NSR}\right)}+\exp{\left(\beta*C_{SCR}\right)}}`$
