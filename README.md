# Methane emissions from dairies and cattle feedlots in Texas and California

Data and [R](https://www.r-project.org/) code for the analysis underlying [these](https://insideclimatenews.org/news/18082023/texas-dairy-among-states-biggest-methane-emitters/) [two](https://insideclimatenews.org/news/18082023/californias-top-methane-emitter-is-cattle-feedlot/
) Inside Climate News articles, estimating methane emissions from cattle held in concentrated animal feeding operations (CAFOs) in Texas and California.

### Methodology

The analysis scripts estimate total annual methane emissions for dairies and other cattle feedlots based on the number of animals and their type (milking dairy cows or other cattle), using information from the facilities' state wastewater permits. In Texas, these permits detail the maximum number of animals allowed to be present. In California, they document the most recent recorded animal numbers.

Our approach is modified from [methods](https://github.com/climatetracecoalition/methodology-documents/tree/main/Agriculture) developed by [Climate TRACE](https://climatetrace.org/), a nonprofit coalition that is working to provide a farm-by-farm inventory of greenhouse gas emissions.

We calculated enteric methane emissions, released directly from animals' digestive tracts, using emissions factors defined by the [Intergovernmental Panel on Climate Change (IPCC) in 2019](https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch10_Livestock.pdf). For North America, these assume annual emissions of 138 kg of methane per head for milking cows and 64 kg per head for other cattle.

Methane is also emitted from manure, especially under conditions of anaerobic microbial digestion when manure and urine is stored in lagoons at large dairies. To calculate these emissions, which vary with temperature, we used IPCC emissions factors [derived in 2006](https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_10_Ch10_Livestock.pdf). For North America, these range from 48 to 112 kg per head annually for dairy cows and from 1 to 2 kg per head for other cattle.

To apply the manure emissions factors depending on temperature, we calculated the 2022 average recorded temperature for the nearest [Automated Surface Observing System](https://mesonet.agron.iastate.edu/ASOS/) weather station to each dairy/feedlot, obtaining temperature data from the [Iowa Environmental Mesonet](https://mesonet.agron.iastate.edu/) using the [**riem** R package](https://docs.ropensci.org/riem/index.html).

Wastewater permits for dairies in California give numbers for mature dairy cows only, yet these facilities typically also house other cattle, including calves and heifers. This creates uncertainty in our estimates. But to account for these additional animals, for each facility we applied the ratio of milking to other cattle calculated for dairies across Texas.

To reduce methane emissions. California is investing in alternative manure management projects, which avoid storage in lagoons, and [dairy digesters](https://clear.ucdavis.edu/explainers/what-dairy-digester-and-how-does-it-affect-methane-emissions), which capture emissions from anaerobic digestion. Through geospatial joins and some manual matching, we were able to associate 217 state-funded projects with a specific dairy or feedlot and adjusted their manure emissions to account for the cuts claimed for each project.

### Scripts

There is a separate analysis script for each state:

* `tx.R`
* `ca.R`

### Other files/folders
* `data` Input data files:

    -  `tx_dairies_feedlots.csv` Information on CAFOs in Texas, derived from a [search of wastewater permits](https://www2.tceq.texas.gov/wq_dpa/index.cfm?fuseaction=home.permit_info_search) at the website of the Texas Commission on Environmental Quality. The `tx.R` script includes some web scraping to derive detailed information on each individual permit.
    -  `ca_dairies_feedlots.csv` Information on CAFOs in California, derived from a [search of wastewater permits](https://ciwqs.waterboards.ca.gov/ciwqs/readOnly/CiwqsReportServlet?inCommand=reset&reportName=RegulatedFacility) at the California State Water Resources Control Board's California Integrated Water Quality System Project.
    -  `manure_emissions_factors.csv` Emissions factors for manure from dairy and other cattle at different temperatures, [defined in 2006](https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/4_Volume4/19R_V4_Ch10_Livestock.pdf) by the IPCC.
    - `ca_mitigation.xlsx` Data on greenhouse gas mitigation projects in California, downloaded from the [California Climate Investments Project Map](https://webmaps.arb.ca.gov/ccimap/). This includes data on projects to reduce methane emissions from dairies and other cattle feedlots through alternative manure management or the installation of dairy digesters.
 
- `processed_data` Data files exported from the two analysis scripts above and from manual cleaning of the exported data.

### Questions/Feedback
Email Peter Aldhous at peter.aldhous@insideclimatenews.org.
