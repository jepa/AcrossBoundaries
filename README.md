# Managing Across Boundaries 

This repository supports the Lenfest project; [Adaptive Allocation Strategies for Transboundary Fish Stocks](https://www.lenfestocean.org/en/research-projects/new-effort-to-inform-adaptive-allocation-strategies-for-transboundary-fish-stocks)

# Authors

Juliano Palacios-Abrantes<sup>1</sup>, Scott Crosson<sup>2</sup>, Chris Dumas<sup>3</sup>, Rod Fujita<sup>4</sup>, Arielle Levine<sup>5</sup>, Catherine Longo<sup>6</sup>, Olaf P. Jensen<sup>7</sup>
 
1. Center for limnologyLimnology, University of Wisconsin-Madison, Madison WI, US. 
2. NOAA Southeast Fisheries Science Center, Miami FL, US
3. Department of Environmental Sciences, University of North Carolina Wilmington, Wilmington NC, US
4. Environmental Defense Fund, 123 Mission Street, 28th Floor, San Francisco CA 94602, US 
5. Department of Geography, San Diego State University, San Diego CA, US
6. Science & Standards, Marine Stewardship Council, 1 Snow Hill, EC1A 2DH London, UK

*Corresponding author*: Juliano Palacios-Abrantes | j.palacios[at]oceans.ubc.ca


# Project background

Climate change is driving shifts in fish stock distributions that will affect resource availability across different jurisdictions around the world. Elinor Ostrom’s fundamental principles of common pool resource governance include a requirement for clear boundaries over resource units. However, when stock ranges shift, governance systems and boundaries are no longer aligned. This misalignment has already created conflicts around access and the re-distribution of benefits flowing from the stocks. This is a global challenge that is likely to increase as the effects of climate change intensify in the coming years. Specific problems that arise from the movement of stocks across jurisdictions include: overfishing; conflicts over access; unfairness to stakeholders who have borne the costs of resource stewardship but cannot capture the benefits due to stock movement; and high costs and conflicts associated with re-negotiating allocation, which is typically the most contentious aspect of fishery management.

# Objective and research questions

The objective of this tool is to evaluate policy options for managing fish stocks that are shifting across management boundaries, and to assess the socio-economic benefits and tradeoffs of these options for two important fisheries in the U.S. Mid-Atlantic.

1. Would  adaptive allocation  have reduced fuel use and carbon emissions, had it been applied when black sea bass and summer flounder stocks started to shift?

2. Would adaptive allocation have a positive economic impact on these fisheries, had it been applied when black sea bass and summer flounder stocks started to shift?

3.	How can conflicts around allocation be reduced in the context of shifting stocks?

4.	Where else in the U.S.  could this type of solution be applied to alleviate the challenges that arise when stocks shift?


# Partners

- [Arielle Levine, San Diego State University](https://geography.sdsu.edu/people/bios/levine)

- [Rod Fujita, Environmental Defense Fund](https://www.edf.org/people/rod-m-fujita)

- [Katie Longo, Marine Stewardship Council](https://www.researchgate.net/profile/Catherine-Longo)

- [Olaf Jensen, University of Wisconsin, Madison](https://limnology.wisc.edu/staff/jensen-olaf/)

- [Lisa Wainger, University of Maryland Center for Environmental Science](https://www.umces.edu/lisa-wainger)

- [Scott Crosson, NOAA](https://www.fisheries.noaa.gov/contact/scott-crosson-phd)

- [Chris Dumas, University of North Carolina Wilmington](https://csbapp.uncw.edu/data/fs/vita.aspx?id=8307)

- [Juliano Palacios-Abrantes, University of Wisconsin, Madison](https://limnology.wisc.edu/staff/palacios-abrantes-juliano/)


![](./Allocation_tool/www/logo_all2.png)

# Repository structure

## Data

### Processed 

This folder contains the data directly generated by our analysis and can be used to repplicate the results of the study as well as figures and tables. Please refere to the published paper for methods. It contains the following sub-folders and data: 

- **Interpolation**, contains the interpolation of each species analyszed (`tif_Centropristis_striata.csv`, `tif_Paralichthys_dentatus.csv`, and `tif_Stenotomus_chrysops.csv`. All data follows the same structure:
  - *index*, a unique identifier for each grid cell
  - *year*, year of data from 2970 to 2019
  - *region*, survey region (opt. Northeast US Fall or Northeast US Spring)
  - spp, species (Opt. black sea bass, yellowtail flounder, scup)
  - lon, longitude in degrees
  - lat, latitude in degrees
  - value, interpolation value as CPUE
- **grid_fp**, contains the shapefile and .csv file of the fishing ports grid
- **grid_sw**, contains the shapefile and .csv file of the state waters grid
- `top_ports.csv`, This is the list of top ports of the study. In addition to the previous structure:
  - species_name, species (Opt. black sea bass, yellowtail flounder, scup)
  - port_name, name of fishing port where catch is landed
  - state_postal, Abbreviation of state name
  - year_ton, year landings in tonns
  - year_value, year value of landings in USD
  - tresh, treshold relative to the top catches
  - lon, longitude in degrees
  - lat, latitude in degrees

### Raw

This folder contains the raw data used to create the analysis for this project. Please refere to the original data sources for specifics on methods. It contains the following sub-folders:

- OceanAdapt, contains the bottom trawl data hosted and cleaned by [OceanAdapt](https://oceanadapt.rutgers.edu/) for each species analyzed (_Centropristis striata_, *Paralichthys dentatus*, and *Stenotomus chrysops*. Please see *Pinsky, et al. 2013 doi: 10.1126/science.1239352* for further information. All data (`obs_Centropristis_striata.csv, obs_Paralichthys_dentatus.csv, obs_Stenotomus_chrysops.csv` ) follows the same structure:
  - *region*, survey region (opt. Northeast US Fall or Northeast US Spring)
  - haulid, a unique identifier for haul
  - year, yer haul was performed
  - spp, species scientific name
  - wtcpue, catch per unit of effort in wet tones
  - common, common species name
  - stratum, stratum of haul collected
  - stratumarea, area of strutum
  - lon, longitude in degrees
  - lat, latitude in degrees
  - depth, depth of haul in meters

- ACCSP,  Public, Annual Landings, Value, Summaries by port for black sea bass, scup and summer flounder, from NC-ME; generated by John Doe; using  the Atlantic Coastal Cooperative Statistics Program [(ACCSP)](https://www.accsp.org) Data Warehouse [online application], Arlington, VA. In: Atlantic Coastal Cooperative Statistics Program. See data for basic structure (`PalaciosJ - BSB, SFlounder, Scup by Port 1970-2020 - 2021.10.25.xlsx`)

## Figures
Folder containing figures for manuscript. Note these might not be the final manuscript figures.

## References
Files related to manuscript references

## Scripts
Folder containing `rmarkdown` scripts for data analysis, papers, etc. See script `Spatial_analisis.Rmd` for paper reproductibillity


