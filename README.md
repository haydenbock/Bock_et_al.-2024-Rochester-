# Bock_et_al.-2024-Rochester-
Data for Bock et al. 2024 Eco Letters


All analyses were conducted in the `ManuscriptAnalysis4.0.R` file. The script is organized by our study questions/hypotheses;
  Q1 - understanding how urbanization influences soil animal community composition
  Q2 - understanding the most important drivers of community composition
  Q3 - testing the urban homogenization hypothesis and determining stochastic/deterministic forces of urbanization


All data for the analyses originate from the `NMDS_Data.csv` file, which represents the most complete data file. Other files are subsetted or are other permutations of that original dataset. Data file naming convention is somewhat arbitrary and corresponds to what analysis the subset was used for. File paths within each analysis in the `.R` script are routed to the proper files for easy reference. 


METADATA:
Within the `NMDS_Data.csv` file, the rows correspond to "parks" at a given "Timepoint" and "Year" (a maximum of 4 rows per park). Data columns are as follows: 
  Park - Park ID number (categorical)
  Timepoint - Spring/Fall season
  Year - year of study
  Name - Identifying name of park/greenspace
  Municipality - jurisdiction with which the greenspace resides
  Urban_Kmeans_Cluster - ranked as "high" or "low" urbanization according to a kmeans clustering analysis
  MedianIncome - median household income in USD according to US Census data
  Distance.From.CityCenter(km) - distance of park from the geographic city center. measured in kilometers
  Park_Area(m2) - area of park, measured in square meters
  Perimeter-Area_ratio - as name implies; used to insinuated park "edginess"
  Population_Density_in_Surrounding_Area - mean population density in persons/sq mile according to US census
  Imperviousness_in_500m_boundary - quantity of impervious surfaces within 500m boundary of park, as       calculated through qGIS                        
 Soil_Water_Holding_Capacity - grams water held in soil after 24hrs post saturation                            BD_mean - density of soil, in g/cm3                                                  
 Soil_Nitrogen_Percentage - soil nitrogen content as a percentage of soil weight                            
 Soil_Carbon_Percentage - soil carbon content as a percentage of soil weight                              
 Soil_pH - measured soil pH                                                  
 MedianResidentAge - median resident age in years according to US census                         
 Soil_Sand - soil sand content as percentage total mass                                                
 Soil_Silt - soil silt content as percentage total mass                                              
 Soil_Clay - soil clay content as percentage total mass                                             
 Soil_Saturation - percentage of water in soil at collection time based on proportion to total WHC             Soil_Moisture - g water per g soil at sample collection                                             
 PC1_Urban - unused alternate urbanization diagnosis                                                 
 PC2_SoilSpatial  - unused alternate urbanization diagnosis                                           
 PC3_SoilResource  - unused alternate urbanization diagnosis                                        
 Abundance_per_kg - soil animals (count) per kg dry soil                                         
 Richness - soil animals (unique individuals) per kg dry soil                                                  BrayCurtis_BetweenPark - calculated Bray-Curtis dissimilarity by soil animal community (unitless)             Shannon_WholePark - calculated Shannon diversity dissimilarity by soil animal community (unitless)
 Entomobryidae:Araneae - count of organisms in organisms/kg dry soil