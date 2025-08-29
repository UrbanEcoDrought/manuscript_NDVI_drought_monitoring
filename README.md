# manuscript_NDVI_drought_monitoring
scripts that encompass the analysis used in the NDVI drought monitoring manuscript

Descriptions:
- 000_Calculate_GAMM_Derivatives.R - function to get derivative of GAM object and confidence interval around it
- 00_Google_Earth_Engine_Functions.R - common earth engine functions to help with extraction script
- 01_setup_NLCD.R - creates harmonized NLCD layer with land cover info for all the years included in Chicago region
- 02_extract_Landsat_NDVI_5-9.R - extracts raw landsat NDVI data for landsat 5,7,8, and 9 for each land cover type in the region
- 03_combine_Landsat_NDVI_data.R - combines landsat data for each land cover type into one big dataframe, with a few line plots of NDVI vs. time

