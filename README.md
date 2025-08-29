# manuscript_NDVI_drought_monitoring
scripts that encompass the analysis used in the NDVI drought monitoring manuscript

Descriptions:
- 000_Calculate_GAMM_Derivatives.R - function to get derivative of GAM object and confidence interval around it
- 00_Google_Earth_Engine_Functions.R - common earth engine functions to help with extraction script
- 0_Calculate_GAMM_Posteriors.R - function to calculate posterior distribution for GAM object
- 01_setup_NLCD.R - creates harmonized NLCD layer with land cover info for all the years included in Chicago region
- 02_extract_Landsat_NDVI_5-9.R - extracts raw landsat NDVI data for landsat 5,7,8, and 9 for each land cover type in the region
- 03_combine_Landsat_NDVI_data.R - combines landsat data for each land cover type into one big dataframe, with a few line plots of NDVI vs. time
- 04_raw_and_harmonized_NDVI_all_LC.R - takes raw NDVI data and runs initial GAM for each land cover to harmonize across landsat missions
- 05_calculate_norms_all_LC.R - calculates unique NDVI "normals" using GAMs for each land cover type over a typical year
- 06_year_splines_all_LC.R - calculates NDVI using GAMs for each land cover and each unique year in the data
- 07_growing_season_dates.R - calculates growing season data range for each land cover class, makes table of dates found in appendix
- 08_norms_derivatives.R - calculates first derivatives of norm curves
- 09_year_splines_derivatives.R - calculates first derivatives of year splines
- 10_figure_2_raw_vs_harmonized_mission_curves.R - generates figure 2 of raw vs. harmonized NDVI and table for appendix
- 11_anovas_tukey_tests.R - analysis of variance tests for each land cover, drought category, and NDVI anomaly; Tukey tests and table for appendix; weighted averages for USDM data
- 12_figure_3_boxplot_anomalies.R - generates figure 3, boxplot and barplot with letters
- 13_format_dataframe_for_figure_4.R - formats anomalies and derivative anomalies into a cleaner dataframe for figure 4
- 14_figure_4_ndvi_anomaly_panels.R - generates figure 4, NDVI anomaly panels for each land cover class with lines for each year
- 15_figure_5_ndvi_anom_deriv_anom_year_panels.R - generates figure 5, NDVI anomaly and first derivative panels with lines as each land cover
- 16_tables_1&2_significant_dates.R - creates tables 1 and 2 of dates comparing to USDM onset

