# urban-green-area-accessibility-prioritization
R scripts and Zonation input and output files for the research article Jalkanen, Fabritius, Vierikko, Moilanen &amp; Toivonen (2020), “Analyzing fair access to urban green areas using multimodal accessibility measures and spatial prioritization”, Applied Geography (doi:10.1016/j.apgeog.2020.102320).

# R scripts

1. 01_Distance-decays_of_travel_modes.r: Code for defining distance-decay functions for travels from home to a recreational area. Functions are defined separately for different travel modes (walking, biking, public transport) and they are based on a travel survey by Helsinki Region Transport Authority (Brandt et al. 2019).

2. 02_Green_area_accessibility_layers_from_cell-specific_travel_times.r: Code for creating raster layers depicting the accessibility of green areas in the Helsinki Metropolitan Area, separately from the point of view of all the metropole’s districts. Accessibility is based on modeled travel times (Tenkanen & Toivonen 2020) and distance-decay functions (previous code).

3. 03_Green_area_buffer_analysis_for_comparison.r: Code for calculating the number of people living within 500m buffer around different green area pixels in the Helsinki Metropolitan Area.

# Zonation files

Each folder contains standard Zonation input and output files for different analysis versions described in the article. The .bat files that execute each Zonation run are located in the corresponding folders. The “input” subfolders include the features_list.spp and settings.dat files for each run. The “output” subfolders include all files generated and named automatically by the Zonation software. For instance, the priority rank maps shown in the article are found in these subfolders. See the Zonation manual (Moilanen et al. 2014) for details about e.g. the usage, naming, or structure of the different files.

Zonation analysis versions are named as follows:
* walk = Analysis includes the accessibility of all green areas based on walking.
* bike = Analysis includes the accessibility of all green areas based on biking.
* pt = Analysis includes the accessibility of large forests based on public transportation.
* weights = The population-weighted version of the analysis. Here, each input raster layer (showing the accessibility of green areas from different city districts) is weighted by the population of the corresponding district.

# References

Brandt E, Kantele S & Räty P (2019). Liikkumistottumukset Helsingin seudulla 2018 (Travel habits in the Helsinki region in 2018). HSL Publications 9/2019. https://www.hsl.fi/sites/default/files/hsl_julkaisu_9_2019_netti.pdf

Tenkanen, H & Toivonen T (2020). Longitudinal spatial dataset on travel times and distances by different travel modes in Helsinki Region. Scientific Data 7: 1–15. https://doi.org/10.1038/s41597-020-0413-y

Moilanen, Pouzols FM, Meller L, Veach V, Arponen A, Leppänen J, Kujala H (2014) Zonation Version 4 user manual. C-BIG, University of Helsinki, Helsinki. 
