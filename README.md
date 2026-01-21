# UruguayFishingEffort
R scripts to estimate industrial fishing effort combining local registries and Global Fishing Watch (GFW) information

These scripts are associated to the publication: Estimation of fishing effort by the Uruguayan industrial fleet during 2024, by Luis Orlando and Daniel García. 

Workflow is linear: 
"1Data.R"           deals with gathering information on the vessels that integrate the Uruguayan industrial fleets from local sources
"2Fleet.R"          creates a depurated list of GFW vessel id
"3Port.R"           processes port events, filters by location and gathers information on port time by season
"4Fishing.R"        processes fishing event information filtering by duration and speed, produces seasonal estimations of effort
"geo_check1_1.R"    a generic function for checking geographical position for port events among other functionalities
"posicion1_1.R"     asigns code quadrant name according to latitude and longitude
"inv_posicion1_1.R" returns the central position (latitude and longitude) for given code quadrant

