# ILSI Remote Repository
This is a repository for the ILSI Sampling Project created and maintained by Xianbin (Eric) Cheng. 

The simulation model includes three modules: contamination simulation, sampling plan simulation, and sampling evaluation. 

In the contamination simulation module, we generate random contamination spots with certain level of contamination and simulate the spreading. 
		*With discrete spread, the contamination points are scattered around the contamination spots in a bivariate normal distribution. The level of contamination of all the points follows a log normal distribution. 
		*With continuous spread, a circular area around the contamination spots is marked as the contamination zone and its level of contamination is defined by a decay function. The further away from the spot, the lower the contamination level.

In the sampling plan simulation module, we create three types of sampling plans: simple random sampling (SRS), stratified random sampling (STRS), and systematic sampling (SS).

In the sampling evaluation module, we overlay the sampling plan onto the contaminated field and calculate the rate of detection.
