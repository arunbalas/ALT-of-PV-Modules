# ALT-of-PV-Modules
Acceleration Factor (AF) prediction for accelerated life testing of photovoltaic (PV) modules plays a major role in estimating the reliability of modules in the field. Accelerated stress tests are designed in such a way that the test replicates the failure mechanism that the module experiences outdoor. However, in most cases, both accelerated test degradation data and field degradation data on identical PV modules are not available. In such cases, it is hard to design a test plan and determine the degradation threshold or number of hours (or cycles) required to run the accelerated tests for estimating acceleration factor. This research presents a novel approach in determining the acceleration factor based on available data from field measurements in different climatic conditions and uses it for determining the acceleration factor for accelerated damp heat (DH) testing of PV modules for 1000 hours (DH1000). Utilizing the available meteorological and degradation data, each field condition is considered as test environment with varying stress levels and a simple linear model is used for predicting the allowed degradation threshold of DH1000 for field equivalent 25 years. Finally, the results are validated from the known qualification test data and the acceleration factor plot for different regions around the world is presented.

<p align="center">
<img src="https://github.com/arunbalas/ALT-of-PV-Modules/blob/master/Image/AF_DH1000.png" width="400" height="450">

<img src="https://github.com/arunbalas/ALT-of-PV-Modules/blob/master/Image/Deg%20Threshold.png" width="400" height="450">
</p>

Note that the model is built based on the limited degradation information from two sites in Arizona and New York. Hence the number of factors included in the model is one (temperature). I am still statistically calibrating this model to account for the unknown (hidden) factor effects so that the model accuracy can be improved. The results will be updated soon!
