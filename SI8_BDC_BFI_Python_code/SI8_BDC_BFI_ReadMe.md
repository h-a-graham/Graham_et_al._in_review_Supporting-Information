## SI8 Beaver Foraging Index (BFI) and Beaver Dam Capacity (BDC) Model Code.

BFI model production:

Firstly many of the scripts reference two differnet acronyms - BVI and BHI.

BVI is equivalent to the BFI reported in the manuscript. BHI is the same dataset clipped to a desired (100m) distance of any fresh water.

By running the BFI/BVI/BHI workflow, two datasets are produced. the BFI or BVI is what is required to be processed by the BDC.
Although, as long as the buffer is >40m then it doesn't matter as this is the maximum search area used by the BDc model to derive 
environmental information.

Use the wrapper named master as an example of how to automate the workflow.

BDC model production:

Be sure to edit the relevant R executable and R.libs objects in the CEH hydrology script. R is used here because of the excellent RNRFA
which is a great package that allows us to access the CEH national river flow archive api with ease.

Again there is a BDC_master.py script which gives an example workflow that can be used to run the required BDC tools.


Additional tools folder:

These are just a bunch of tools that I made throughout the process to make life a little easier.
For example many of the toold for BDC require esri shapefile inputs so there is a GeoJson to shp converter.
See if there is anything of use there but none of it is essential to the modelling workflow.


