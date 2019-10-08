# This an example wrapper script to help run the various tools required in the BDC workflow
# may benefit from adding R executable paths for Hydrology sections here too...


import Dataset_Prep
import SplitLinesGeoPand
import BDC_Terrain_Processing
import BDC_tab_GEoPand
import Veg_FIS
import iHyd
import Comb_FIS
import os
import arcpy
from datetime import datetime

def main():
    startTime = datetime.now()
    print(startTime)
    
    # Define your project variable paths...
    
    rivers_root = os.path.abspath("C:/...")

    dem_path = os.path.abspath("D:/...")

    bvi_etc_root = os.path.abspath("D:/...")

    operCatch = os.path.abspath("C:/...")

    cehHydArea = os.path.abspath("C:/...")

    os_gridPath = os.path.abspath("C:/...")

    outRoot = os.path.abspath("D:/...")

    epsg_code = str(27700)

    print("running data prep script to organise inputs for all target Dam Capacity Areas/ Catchments")
    Dataset_Prep.BDC_setup_main(rivers_root, dem_path, bvi_etc_root, operCatch, os_gridPath, epsg_code, outRoot) # should add failsfe for this bit.

    direc_list = next(os.walk(outRoot))[1]

    for dir in direc_list:
        if os.path.isfile(os.path.join(outRoot, dir, "BDC_OC{0}/Output_BDC_OC{0}.shp".format(dir[-4:]))):
            print("Operational Catchment {0} already completed - pass".format(dir[-4:]))
        else:
            ocNum = dir[-4:]
            print("running BDC pipeline for Operational Catchment {0}".format(ocNum))
            home = os.path.join(outRoot, dir)
            raw_lines = os.path.join(home, "OC{0}_MM_rivers.shp".format(ocNum))


            split_lines = os.path.join(home, "BDC_reaches.shp")

            if os.path.isfile(split_lines):
                print("working reaches already exist, skip split lines")
            else:
                print("running line splitting tool")
                SplitLinesGeoPand.main(home, raw_lines)

            split_lines = os.path.join(home, "BDC_reaches.shp")

            DEM_path = os.path.join(home, "OC{0}_DTM.tif".format(ocNum))  # Below commented out for testing split lines
            in_waterArea = os.path.join(home, "OC{0}_OS_InWater.shp".format(ocNum))
            BVI_raster = os.path.join(home, "OC{0}_BVI.tif".format(ocNum))

            gdb_name = "scratch_OC{0}.gdb".format(ocNum)
            scratch_gdb = os.path.join(home, gdb_name)
            if arcpy.Exists(scratch_gdb):
                print("scratch gdb exists")
            else:
                # os.mkdir(scratch_gdb)
                arcpy.CreateFileGDB_management(home, gdb_name)
            #
            print("runnning BDC data extraction script")
            DEM_burn = os.path.join(home, "BDC_OC{0}strBurndDEm.tif".format(ocNum))
            DrAreaRas = os.path.join(home, "DrainArea_sqkm.tif")
            spltLinesP2 = os.path.join(scratch_gdb + "/seg_network_b")

            paramList = [DEM_burn, DrAreaRas, spltLinesP2]
            for p in paramList:
                if arcpy.Exists(p) is False:
                    print()
                    BDC_Terrain_Processing.main(home, scratch_gdb, split_lines, DEM_path)
                else:
                    print('{0} aready exitst! woop'.format(p))

            BDC_tab_GEoPand.main(home, spltLinesP2, DEM_burn, in_waterArea, BVI_raster, DrAreaRas) # current

            os.path.join(home, "BDC_OC{0}".format(ocNum), "Output_BDC_OC{0}.shp".format(ocNum))
            bdc_net = os.path.join(home, "BDC_OC{0}".format(ocNum), "Output_BDC_OC{0}.shp".format(ocNum))
            print(bdc_net)
            print("running Vegetation Fuzzy Inference System")
            Veg_FIS.main(bdc_net, scratch_gdb)

            opCatchArea = os.path.join(home, "OC{0}_catchmentArea.shp".format(ocNum))
            print("running Hydrological Fuzzy Inference System")
            iHyd.main(bdc_net, scratch_gdb, cehHydArea, opCatchArea)

            print("running Combined Fuzzy Inference System")
            Comb_FIS.main(bdc_net, scratch_gdb)


    finTime = datetime.now() - startTime
    print("Master Script Completed. \n"
          "Processing time = {0}".format(finTime))

if __name__ == '__main__':
    main()
