
# Notes: THis works pretty well at the moment. Still requires arcpy due to weird quirks with splitting lines at points
# and importing large vector datasets with geopandas...

import os
import geopandas as gpd
import pandas as pd
from shapely.geometry import Point, MultiLineString, LineString
# from shapely.ops import split, snap
# from shapely import wkt
import math
# import matplotlib.pyplot as plt
import arcpy
import shutil
from pathlib import Path
import glob
import sys

# pd.set_option('display.max_rows', 500)
# pd.set_option('display.max_columns', 500)
# pd.set_option('display.width', 1000)

arcpy.env.overwriteOutput = True


def main(home, rivers):
    scratch = os.path.join(home, "scratch")
    if os.path.isdir(scratch):
        print("scratch folder exists")
        try:
            shutil.rmtree(scratch)
        except Exception as e:
            print(e)
        os.mkdir(scratch)
    else:
        print("creating scratch folder")
        os.mkdir(scratch)


    EvalLineSize(rivers, scratch, home)


def EvalLineSize(rivers_shp, scratchy, homey):
    print("checking size of input lines data")
    rivers_gpd = gpd.read_file(rivers_shp, driver="ESRI Shapefile")
    crs = rivers_gpd.crs
    # nrows = len(rivers_gpd)
    del rivers_gpd


    result = arcpy.GetCount_management(rivers_shp)
    nrows = int(result[0])
    limit = 5000

    n = 0


    if nrows > limit:
        print("large dataset chunking up")
        list_of_chunks = chunking(nrows, limit, scratchy, rivers_shp)

        # outShpList = []

        for chunk in list_of_chunks:
            p = os.path.abspath(Path(chunk).parents[0])
            print(p)
            print(chunk)
            SpitFunction(chunk, p, p, n)

            # outShpList.append(out_shp)

        file_search = os.path.join(scratchy, "chunk_*", "BDC_reaches.shp")
        outShpList = glob.glob(file_search, recursive=True)


        shp_save = os.path.join(homey, "BDC_reaches.shp")

        print("merging glob")
        vecmap_gp = pd.concat([
            gpd.read_file(shp)
            for shp in outShpList
        ], sort=True).pipe(gpd.GeoDataFrame)
        vecmap_gp.crs = crs
        vecmap_gp.to_file(shp_save, driver="ESRI Shapefile")

        if os.path.isdir(scratchy):
            try:
                print("not deleting scratch files for debug")
                shutil.rmtree(scratchy)
            except Exception as e:
                print(e)
        print("script finished")

    else:
        print("small dataset no need to chunk")

        print("begin split function")

        SpitFunction(rivers_shp, scratchy, homey, n)

        if os.path.isdir(scratchy):
            try:
                shutil.rmtree(scratchy)
            except Exception as e:
                print(e)

        print("script finished")

def chunking(n_feat, lim, homeloc, inFc1):

    print("large river network submitted ({0} rows) - run chunked workflow".format(n_feat))
    newChunklist = []
    n_chunks = round(n_feat/lim)
    chunk_size = round(n_feat/n_chunks)

    chunk_ranges = []
    chunk_list = range(1, n_chunks + 1)
    start = [0, int(chunk_size)]
    for i in chunk_list:
        # print(i)
        if i == 1:
            values = start
        # elif i > 1 and i < n_chunks:
        elif n_chunks > i > 1:
            values = [(int(chunk_size) * (i - 1)) + 1, int(chunk_size) * i]
        else:
            values = [(int(chunk_size) * (i - 1)) + 1, int(n_feat)]
        chunk_ranges.append(values)

    print(chunk_ranges)

    counter = 0
    for i in chunk_ranges:
        arcpy.Delete_management("in_memory")
        counter += 1
        # print("Running line split tool for chunk {0}/{1}".format(counter, n_chunks))

        chunk_folder = os.path.join(homeloc, "chunk_{0}".format(counter))
        if os.path.isdir(chunk_folder):
            try:
                shutil.rmtree(chunk_folder)
            except Exception as e:
                print(e)
        os.makedirs(chunk_folder)

        s, e = i[0], i[1]


        cflayer = arcpy.MakeFeatureLayer_management(inFc1, "fl")

        arcpy.SelectLayerByAttribute_management(cflayer, "NEW_SELECTION",
                                                '"FID" >={0} AND "FID" <={1}'.format(s, e))

        inCFl = arcpy.MakeFeatureLayer_management(cflayer, r'in_memory/chunk{0}'.format(counter))


        inCFc = os.path.join(chunk_folder, "inFc.shp")
        arcpy.CopyFeatures_management(inCFl, inCFc)
        # river_gp_chunk.to_file(inCFc, driver="ESRI Shapefile")

        newChunklist.append(inCFc)



    return newChunklist


def SpitFunction (riv_shp, scratch, home, counter):
    dist = 200
    counter += 1
    print("iteration {0} of split function".format(counter))

    rivers_gpd = gpd.read_file(riv_shp, driver="ESRI Shapefile")
    print("shp file loaded")
    rivers_gpd["Llength"] = rivers_gpd.length

    long_riv_gpd = rivers_gpd.loc[rivers_gpd["Llength"] > dist]

    geom_list = []

    for index, row in long_riv_gpd.iterrows():
        shapelyLine = row['geometry']

        shpLen = shapelyLine.length

        nsplit = math.ceil(shpLen/dist)

        split_range = range(1, nsplit)

        for i in split_range:

            p_dist = 1/(i+1)

            splitPoint = (shapelyLine.interpolate(p_dist, normalized=True))
            x, y = splitPoint.coords.xy
            geom_list.append((float(x[0]), float(y[0])))

    gs_points = gpd.GeoSeries(Point(pnt[0], pnt[1]) for pnt in geom_list)
    gdf_points = gpd.GeoDataFrame()
    gdf_points['geometry'] = gs_points

    pnts_path = os.path.join(scratch, "SnipPoints.shp")
    gdf_points.to_file(pnts_path, driver="ESRI Shapefile")
    print("points file created")

    spltLines = os.path.join(scratch, "SplitLines_c{0}.shp".format(counter))
    print(str(counter))

    print("split line at point tool runing...")
    print(riv_shp)
    print(spltLines)
    arcpy.SplitLineAtPoint_management(riv_shp, pnts_path, spltLines, search_radius=1)
    print("slp tool completed.")
    Splitriv_gpd = gpd.read_file(spltLines, driver="ESRI Shapefile")

    print("checking line lengths...")
    if any(x > dist for x in list(Splitriv_gpd.length)) is True:
        SpitFunction(spltLines, scratch, home, counter)
    else:
        print("all line lengths less than {0}".format(dist))
        outfile = os.path.join(home, "BDC_reaches.shp")
        if arcpy.Exists(outfile):
            arcpy.Delete_management(outfile)
        arcpy.CopyFeatures_management(spltLines, outfile)

        print("chunk completed")



if __name__ == '__main__':
    # main(root, tarea, riverspath)
    main(sys.argv[1],
              sys.argv[2])
