import ee
import pandas as pd
import numpy as np

ee.Initialize()

x = np.arange(-17.975, 52, 0.05).tolist()
y = np.arange(-34.975, 20, 0.05).tolist()

coords = np.array(np.meshgrid(x, y)).T.reshape(-1,2)

cci = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207")

points = []
for x, y in coords:
    geom = ee.Geometry.Point(x, y).buffer(15000)
    feat = ee.Feature(geom, {'x': x, 'y': y})
    points.append(feat)

features = []
i = 0
while i < len(points):
    j = i + 3000
    fc = ee.FeatureCollection(points[i:j])
    features.append(fc)
    i = j

ccir = map(lambda(x): cci.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), features)

def rename_dict(pref, d):
    for i in d.keys():
        d[pref + i] = d.pop(i)
    return(d)

def merge_dicts(*dicts):
    superdict = {}
    for d in dicts:
        for k, v in d.iteritems():
            superdict[k] = v
    return(superdict)

cciaccum = pd.DataFrame()
for f in ccir[:3]:
    for i in f['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('cci_', i['properties']['histogram']),
                                        {'x': i['properties']['x']},
                                        {'y': i['properties']['y']}), index = [0])
        cciaccum = cciaccum.append(temp)

cciaccum = cciaccum.fillna(0)

cciaccum.to_csv("G:\\My Drive\DHS Processed\landcover_grid.csv", index=False)

