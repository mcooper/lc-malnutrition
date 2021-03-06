import ee
import pandas as pd

out = pd.read_csv("G:\\My Drive\DHS Processed\sp_export.csv")

out = out[['code', 'latitude', 'longitude', 'interview_year']].drop_duplicates()

ee.Initialize()

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
for year in out.interview_year.unique():    
    if year < 1992:
        y = 1992
    elif year > 2015:
        y = 2015
    else:
        y = year
    
    print(year)
    
    cci = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-" + str(y) + "-v207")
    
    sel = out.loc[out['interview_year'] == year]
    
    points = []
    for row in sel.iterrows():
        if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
            geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(15000)
            feat = ee.Feature(geom, {'code':row[1]['code']})
            points.append(feat)
    
    features = []
    i = 0
    while i < len(points):
        j = i + 3000
        fc = ee.FeatureCollection(points[i:j])
        features.append(fc)
        i = j
    
    ccir = map(lambda(x): cci.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), features)
    
    for f in ccir:
        for i in f['features']:
            temp = pd.DataFrame(merge_dicts(rename_dict('cci_', i['properties']['histogram']),
                                            {'code': i['properties']['code']},
                                            {'interview_year': year}), index = [0])
            cciaccum = cciaccum.append(temp)

cciaccum = cciaccum.fillna(0)

cciaccum.to_csv("G:\\My Drive\DHS Processed\landcover.csv", index=False)

