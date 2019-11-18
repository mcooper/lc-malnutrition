import ee
import pandas as pd
import time

data = pd.read_csv("~/mortalityblob/dhs/sp_export.csv")

data = data[['code', 'latitude', 'longitude', 'interview_year']].drop_duplicates()

ee.Initialize()

buffersize = 15000

gl00 = ee.ImageCollection('users/mwcoopr/globeland30_2000').mosaic()
gl10 = ee.ImageCollection('users/mwcoopr/globeland30_2010').mosaic()

# def rename_dict(suf, d):
#     for i in d.keys():
#         if i == '10':
#             cov = 'Agriculture'
#         elif i == '20':
#             cov = 'Forest'
#         elif i == '30':
#             cov = 'Grassland'
#         elif i == '40':
#             cov = 'Shrubland'
#         elif i == '50':
#             cov = 'Wetland'
#         elif i == '60':
#             cov = 'Water'
#         elif i == '70':
#             cov = 'Tundra'
#         elif i == '80':
#             cov = 'Artificial'
#         elif i == '90':
#             cov = 'Bare'
#         elif i == '100':
#             cov = 'Ice'
#         elif i == '255':
#             cov = 'Ocean'
#         else:
#             cov = i
#         d[cov + "_" + suf] = d.pop(i)
#     return(d)

def tryReduceRegions(raster, feature):
    try:
        reduction = raster.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=feature, scale=30).getInfo()
    except ee.ee_exception.EEException:
        print('Memory Exceeded, waiting')
        time.sleep(60*5)
        reduction = tryReduceRegions(raster, feature)
    return reduction

all = []
for y in ['2000', '2010']:
  print("****************************\nNow Running Year " + y + "\n****************************")
  
  if y == '2000':
      lc = gl00
  elif y == '2010':
      lc = gl10
  else:
      raise Exception('Dude where\'s my car?')
  
  print("Make buffered points")
  points = []
  for row in data.iterrows():
      if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
          geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(buffersize)
          feat = ee.Feature(geom, {'code':row[1]['code'], 'interview_year': row[1]['interview_year']})
          points.append(feat)
  
  print("Make features")
  feat = []
  i = 0
  while i < len(points):
      j = i + 50
      fc = ee.FeatureCollection(points[i:j])
      feat.append(fc)
      i = j
  
  print("Reduce regions")
  for f in feat:
    reduction = tryReduceRegions(lc, f)
    for r in reduction['features']:
        temp = r['properties']['histogram']
        temp['code'] = r['properties']['code']
        temp['year'] = y
        all.append(temp)
    print(str(feat.index(f)) + " of " + str(len(feat)))
  
  time.sleep(60)

final = pd.DataFrame(all)

final = final.fillna(0)

final.to_csv("~/mortalityblob/dhs/globeland30_15km.csv", index=False)

import os
os.system('~/telegram.sh "Done with globeland"')

