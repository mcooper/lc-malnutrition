var imgCol2000 = ee.ImageCollection('users/mwcoopr/globeland30_2000')
var imgCol2010 = ee.ImageCollection('users/mwcoopr/globeland30_2010')

// Define a palette for the 18 distinct land cover classes.
var from = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 255]
var to = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

var img2000 = imgCol2000.mosaic().remap(from, to)
var img2010 = imgCol2010.mosaic().remap(from, to)


var palette = [
              "FFFF64", //Ag
              "006400", //Forest
              "FFB432", //Grassland
              "966400", //Shrubland
              "00785A", //Wetland
              "0046C8", //Water
              "FFDCD2", //Tundra
              "C31400", //Artificial
              "FFF5D7", //Bare
              "FFFFFF", //Ice
              "0046C8"  //Ocean
              ]


Map.addLayer(img2010, {palette: palette})
Map.addLayer(img2000, {palette: palette})


