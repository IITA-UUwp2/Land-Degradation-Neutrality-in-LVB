// var studyArea = 

// WorldPop population, 2000-2020
var worldpop = ee.ImageCollection('WorldPop/GP/100m/pop')
  .filter(ee.Filter.eq('country', 'KEN'));

var populationYears = ee.List.sequence(2000, 2020);

var populationStack = ee.Image(
  populationYears.iterate(function(year, image) {

    year = ee.Number(year);

    var population = ee.Image(
      worldpop
        .filter(ee.Filter.eq('year', year))
        .first()
    )
      .select('population')
      .rename(ee.String('Population_').cat(year.format('%d')))
      .clipToCollection(studyArea);

    return ee.Image(image).addBands(population);

  }, ee.Image(0).select([]))
);

Map.centerObject(studyArea, 8);

Map.addLayer(
  populationStack.select('Population_2020'),
  {min: 0, max: 100, palette: ['ffffff', 'ffff00', 'ff8800', 'ff0000']},
  'WorldPop 2020'
);

Export.image.toDrive({
  image: populationStack,
  description: 'LVB_WorldPop_2000_2020',
  folder: 'WorldPop_LVB',
  fileNamePrefix: 'LVB_WorldPop_2000_2020',
  region: studyArea.geometry(),
  scale: 100,
  maxPixels: 1e13
});


// Livestock headcount, 2000-2022
var livestock = ee.ImageCollection(
  'projects/global-pasture-watch/assets/gld-1km/v1/livestock-headcount-faostat_m'
);

var livestockYears = ee.List.sequence(2000, 2022);

var species = [
  'cattle',
  'goat',
  'sheep',
  'buffalo',
  'horse'
];

var livestockStack = ee.Image(
  livestockYears.iterate(function(year, image) {

    year = ee.Number(year);

    var annual = ee.Image(
      livestock
        .filterDate(
          ee.Date.fromYMD(year, 1, 1),
          ee.Date.fromYMD(year.add(1), 1, 1)
        )
        .first()
    );

    var annualBands = annual
      .select(species)
      .rename(
        ee.List(species).map(function(s) {
          return ee.String(s)
            .cat('_')
            .cat(year.format('%d'));
        })
      )
      .clipToCollection(studyArea);

    return ee.Image(image).addBands(annualBands);

  }, ee.Image(0).select([]))
);

Map.addLayer(
  livestockStack.select('cattle_2022'),
  {min: 0, max: 100},
  'Cattle 2022'
);

Export.image.toDrive({
  image: livestockStack,
  description: 'LVB_GPW_livestock_2000_2022',
  folder: 'GPW_Livestock',
  fileNamePrefix: 'LVB_GPW_livestock_2000_2022',
  region: studyArea.geometry(),
  scale: 1000,
  maxPixels: 1e13
});


// Distance to nearest road using GRIP4
var roadsAfrica = ee.FeatureCollection(
  'projects/sat-io/open-datasets/GRIP4/Africa'
);

// Include nearby roads outside the basin boundary
var roads = roadsAfrica.filterBounds(
  basin.geometry().buffer(200000)
);

var roadDistance = roads
  .distance(200000, 100)
  .divide(1000)
  .clip(basin)
  .rename('distance_to_road_km');

Map.addLayer(
  roads.filterBounds(basin),
  {color: 'black'},
  'GRIP4 roads'
);

Map.addLayer(
  roadDistance,
  {
    min: 0,
    max: 20,
    palette: ['d73027', 'fdae61', 'ffffbf', 'a6d96a', '1a9850']
  },
  'Distance to nearest road (km)'
);

Export.image.toDrive({
  image: roadDistance,
  description: 'LVB_distance_to_nearest_road_GRIP4',
  folder: 'roadDistance',
  fileNamePrefix: 'LVB_distance_to_nearest_road_GRIP4',
  region: basin.geometry(),
  scale: 1000,
  maxPixels: 1e13,
  fileFormat: 'GeoTIFF'
});


// Market accessibility, 2015
var marketAccess = ee.Image(
  'projects/malariaatlasproject/assets/accessibility/' +
  'accessibility_to_cities/2015_v1_0'
).select('accessibility');

var marketAccessBasin = marketAccess
  .clip(basin)
  .rename('travel_time_to_city_min');

Map.addLayer(
  marketAccessBasin,
  {
    min: 0,
    max: 600,
    palette: [
      '006837', '31a354', '78c679',
      'c2e699', 'ffffcc', 'fdae61',
      'd7191c'
    ]
  },
  'Travel time to nearest city (minutes)'
);

Export.image.toDrive({
  image: marketAccessBasin,
  description: 'LVB_market_accessibility_2015',
  folder: 'MarketAccessibility',
  fileNamePrefix: 'LVB_market_accessibility_2015',
  region: basin.geometry(),
  scale: 1000,
  maxPixels: 1e13,
  fileFormat: 'GeoTIFF'
});