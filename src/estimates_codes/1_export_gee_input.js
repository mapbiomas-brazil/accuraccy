// GEE Link: https://code.earthengine.google.com/b0f70f9d9891efefdc287ece45cb2a91
 
var biomes = ee.Image('projects/mapbiomas-workspace/AUXILIAR/biomas-raster-41')
var bioDict = {1:'Amazônia', 2:'Mata Atlântica', 3:'Pantanal', 4:'Cerrado', 5:'Caatinga', 6:'Pampa'}

var anos = ['1985', '1986', '1987','1988', '1989', '1990','1991', '1992', '1993','1994', '1995', '1996','1997', '1998', '1999','2000', '2001', '2002','2003', '2004', '2005','2006', '2007', '2008','2009', '2010', '2011','2012', '2013', '2014','2015', '2016', '2017', '2018']

var excludedClasses = [
    "Não Observado",
    "Erro",
    "Desmatamento",
    'Regeneração',
    'Não consolidado',
];

var classes = ee.Dictionary({
  "Afloramento Rochoso": 29,
  "Apicum": 32,
  "Aquicultura": 31,
  "Cultura Anual": 19,
  "Cultura Perene": 36,
  "Cultura Semi-Perene": 19,
  "Floresta Plantada": 9,
  "Formação Campestre": 12,
  "Formação Florestal": 3,
  "Formação Savânica": 4,
  "Infraestrutura Urbana": 24,
  "Mangue": 5,
  "Mineração": 30,
  "Mosaico De Ocupação": 21,
  "Não Observado": 0,
  'Não observado': 0,
  "Não consolidado": 0,
  "Outra Formação Natural Não Florestal": 13,
  "Outra Área Não Vegetada": 25,
  "Outra Área não Vegetada": 25,
  "Pastagem Cultivada": 15,
  "Praia e Duna": 23,
  "Rio, Lago e Oceano": 33,
  "Área Úmida Natural Não Florestal": 11,
  "Área Úmida Natural não Florestal": 11,
});

var assetSamples = 'users/vieiramesquita/MAPBIOMAS/mapbiomas_100k_all_points_w_edge_and_edited_v7';
var assetMapBiomas = 'projects/mapbiomas-workspace/COLECAO5/mapbiomas-collection50-integration-v8'
var folder = 'ACC_COL5_v8_no_EDGE'

for (var Year in anos){
  var year = anos[Year];
  var ano = anos[Year];
  
  var samples = ee.FeatureCollection(assetSamples)
  
  print(ano)
  print(samples.aggregate_histogram('CLASS_' + ano))
  
  var cartas_unique = samples.aggregate_histogram('CARTA').keys()
  var declividade_strats = samples.aggregate_histogram('DECLIVIDAD').keys()
  
  var carta_stratsize_total = ee.Dictionary(cartas_unique.iterate(function(carta,cartas_remade){
    return ee.Dictionary(cartas_remade).set(carta,samples.filter(ee.Filter.eq('CARTA',carta)).aggregate_histogram('DECLIVIDAD'))
  },ee.Dictionary()))
  
  if (year == '2019'){
    ano = '2018'
  }
  
  samples = samples.filter(ee.Filter.inList('CLASS_' + ano, excludedClasses).not())
                   .map(function (feature) {
                        return feature.set('year', year)
                                      .set('reference', classes.get(feature.get('CLASS_' + ano)));
                   })
                   .filter(ee.Filter.neq('BORDA_' + ano,'TRUE'))
                   //.filter(ee.Filter.eq('POINTEDITE','FALSE'))
  
  var carta_stratsize_filtered = ee.Dictionary(cartas_unique.iterate(function(carta,cartas_remade){
    return ee.Dictionary(cartas_remade).set(carta,samples.filter(ee.Filter.eq('CARTA',carta)).aggregate_histogram('DECLIVIDAD'))
  },ee.Dictionary()))
  
  samples = samples.map(function(feat){
    
    feat = ee.Feature(feat)
    
    var carta = feat.get('CARTA')
    var strat = feat.get('DECLIVIDAD')
    var amos_prob = ee.Number(feat.get('PROB_AMOS'))

    var vote_count = ee.Number(feat.get(ee.String('COUNT_').cat(ano)))
  
    var strat_total_size = ee.Number(ee.Dictionary(carta_stratsize_total.get(carta)).get(strat))
    var strat_filtered_size = ee.Number(ee.Dictionary(carta_stratsize_filtered.get(carta)).get(strat))
    var new_prob = ee.Number(amos_prob.multiply(strat_filtered_size.divide(strat_total_size)))
    
    var tot_matrix = ee.Algorithms.If(ee.String(feat.get('POINTEDITE')).match('FALSE'), 6, 8)
    var tot_votes = ee.Algorithms.If(ee.String(feat.get('POINTEDITE')).match('FALSE'), 3, 4)
    
    var vote_weight = ee.Algorithms.If(vote_count.eq(1), 1,
      ee.Algorithms.If(vote_count.eq(2), 0.5,
        ee.Algorithms.If(vote_count.eq(3), ee.Number(1).divide(3), 1)
      )
    )
    
    var value_peso = ee.Number(vote_weight)
    
    var peso_voto = ee.Number(amos_prob).multiply(ee.Number(vote_weight))//((vote_count.multiply(tot_votes)).subtract(tot_votes)).divide(tot_matrix)
    
    return feat.set({'NEW_PROB':new_prob,'PESO_VOT':peso_voto, 'VAL_PESO':value_peso, 'COUNT':vote_count})
    
  })
  
  var mapbiomas = ee.Image(ee.ImageCollection(assetMapBiomas).mosaic()).select('classification_'+year).rename('classification').addBands(biomes.rename('BioNB'))
  
  var result = mapbiomas
                    .sampleRegions({
                          collection: samples, 
                          properties: ['CLASS_' + ano,'reference','year','BIOMA','CARTA','TARGET_FID','POINTEDITE', 'LON', 'LAT','PROB_AMOS','AMOSTRAS','REINSP','NEW_PROB','PESO_VOT','VAL_PESO','COUNT'], 
                          scale: 30, 
                          geometries: false
                    })
  Export.table.toDrive({
    collection:result, 
    description:'acc_mapbiomas_' + year, 
    folder: folder,
    fileFormat: 'csv'
  })
  
}
