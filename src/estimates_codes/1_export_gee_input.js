// Load biome and state images from MapBiomas
var biomes = ee.Image('projects/mapbiomas-workspace/AUXILIAR/biomas-raster-41');
var states = ee.Image('projects/mapbiomas-workspace/AUXILIAR/estados-2016-raster');

// Dictionary mapping numerical codes to biome names
var bioDict = {1:'Amazon', 2:'Atlantic Forest', 3:'Pantanal', 4:'Cerrado', 5:'Caatinga', 6:'Pampa'};

// List of years considered in the analysis
var anos = ['1985', '1986', '1987','1988', '1989', '1990','1991', '1992', '1993','1994', '1995', '1996',
'1997', '1998', '1999','2000', '2001', '2002','2003', '2004', '2005','2006', '2007', '2008','2009', '2010',
'2011','2012', '2013', '2014','2015', '2016', '2017', '2018','2019','2020','2021','2022','2023'];

// Classes to be excluded from the analysis
var excludedClasses = [
    "NÃO OBSERVADO",
    "ERRO",
    "DESMATAMENTO",
    'REGENERAÇÃO',
    'NÃO CONSOLIDADO',
    'Não consolidado',
];

// Dictionary mapping land use and land cover classes to numerical values
var classes = ee.Dictionary({
  'AFLORAMENTO ROCHOSO':29,
  "APICUM": 32,
  "AQUICULTURA": 31,
  "CAMPO ALAGADO E ÁREA PANTANOSA": 11,
  "LAVOURA TEMPORÁRIA": 19,
  "LAVOURA PERENE": 36,
  "CANA": 20,
  "FLORESTA PLANTADA": 9,
  "FORMAÇÃO CAMPESTRE": 12,
  "FORMAÇÃO FLORESTAL": 3,
  'FORMAÇ��O FLORESTAL':3,
  "FORMAÇÃO SAVÂNICA": 4,
  "INFRAESTRUTURA URBANA": 24,
  "MANGUE": 5,
  "MINERAÇÃO": 30,
  "NÃO OBSERVADO": 0,
  "OUTRA FORMAÇÃO NÃO FLORESTAL": 13,
  "OUTRA ÁREA NÃO VEGETADA": 25,
  "PASTAGEM": 15,
  "PRAIA E DUNA": 23,
  'RESTINGA HERBÁCEA':50,
  "RIO, LAGO E OCEANO": 33,
  'VEGETAÇÃO URBANA': 24,
  'FLORESTA INUNDÁVEL':3,
});

// Define Google Earth Engine assets for samples and MapBiomas data
var assetSamples = 'projects/mapbiomas-workspace/VALIDACAO/mapbiomas_85k_col4_points_w_edge_and_edited_v1';
var assetMapBiomas = 'projects/mapbiomas-public/assets/brazil/lulc/collection9/mapbiomas_collection90_integration_v1';

// Folder where exported files will be saved
var folder = 'ACC_COL8_v9_6_no_EDGE';

// Loop over years to process data
for (var Year in anos) {
    var year = anos[Year];
    var ano = anos[Year];

    // Load samples as a FeatureCollection
    var samples = ee.FeatureCollection(assetSamples);

    // Get unique values from CARTA_2 and DECLIVIDAD columns
    var cartas_unique = samples.aggregate_histogram('CARTA_2').keys();
    var declividade_strats = samples.aggregate_histogram('DECLIVIDAD').keys();

    // Compute total stratified distribution for each map sheet
    var carta_stratsize_total = ee.Dictionary(cartas_unique.iterate(function(carta, cartas_remade) {
        return ee.Dictionary(cartas_remade).set(carta, samples.filter(ee.Filter.eq('CARTA_2', carta)).aggregate_histogram('DECLIVIDAD'));
    }, ee.Dictionary()));

    // Filter samples, excluding unwanted classes
    samples = samples.filter(ee.Filter.inList('CLASS_' + ano, excludedClasses).not())
        .map(function(feature) {
            return feature.set('year', year)
                .set('reference', classes.get(feature.get('CLASS_' + ano)));
        })
        .filter(ee.Filter.neq('BORDA_' + ano, 1));

    // Compute filtered stratified distribution
    var carta_stratsize_filtered = ee.Dictionary(cartas_unique.iterate(function(carta, cartas_remade) {
        return ee.Dictionary(cartas_remade).set(carta, samples.filter(ee.Filter.eq('CARTA_2', carta)).aggregate_histogram('DECLIVIDAD'));
    }, ee.Dictionary()));

    print(samples.first()); // Display the first sample
    print(samples.size());  // Display the total number of samples

    // Compute sampling weights and other metrics for each feature
    samples = samples.map(function(feat) {
        feat = ee.Feature(feat);
        var carta = feat.get('CARTA_2');
        var strat = feat.get('DECLIVIDAD');
        var amos_weight = ee.Number.parse(ee.String(feat.get('PESO_AMOS')).replace(',', '.'));
        var amos_prob = ee.Number(1).divide(amos_weight);
        var vote_count = ee.Number.parse(feat.get(ee.String('COUNT_').cat(ano)));
        var strat_total_size = ee.Number.parse(ee.Dictionary(carta_stratsize_total.get(carta)).get(strat));
        var strat_filtered_size = ee.Number(ee.Dictionary(carta_stratsize_filtered.get(carta)).get(strat));
        var new_prob = amos_prob.multiply(strat_filtered_size.divide(strat_total_size));
        var new_weight = amos_weight.multiply(strat_filtered_size.divide(strat_total_size));

        var vote_weight = ee.Algorithms.If(vote_count.eq(1), 1,
            ee.Algorithms.If(vote_count.eq(2), 0.5,
                ee.Algorithms.If(vote_count.eq(3), ee.Number(1).divide(3), 1)
            )
        );

        var value_peso = ee.Number.parse(vote_weight);
        var peso_voto = amos_prob.multiply(value_peso);

        return feat.set({
            'PROB_AMOS2': amos_prob,
            'NEW_PROB': new_prob,
            'NEW_WEIGHT': new_weight,
            'PESO_VOT': peso_voto,
            'VAL_PESO': value_peso,
            'COUNT': vote_count
        });
    });

    // Load MapBiomas classification
    var classification = ee.Image(assetMapBiomas);
    var mapbiomas = classification.select('classification_' + year).rename('classification')
        .addBands([states.rename('StateNB'), biomes.rename('BioNB')]);

    // Perform region sampling
    var result = mapbiomas.sampleRegions({
        collection: samples,
        properties: ['CLASS_' + ano, 'reference', 'year', 'BIOMA', 'CARTA_2', 'DECLIVIDAD', 'TARGETID', 'LON', 'LAT', 'PROB_AMOS', 'PROB_AMOS2', 'NEW_WEIGHT', 'NEW_PROB', 'PESO_VOT', 'VAL_PESO', 'VOTOS'],
        scale: 30,
        geometries: false
    });

    // Export data to Google Drive
    Export.table.toDrive({
        collection: result,
        description: 'acc_mapbiomas_' + year,
        folder: folder,
        fileFormat: 'csv'
    });
}
