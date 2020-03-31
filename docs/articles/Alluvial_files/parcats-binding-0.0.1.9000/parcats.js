
HTMLWidgets.widget({
  name: "parcats",
  type: "output",

  initialize: function(el, width, height) {
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.autosize) {
      var width = instance.width || width;
      var height = instance.height || height;
      Plotly.relayout(el.id, {width: width, height: height});
    }
  },  
  
  renderValue: function(el, x, instance) {
    
    var data = [];
    var layout = [];
    var myPlot = document.getElementById(el.id);
    
    // x.traces is passed as an Object with named elements
    // but needs to be converted to array with unnamed elements
    for( var key of Object.keys(x.traces)){
      data.push( x.traces[key] );
    }
    
      
    layout = x.layout;
    
    var shapes = [];
    
    for( var key of Object.keys(x.shapes)){
      shapes.push( x.shapes[key] );
    }
    
    var shapes_original = [];
    
    for( var key of Object.keys(x.shapes_original)){
      shapes_original.push( x.shapes_original[key] );
    }
    
    layout['shapes'] = shapes;

    Plotly.plot(el.id, data, layout, {responsive: true});
    
    myPlot.on('plotly_hover', function(data){
      
      // this is the proper way too deepcopy object in JS
      // before I was passing two versions of the same object
      shapes = JSON.parse(JSON.stringify(shapes_original));
      
      var curve = data.points[0].curveNumber;
      
      // curve == 0, is the parcats trace
      if(  curve == 0 ){
      
        for ( var i_data in data.points){
        
          var point = data.points[i_data].pointNumber;
      
          for(var i in x.map_curve[point] ){
            
            if( x.map_type[point][i] == 'scatter'){
            
              Plotly.restyle(el.id, {fillcolor : x.parcats_cols[point]
                                      // changing of the marker color takes too long
                                      //, line : { color : x.parcats_cols[point] }
                                      //, marker : { color : x.parcats_cols[point] }
                                      }
                             , parseInt(x.map_curve[point][i]) );
            }
            
            if( x.map_type[point][i] == 'bar'){
            
              Plotly.restyle(el.id, { marker : { color : x.parcats_cols[point] }
                                      }
                             , parseInt(x.map_curve[point][i]) );
            }

            if( x.map_type[point][i] == 'line'){
              
              tr = x.map_curve[point][i];
              
              shape_index = x.map_trace_2_shape[tr -1];
              
              shapes[shape_index -1 ]['line']['color'] = x.parcats_cols[point];
            
            }

          }
        
        }
      }
      
      var t1 = curve
      , t2 = data.points[0].pointNumber
      , t3 = x.map_type[t2]
      , t4 = x.map_curve[t2]
      ;
      
      // use first title option for debugging
      var title = t1 + '_' + t2 + '_' + t3 + '_' + t4 + '_' + x.map_trace_2_shape[12]+ '_' + x.map_trace_2_shape[15];
      var title = '';

      if(curve == 0){
        Plotly.relayout(el.id, { shapes : shapes
                                , title : title} );
      }
    });
    
    myPlot.on('plotly_unhover', function(data){
      
      var curve = data.points[0].curveNumber;

      if( curve == 0){
      
        for( var i_data in data.points){
        
          var point = data.points[i_data].pointNumber;
    
          for(var i in x.map_curve[point] ){
            
            if( x.map_type[point][i] == 'scatter'){
            
              Plotly.restyle(el.id, {fillcolor :  x.map_color[point][i]
                                      //, line : { color :  x.hist_cols[x.map[point][i] - 1] }
                                      //, marker : {  color :  x.hist_cols[x.map[point][i] - 1] }
                                      }
                                      , parseInt(x.map_curve[point][i]) );
            }
            
            if( x.map_type[point][i] == 'bar'){
            
              Plotly.restyle(el.id, { marker : {  color :  x.map_color[point][i] }
                                      }
                                      , parseInt(x.map_curve[point][i]) );
            }
            
          }
        
        }
      }

      Plotly.relayout(el.id, { shapes : shapes_original
                                , title : ''} );
    });

  }
});