(function(){
  var dataPromise = $.Deferred();
  var sliderPromise = $.Deferred();
  var pagePromise = $.Deferred();
  
  var yeardata = {};
  var paths = {};
  
  var interval = setInterval(function(){
    var checkEle = $('#footer')
    if (checkEle.length > 0) {
      clearInterval(interval);
      pagePromise.resolve();
    }
  }, 25);
  
  $.get("data/year-data.json", function(data) {
    yeardata = data;
    dataPromise.resolve();
  });
  
  $.when(dataPromise).then(function(){
    var i = 1;
    for (var group in yeardata) {
      paths[group] = {};
      paths[group]["orig"] = $('#all-sites-' + i++).attr("d");
      paths[group]["split"] = paths[group]["orig"].split("M");
    }
  });
  
  window.vizlab = {}; // remove this
  vizlab.showyear = function(year) {
    year = "" + year; // force year to be string
    var filterFunc = function(val, i){return (indices.indexOf(i) != -1)}
    var i = 1;
    for (var group in yeardata) {
      if (paths.hasOwnProperty(group)) {
        var indices = yeardata[group][year];
        var newpath = paths[group]["split"].filter(filterFunc);
        newpath = "M" + newpath.join("M");
        $('#all-sites-' + i++).attr("d", newpath);
      }
    }
  }
})();