(function(){
  var dataPromise = $.Deferred();
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
  
  // also make sure page is fully loaded
  $.when(dataPromise).then(function(){
    for (var group in yeardata) {
      paths[group] = {};
      paths[group]["orig"] = $('#' + group).attr("d");
      paths[group]["split"] = paths[group]["orig"].split("M");
    }
  });
  
  window.vizlab = {}; // remove this
  vizlab.showyear = function(year) {
    year = "" + year; // force year to be string
    var filterFunc = function(val, i){return (indices.indexOf(i) != -1)}
    for (var group in yeardata) {
      if (paths.hasOwnProperty(group)) {
        var indices = yeardata[group][year];
        var newpath = paths[group]["split"].filter(filterFunc);
        newpath = "M" + newpath.join("M");
        $('#' + group).attr("d", newpath);
      }
    }
  }
})();