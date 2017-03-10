var vizlab = vizlab || {};
var hovertext;

(function(){
  var dataPromise = $.Deferred();
  var pagePromise = $.Deferred();
  
  var yeardata = {};
  var paths = {};
  var mapSVG;
  
  var yearPointer = 0;
  var startYear = undefined;
  var numYears = undefined;
  var delay = 500; // ms
  var playInterval = undefined;
  
  var interval = setInterval(function(){
    var checkEle = $('#footer')
    if (checkEle.length > 0) {
      clearInterval(interval);
      pagePromise.resolve();
    }
  }, 25);
  
  $.get("data/year-data.json", function(data) {
    lsfilter = function(val, i) {
      return (ls.indexOf(val) == -1); // filter if missing from ls
    };
    for (var group in data) {
      var prevgages = [];
      yeardata[group] = {};
      if (undefined === numYears && undefined === startYear) {
        var keys = Object.keys(data[group])
        startYear = Number(keys[0]);
        numYears = keys.length;
      }
      for (var year in data[group]) {
        var gn = data[group][year]["gn"]
        var ls = data[group][year]["ls"]
        var newgages = prevgages.filter(lsfilter)
        newgages = newgages.concat(gn);
        yeardata[group][year] = newgages;
        prevgages = newgages;
      }
    }
    dataPromise.resolve();
  });
  
  // also make sure page is fully loaded
  $.when(dataPromise).then(function(){
    for (var group in yeardata) {
      paths[group] = {};
      paths[group]["orig"] = $('#' + group).attr("d");
      paths[group]["split"] = paths[group]["orig"].split("M");
    }
    
    play();
  });
  
  $(document).ready(function() {
    // Set up svg mouse over dom
    mapSVG = vizlab.svg(document.getElementById("map-svg"));
    mapSVG.addTooltip();
  });
  
  var play = function() {
    playInterval = setInterval(function(){
      var year = startYear + yearPointer;
      yearPointer = (yearPointer + 1) % numYears;
      vizlab.showyear(year);
    }, delay);
  };
  
  var pause = function(year) {
    clearInterval(playInterval);
    yearPointer = year - startYear;
    vizlab.showyear(year);
  };
  
  vizlab.showyear = function(year) {
    return;
    year = "" + year; // force year to be string
    var filterFunc = function(val, i){
      if (undefined !== indices) {
        return (indices.indexOf(i) != -1)
      } else {
        return false;
      }
    }
    for (var group in yeardata) {
      if (paths.hasOwnProperty(group)) {
        var indices = yeardata[group][year];
        var newpath = paths[group]["split"].filter(filterFunc);
        newpath = "M" + ((newpath.length > 0) ? newpath.join("M") : "0,0");
        $('#' + group).attr("d", newpath);
      }
    }
  }
  
  hovertext = function(text, event) {
    console.log('text is"' + text + '"');
    if (text) {
      var id = event.currentTarget.id;
      var year = id.slice(2);
      var tooltipText = text + ', year: ' + year;
      mapSVG.showTooltip(event.clientX, event.clientY, tooltipText);
    } else {
      mapSVG.hideTooltip();
    }
  }
})();