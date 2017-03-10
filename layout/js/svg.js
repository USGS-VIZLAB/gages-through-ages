var vizlab = vizlab || {};

(function() {
  "use strict";
  
  /*
   * Provides operations on the svg element parameter
   * @param {SVGSVGElement} svg
   * 
   */
  vizlab.svg = function(svg) {
    
    var cursorPoint = function(screenX, screenY) {
      var point = svg.createSVGPoint();
      point = point.matrixTransform(svg.getScreenCTM().inverse());
      point.x = Math.round(point.x);
      point.y = Math.round(point.y);
      return point;
    }
    
    var TOOLTIP_HTML = 
      '<defs>' + 
      '<clipPath id="tipClip-test">' +
      '<rect x="-6" y="-11.5" height="11" width="12"/>' +
      '</clipPath>' + 
      '</defs>' + 
      '<rect id="tooltip-box-test" height="24" class="tooltip-box"/>' +
      '<path id="tooltip-point-test" d="M-6,-12 l6,10 l6,-10" class="tooltip-box" clip-path="url(#tipClip-test)"/>' +
      '<text id="tooltip-text-test" dy="-1.1em" text-anchor="middle" class="svg-text"> </text>';
    
    var addTooltip = function() {
      var tooltipGroup = document.createElement("g");
      tooltipGroup.id = "tooltip-group-test";
      tooltipGroup.innerHTML = TOOLTIP_HTML;
      svg.appendChild(tooltipGroup);
    };
    /*
     * @param {Number} - DOM x coordinate where tooltip should be rendered
     * @param {Number} - DOM y coordinate where tooltip should be rendered
     * @param {String or Function} tooltipText - Returns text to appear in tooltip box. If tooltipText is a function,
          the function parameters will be evt, options.
     */
    var showTooltip = function(x, y, tooltipText) {
      var tooltip = document.getElementById("tooltip-text-test");
      var tooltipBox = document.getElementById("tooltip-box-test");
      var toolPoint = document.getElementById("tooltip-point-test");
      var text = (typeof tooltipText === "function") ? tooltipText(options) : tooltipText;
      var svgPoint = cursorPoint(x, y);
      var svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
      var textLength;
      var halfLength;
      var tooltipX
      
      tooltip.firstChild.data = text;
      textLength = Math.round(tooltip.getComputedTextLength());
      halfLength = textLength / 2;
      
      /* Make sure tooltip text is within the SVG */
      if (svgPoint.x - halfLength - 6 < 0)  {
        tooltipX = halfLength + 6;
      }
      else if (svgPoint.x + halfLength + 6 > svgWidth) {
        tooltipX = svgWidth - halfLength - 6;
      } 
      else {
        tooltipX = svgPoint.x;
      }
      tooltip.setAttribute("x", tooltipX);
      tooltip.setAttribute("y", svgPoint.y);
      tooltip.setAttribute("class", "shown");
      
      /* Set attributes for background box */
      tooltipBox.setAttribute("x", tooltipX - halfLength - 6);
      tooltipBox.setAttribute("y", svgPoint - 35);
      tooltipBox.setAttribute("width", textLength + 12);
      tooltipBox.setAttribute("class", "tooltip-box");
      
      /* Set attributes for the tooltip point */
      tooltipPoint.setAttribute("transform", "translate(" + svgPoint.x + "," + svgPoint.y + ")");
      tooltipPoint.setAttribute("class", "tooltip-box");
    };
    
    var hideTooltip = function() {
      var tooltip = document.getElementById("tooltip-text-test");
      var tooltipBox = document.getElementById("tooltip-box-test");
      var toolPoint = document.getElementById("tooltip-point-test");
      
      tooltip.firstChild.data = " ";
      tooltipBox.setAttribute("class", "hidden");
      tooltipPoint.setAttribute("class", "hidden");
    };
  
    return {
      addTooltip: addTooltip,
      showTooltip : showTooltip,
      hideTooltip : hideTooltip
    };
  }
})();
