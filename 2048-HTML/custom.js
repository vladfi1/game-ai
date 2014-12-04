/**
 * Interface into js from haskell, put all haskell inline functions here
 */
console.log("loaded custom.js");
function loadThrees(dir) {
  console.log("loadThrees() " + dir);
  var indexDotHtml = dir + "/index.html";
  var xhr= new XMLHttpRequest();
  xhr.open('GET', indexDotHtml, true);
  xhr.onreadystatechange= function() {
      if (this.readyState!==4) return;
      if (this.status!==200) return; // or whatever error handling you want
      
      var doc = document.createElement('html');
      doc.innerHTML = this.responseText;
      var links  = doc.getElementsByTagName("link");
      var scripts = doc.getElementsByTagName("script");
      for (var i = links.length-1; i >= 0; i --) {
      }
      for (var i = scripts.length-1; i >= 0; i--) {
      }

      //fill body
      document.body.innerHTML = doc
        .getElementsByTagName("body")[0].innerHTML;

      for (var i=0; links.length > 0;) {
        //debugger;
        var link = links[i];
        link.parentNode.removeChild(link);
        var newlink = document.createElement('link');
        newlink.href = dir + '/' + link.getAttribute('href');
        newlink.type = link.type;
        newlink.rel = link.rel;
        newlink.innerHTML = link.innerHTML;
        document.head.appendChild(newlink.cloneNode(true));
      }

      
      //fill javascript
      for (var i=0; scripts.length > 0;) {
        var elm = scripts[i];
        elm.parentNode.removeChild(elm);
        var newscript = document.createElement('script');
        newscript.src = dir + '/' + elm.getAttribute('src');
        newscript.type = 'text/javascript';
        newscript.innerHTML = elm.innerHTML;
        document.head.appendChild(newscript.cloneNode(true));
      }

  };
  xhr.send();
};

function pressDown() {
  $(window).trigger({ type : 'keydown', which : 40 , keyCode: 40 })
}


function writeGameState(arrstring) {
  debugger;
  console.log('got new gamestate\n' + arrstring);
  //var arr = arrstring.trim().split('\n').map(function(line) {
  //  console.log('line ' + line)
  //  var withoutparens = line.replace('(', '').replace(')', '').trim();
  //  console.log("map returns " + withoutparens.split(" "));
  //  return withoutparens.split(" ");
  //});
  //(function transpose(arr) {
  //  var tmp;
  //  for (var x=0; x< arr.length; x++) {
  //    for (var y = x + 1; y < arr[x].length; y ++) {
  //      console.log('x y ' + x + ' ' + y);
  //      console.log('arr[x] ' + arr[x]) ;
  //      console.log('arr[y] ' + arr[y]) ;
  //      tmp = arr[x][y];
  //      arr[x][y] = arr[y][x];
  //      arr[y][x] = tmp;
  //    }
  //  }
  //})(arr);
  //console.log(arr);
  //window.gamemanager.makeBoardFromState(arr);
}

window.loadThrees = loadThrees;
window.pressDown = pressDown;
window.writeGameState = writeGameState;
onLoad(); //defined in trampoline code

