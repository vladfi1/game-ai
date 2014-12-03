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

window.loadThrees = loadThrees;
window.pressDown = pressDown;

onLoad(); //defined in trampoline code

