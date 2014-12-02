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
      
      //fill body
      var doc = document.createElement('html');
      doc.innerHTML = this.responseText;
      document.body.innerHTML = doc
        .getElementsByTagName("body")[0].innerHTML ;
      
      //fill head
      var headElms = doc.getElementsByTagName("head")[0].children;
      for (var i=0; i<headElms.length; i++) {
        //debugger;
        var elm = headElms[i];
        if (elm.tagName.toLowerCase() == 'script' && elm.getAttribute('src')) {
          var newscript = document.createElement('script');
          newscript.src = dir + '/' + elm.getAttribute('src');
          newscript.type = 'text/javascript';
          newscript.innerHTML = elm.innerHTML;
          elm = newscript;
        }
        document.head.appendChild(elm.cloneNode(true));
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


// ************************************************
// *************** Private functions *************** 
// ************************************************
function rewrite(text, dir) {
}
