/**
 * Interface into js from haskell, put all haskell inline functions here
 */
console.log("loaded custom.js");
function loadThrees(threesUri) {
  console.log("loadThrees() " + threesUri);
  var xhr= new XMLHttpRequest();
  xhr.open('GET', threesUri, true);
  xhr.onreadystatechange= function() {
        if (this.readyState!==4) return;
        if (this.status!==200) return; // or whatever error handling you want
        document.body.innerHTML= this.responseText;
        console.log(this.responseText);
  };
  xhr.send();
};

function pressDown() {
  $(window).trigger({ type : 'keydown', which : 40 , keyCode: 40 })
}

window.loadThrees = loadThrees;
window.pressDown = pressDown;

onLoad(); //defined in trampoline code
