// Wait till the browser is ready to render the game (avoids glitches)
console.log("application.js");
window.requestAnimationFrame(function () {
  //window.gamemanager = new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager);
  
  //var DummyKeyboardManager = function() {
  //}
  //DummyKeyboardManager.prototype.on = function() {};

  window.gamemanager = new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager);
  
  (function hackCss() {
    $('#next-tile-container').css({
      position:"absolute",
      left:100,
      top:400,
      background: "#faf8ef",
      color: "#776e65",
    });


  })()
});
