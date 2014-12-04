// Wait till the browser is ready to render the game (avoids glitches)
console.log("application.js");
window.requestAnimationFrame(function () {
  window.gamemanager = new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager);
  
  (function hackCss() {
    //mega hack. Couldn't get itt to work in css, so i'm doing it here
    
    //currentWidth = $('#next-tile-container').css("width");
    //currentHeight = $('#next-tile-container').css("height");

    //$('#next-tile-container').css("width", currentWidth + 50);
    //$('#next-tile-container').css("height", currentHeight + 50);
    $('#next-tile-container').css({
      position:"absolute",
      left:100,
      top:100,
      background: "#faf8ef",
      color: "#776e65",
    });

    //tile  = new Tile(50, 10);

  })()
});
