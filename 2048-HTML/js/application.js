// Wait till the browser is ready to render the game (avoids glitches)
console.log("application.js");
window.requestAnimationFrame(function () {
  new GameManager(4, KeyboardInputManager, HTMLActuator, LocalStorageManager);
});
