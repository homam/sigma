'use strict';
const React = require('react')
const ReactDOM = require("react-dom");
const Main = require("./Main.purs")

ReactDOM.render(
  // Main.helloComponent({})(["duck"]),
  React.createElement(Main.hello("Bye "), { name: 'homam' }),
  document.getElementById("example")
);

// Main.main();

if (module.hot) {
  module.hot.accept();
}
