"use strict";
Dramatis.Publisher = new Dramatis.Class([
  Puck.Publisher
], {
  invoke_: function invoke_(object, method, new_state) {
    if (method in object || !("send" in object)) {
      Puck.Publisher.prototype.invoke_.apply(this, arguments);
    } else {
      object.send(method, [new_state]);
    }
  }
}, {
  name: "Publisher"
} );
