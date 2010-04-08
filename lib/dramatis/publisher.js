"use strict";
Dramatis.Publisher = new Dramatis.Class([
  Puck.Publisher
], {
  invoke_: function invoke_(object, method, new_state) {
    var c = Dramatis.Continue;
    if (method in object || !("send" in object)) {
      Puck.Publisher.prototype.invoke_.apply(this, arguments);
    } else {
      // console.debug("sending "+object+" "+method);
      Array.prototype.push.call(
        new_state,
        c({
          exception: [this, function(e) {
            if (e instanceof Dramatis.Exception.NoSuchActor) {
              this.remove_subscription([object, method]);
            }
          }]
        }));
      object.send(method, new_state);
    }
  }
}, {
  name: "Publisher"
} );
