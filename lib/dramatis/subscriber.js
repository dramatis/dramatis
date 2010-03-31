"use strict";
Dramatis.Subscriber = new Dramatis.Class([
  Puck.Subscriber
], {
  sub_name_: function() {
    return this.__dramatis__ && this.__dramatis__.name && this.__dramatis__.name() || this;
  }
}, {
  name:"Subscriber"
});
