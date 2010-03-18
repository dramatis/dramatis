"use strict";
Dramatis.Subscriber = new Dramatis.Class({
  sub_name_: function() {
    return this.__dramatis__ && this.__dramatis__.name && this.__dramatis__.name() || this;
  }
}, [ Puck.Subscriber ], {name:"Subscriber"});
