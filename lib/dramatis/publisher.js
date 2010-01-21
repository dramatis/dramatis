"use strict";

(function () {

  /*global Dramatis*/

  var Publisher = Dramatis.Publisher =
    new Dramatis.Class(function Publisher() {
    }, {
      add_subscription: function add_subscription(callback, options) {
        options = options || {};
        this.subscriptions = this.subscriptions || []; 
        this.subscriptions.push(callback);
        if (options.initial && this.update) {
          this.update();
        }
      },
      notify: function notify(state) {
        var i, subs = this.subscriptions;
        for (i in subs) {
          subs[i].call(state);
        }
      }
    });

  (new Dramatis.Class.Subscope(Publisher));

}());