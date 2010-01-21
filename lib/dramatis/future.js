"use strict";
/*global Dramatis*/
(function(){
  var Future = Dramatis.Future =
    new Dramatis.Class(function Future(){
      this.__ready__ = false;
      this.__callbacks__ = [];
    }, {
      set: function set(v) {
        this.__value__ = v;
        this.__ready__ = true;
        var global = (function(){return this;}());
        if(this.__callbacks__){
          for(var i=0; i<this.__callbacks__.length;i++){
            this.__callbacks__[i].call(global,v);
          }
        }
      },
      ready: function ready() {
        return this.__ready__;
      },
      value: function value(fn) {
        if (fn) { fn(this.__value__); }
        return this.__value__;
      },
      subscribe: function subscribe(callback){
        if(this.__ready__){
          callback(this.__value__);
        } else {
          this.__callbacks__ = this.__callbacks__ || [];
          this.__callbacks__.push(callback);
        }
      }
    });

  Future.using = function using(/*..*/) {
    var args = arguments;
    var fn = Array.prototype.pop.call(args);
    var waiting = args.length;
    var fn_args = [];
    var global = (function(){return this;}());
    var callback = function(){
      fn.apply(global,fn_args);
    };
    for(var i=0; i < args.length; i++) {
      if(args[i].ready()){
        fn_args[i] = args[i].value();
        waiting--;
      } else {
        (function(){
          var index = i;
          args[i].subscribe(function(v){
            fn_args[index] = v;
            waiting--;
            if(waiting === 0){
              callback();
            }
          });
        }());
      }
    }
    if(waiting === 0){
      callback();
    }
  };

}());