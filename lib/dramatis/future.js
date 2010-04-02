"use strict";
/*global Dramatis*/
(function(){
  var global = (function(){return this;}());
  var Future = Dramatis.Future =
    new Dramatis.Class(function Future(){
      this.__ready__ = false;
      this.__callbacks__ = [];

      var self = this;
      this.value = {
        call: function(v) {
          self.__value__ = v;
          self.__ready__ = true;
          if (self.__callbacks__) {
            for(var i=0; i<self.__callbacks__.length;i++){
              self.__callbacks__[i].call(global,v);
            }
          }
        }
      };

    }, {
      ready: function ready() {
        return this.__ready__;
      },
      result: function result(fn) {
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
    var callback = function(){
      fn.apply(global,fn_args);
    };
    var closure = function(i) {
      var index = i;
      args[i].subscribe(function(v){
        fn_args[index] = v;
        waiting--;
        if(waiting === 0){
          callback();
        }
      });
    };
    for(var i=0; i < args.length; i++) {
      if(args[i].ready()){
        fn_args[i] = args[i].result();
        waiting--;
      } else {
        closure(i);
      }
    }
    if(waiting === 0){
      callback();
    }
  };

}());