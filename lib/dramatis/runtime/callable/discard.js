"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  Callable.Discard =
    new Dramatis.Runtime.Callable.Class(function Discard() {
    }, [Callable], {
      call: function call(/*...*/) {
      },
      apply: function(args) {
      },
      raise: function raise(exception) {
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+""
          ]};
      }
    });

  Callable.Discard.fromJSON = function() {
    return Dramatis.Discard;
  };

  Dramatis.Discard = new Callable.Discard;
}());
