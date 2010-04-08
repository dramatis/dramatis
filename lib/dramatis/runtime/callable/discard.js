"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  Callable.Discard =
    new Dramatis.Runtime.Callable.Class(function Discard() {
    }, [Callable], {
      call: function call(/*...*/) {
        Dramatis.error("call/discarding "+arguments[0]);
      },
      apply: function(args) {
        Dramatis.error("apply/discarding "+args[0]);
      },
      raise: function raise(exception) {
        Dramatis.error("raise/discarding "+exception);
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

  Dramatis.Discard = new Callable.Discard();
}());
