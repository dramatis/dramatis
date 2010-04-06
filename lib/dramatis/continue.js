"use strict";
(function(){
  var global = (function(){return this;}());
  Dramatis.Continue =
    new Dramatis.Class(function Continue(/*...*/){
      if (this === global) {
        return new Continue(Continue, arguments);
      }
      var args = arguments;
      if (args[0] === Continue) {
        args = args[1];
      }
      this.continuation = new Dramatis.Continuation(this, args);
      return this; // keep emacs happy
    });

  Dramatis.CallCC =
    new Dramatis.Class(function CallCC(){
    });

}());