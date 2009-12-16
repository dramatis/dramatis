(function($){

  var Board = TTT.Board = function(ci,dom){
    this.ci = ci;
    this.bind( "notification" );
    this.ci && this.ci.notify(this.notification);
    $("<div id='svg'></div>").appendTo(dom||"body");
    var paper = Raphael($("#svg")[0],320,200);
    var circle = paper.circle(50,40,10);
  };

  Board.toString = (function(){
    var string = TTT.toString() + ".Board";
    return function(){return string;};
  })();

  Board.prototype.bind = function(/* varargs */) {
    var self = this;
    for(var i=0; i < arguments.length; i++) {
      var name = arguments[i];
      var fn = this[name];
      this[name] = function( /* arargs */ ) { fn.apply(self,arguments); };
    }
  };

  Board.prototype.notification = function() {
    throw "implement";
  };

})(jQuery);

