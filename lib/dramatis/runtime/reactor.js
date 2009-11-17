(function(){
  var Runtime = Dramatis.Runtime;
  var Reactor = Runtime.Reactor =
    new Dramatis.Runtime.Class(function Reactor(){
    },{

      connect: function(url,good,bad) {
        new Reactor.Channel( url, function() {
          good && good();
        }, function() {
          bad && bad();
        } );
      },

      disconnect: function() {},
      destroy: function() {}
    });
  new Dramatis.Class.Subscope(Reactor);
})();
