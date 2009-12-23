(function(){

  var Subscriber = Dramatis.Subscriber =
    new Dramatis.Class( function Subscriber(){
    }, {

      subscribe: function subscribe( options ) {
debug("t",options.to);
        options.to.add_subscription(this, options.call);
      }

    });

  new Dramatis.Class.Subscope(Subscriber);

})();