(function(){

  var Publisher = Dramatis.Publisher =
    new Dramatis.Class( function Publisher(){
    }, {
      add_subscription: function add_subscription( callback, options ) {
        options = options || {};
        this.subscriptions = this.subscriptions || []; 
        this.subscriptions.push( callback );
        if(options.initial && this.update){
          this.update();
        }
      },
      notify: function notify(state) {
        var subs = this.subscriptions;
        for(var i in subs) {
          subs[i].call(state);
        }
      }
    });

  new Dramatis.Class.Subscope(Publisher);

})();