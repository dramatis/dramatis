(function(){

  var Publisher = Dramatis.Publisher =
    new Dramatis.Class( function Publisher(){
    }, {
      add_subscription: function add_subscription( callback ) {
        this.subscriptions = this.subscriptions || []; 
        
      },
      notify: function publish(state) {
        var subs = this.subscriptions;
        for(var i in subs) {
          subs[i](state);
        }
      }
    });

  new Dramatis.Class.Subscope(Publisher);

})();