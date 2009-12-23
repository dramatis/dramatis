(function(){
  var Callable = Dramatis.Runtime.Callable;
  var Method = Callable.Method =
    new Dramatis.Runtime.Callable.Class(function Method( object, method ){
      this.object = object;
      this.method = method;
    }, {
      apply: function apply( args ) {
        var method = this.object[this.method];
        return method.apply( this.object, args );
      }
    } );
  new Dramatis.Class.Subscope(Method);

})();
