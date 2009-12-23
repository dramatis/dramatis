(function(){

  var Method = Dramatis.Runtime.Callable.Method;

  var Continuation = Dramatis.Continuation =
    new Dramatis.Class( function Continuation(){
      if(arguments.length == 2) {
        /* new Continuation(object, method_name) */
        this.value = new Method( arguments[0], arguments[1] );
      } else {
        throw new Error("cannot create continuation from object");
      }
    }, {
      call: function call() {
        this.value.apply( arguments );
      }
    });

  new Dramatis.Class.Subscope(Continuation);

})();