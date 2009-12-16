(function($){

  var Server = TTT.Server;

  var View = Server.View = new Server.Class(function View(server,dom){
    this.server = server;
    this.dom = dom;
    $(this.dom).html("");
  });

})(jQuery);