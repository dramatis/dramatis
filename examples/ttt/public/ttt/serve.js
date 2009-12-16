(function($){

  $(function(){
    $("#server").data( "view", new TTT.Server.View( new TTT.Server,
                                                    $("#server")[0] ) );
  });

})(jQuery);