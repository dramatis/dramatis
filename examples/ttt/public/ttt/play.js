(function($){
 
 var debug = this.debug || ( console && console.debug );

  $(function(){

    var on_filler = function on_filler(event) {
      var input = $(event.target).closest(".value").children("input");
      input.val($.trim($(event.target).find("span.url").text()));
      input.change();
      return false;
    };

    var on_input_change = function on_input_change(event) {
      var b = $(event.target).closest(".connection").data("behavior");
      var v = $(event.target).val();
      if ( v && !v.match(/^\s*$/) ) {
        b.enable_connect();
      } else {
        b.disable_connect();
      }
      return false;
    };

    var Control = function Control(element){
      if(element){
        var self = this;
        var methods = [ "connect", "disconnect" ];
        for(var m in methods) {
          var name = methods[m];
          var method = this[name];
          this[name] = (function(name,method){
            return function name() {
              method.apply(self,arguments);
            };
          })(name,method);
        }
        this.element = element;
        this.element.data("behavior",this);
        this.input = element.find("input");
        this.input.change(on_input_change).change();
      }
    };

    Control.prototype = {
      enable: function enable(){
        this.enable_changes();
        this.input.change();
        return;
        this.disable_changes();
        this.element.find(".value a").removeAttr("href","#").unbind("click");
        this.element.find(".control a").
          removeAttr("href","#").
          unbind("click");
      },
      disable: function disable(){
        this.disable_changes();
        this.element.find(".value a").removeAttr("href","#").unbind("click");
        this.element.find(".control a").
          removeAttr("href","#").
          unbind("click");
      },
      enable_connect: function enable_connect(){
        this.element.find(".control a").
          attr("href","#").
          bind("click",this.connect);
      },
      disable_connect: function disable_connect(){
        this.element.find(".control a").
          removeAttr("href").
          unbind("click");
      },
      enable_changes: function enable_changes(){
        this.input.removeAttr("disabled");
        this.element.find(".value a").attr("href","#").click(on_filler);
        this.element.find(".control a").
          text(this.enabled_text).
          unbind("click").
          click(this.connect);
      },
      disable_changes: function disable_changes(){
        this.input.attr("disabled",true);
        this.element.find(".value a").removeAttr("href").unbind("click");
        this.element.find(".control a").
          text(this.disabled_text).
          unbind("click").
          click(this.disconnect);
      },
      connect: function connect(){ this.disable_changes(); },
      disconnect: function disconnect(){ this.enable_changes(); }
    };

    var Server = function Server() {
      this.enabled_text = "Join";
      this.disabled_text = "Leave";
      Control.apply(this,arguments);
      this.disable_changes();
    };
    Server.prototype = new Control;
    Server.prototype.constructor = Server;
    $.extend( Server.prototype, {
      leave: function leave() {
        this.element.find(".control a").text(this.enabled_text);
      }
    });
    new Server($("#server"));

    var Connection = function Connection() {
      this.enabled_text = "Connect";
      this.disabled_text = "Disconnect";
      Control.apply(this,arguments);
      this.enable_changes();
      var server = $("#server").data("behavior");
      server.disable();
      server.leave();
    };
    Connection.prototype = new Control;
    Connection.prototype.constructor = Connection;
    $.extend( Connection.prototype, {
      connect: function connect() {
        Control.prototype.connect.apply(this,arguments);
        this.element.find(".status").text("Connecting ...");
        var self = this;
        Dramatis.connect(this.input.val(), function connected() {
          self.element.find(".status").text("Connected");
        }, function( reason ) {
          if(reason+"" == "0"){
            reason = "same domain violation";
          }
          self.disconnect();
          self.element.find(".status").text("Connection failed: " + reason);
        });
        var server = $("#server").data("behavior");
        server.enable();
      },
      disconnect: function disconnect() {
        Control.prototype.disconnect.apply(this,arguments);
        this.element.find(".status").text("");
        Dramatis.disconnect(this.input.val());
        var server = $("#server").data("behavior");
        server.disable();
        server.leave();
      }
    });
    new Connection($("#client"));

  });

})(jQuery);