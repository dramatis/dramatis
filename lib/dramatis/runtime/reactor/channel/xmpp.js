"use strict";
(function () {
  var Reactor = Dramatis.Runtime.Reactor;
  var Channel = Reactor.Channel;

  var bosh_parser = new RegExp("^bosh(s)?://([^:/]+)(:([^/]+))?(/(.+))?/([^@:/]+)(:([^@]+))(@([^:/@]+))?(/(.*))?$");

  var parse_bosh = function parse_bosh(xmpp, url) {
    var match = bosh_parser.exec(url);
    if(match){
      if(match[1]){
        throw new Error("implement https");
      }
      xmpp.service = "http://"+match[2];
      if(match[4]){
        xmpp.service += ":"+match[4];
      }
      if(match[6]){
        xmpp.service += "/"+match[6];
      }
      xmpp.jid = match[7] + "@";
      if(match[11]){
        xmpp.jid += match[11];
      } else {
        throw new Error("implement default vhost");
        // xmpp.jid += match[2];
      }
      if(match[12]){
        xmpp.jid += match[12];
      }
      xmpp.password = match[9];
    }
  };

  var global = (function(){return this;}());
  var debug = global.debug || 
               global.console &&
               global.console.log &&
               function(){global.console.log.apply(global.console,arguments);} || function(){};
  var parent_regex = new RegExp("/[^/]*$");

  var new_iframe = function new_iframe(fn) {
    var self = this;
    var iframe = global.document.createElement('iframe');
    var parent = this.service;
    parent = parent.replace( parent_regex, "" );
    parent = parent + "/pub/archive/index.html";
    iframe.setAttribute('src',parent);
    iframe.setAttribute('style',"display: none");
    iframe.onload = function(){ fn.call(self,iframe); };
    global.document.body.appendChild(iframe);
    /* this works around an issue with env.js which doesn't load the frame,
     * but also doesn't do same domain checking */
    if(!iframe.contentWindow){
      global.setTimeout(function(){fn.call(self,iframe);},0);
    } else {
      global.setTimeout(function(){fn.call(self,iframe);},1000);
    }
  };

  var statuses = [];
  (function(){
    for(var key in Strophe.Status) {
      statuses[Strophe.Status[key]]=key;
    }
  }());

  var XMPP;

  var strophe_callback = function strophe_callback(status, reason){
    var global = (function(){return this;}());
    if (!global.Envjs) {
      debug("SC",this.connected,statuses[status].toLowerCase(),global.$.print(reason));
    }
    if(status === Strophe.Status.CONNECTED && this._good){
      this.connected = true;
      this.connecting = false;

      this.jid = this.connection.jid;
      this.user = Strophe.getNodeFromJid(this.jid);
      this.host = Strophe.getDomainFromJid(this.jid);
      this.resource = Strophe.getResourceFromJid(this.jid);
      this.route = new XMPP.Route(this.user, this.host, this.resource);

      this.uri = function(){
        return this.connection.jid ? "xmpp:" + this.connection.jid : "";
      };
      this._good(this);
    } else if (status === Strophe.Status.DISCONNECTED && this.connected) {
      this._call_on_disconnects(reason && reason !== null && reason || this._disconnect_reason);
    } else if (status === Strophe.Status.CONNFAIL && this.connected) {
      this._disconnect_reason = reason;
    } else if (status === Strophe.Status.AUTHFAIL && this.connecting) {
      this.connecting = false;
      if (this._bad) { this._bad(reason); }
    } else if (status === Strophe.Status.CONNFAIL && this.connecting) {
      if (this.connection.abort) {
        this.connection.abort();
      }
      this.connection = new Strophe.Connection(this.service);
      if(global.location.href.indexOf("file://")!==0){
        global.document.domain = global.document.domain;
      }
      // debug(this.connecting, this.connecting_direct);
      if (false && this.connecting && this.connecting_direct) {
        new_iframe.call(this, function(iframe){
          // debug("nif");
          this.connecting_direct = false;
          var self = this;
          var xmpp_window = iframe.contentWindow;
          try {
            if (xmpp_window.document) {}
          } catch(e) {
            xmpp_window = global;
          }
          // debug(xmpp_window.document);
          var callback = function callback() {
            strophe_callback.apply( self, arguments );
          };
          var undef;
          this.connection.connect(this.jid, this.password, callback, undef, undef, xmpp_window);
        });
      } else {
        this.connecting = false;
        if (this._bad) {
            this._bad(reason);
        }
      }
    }
  };

  XMPP = Channel.XMPP =
    new Channel.Class(function XMPP(url, good, bad){
      parse_bosh(this,url);
      this._good = good;
      this._bad = bad;
      var self = this;
      var callback = function callback() {
        strophe_callback.apply( self, arguments );
      };
      this.connecting = this.connecting_direct = true;
      this.connection = new Strophe.Connection(this.service);

      this.connection.connect(this.jid, this.password, callback);

      this.receive = function() {
        return self.constructor.prototype.receive.apply(self,arguments);
      };

      this.connection.addHandler(this.receive, null, "iq", null, null, null);

      this.stanzas = {};
    }, [ Channel ], {
      receiveIQ: function(iq) {
        var type;
        var error;
        var text;
        switch ((type = iq.getAttribute("type"))) {
         case "error":
          // FIX: pretty order dependent ... not sure if that's required
          global.dom = iq;
          var id = iq.getAttribute("id");
          error = iq.childNodes[0];
          if (error.nodeName !== "error") {
            error = iq.childNodes[1];
          }
          text = error.childNodes[0].nodeName;
          var text_contents = error.childNodes[1] && error.childNodes[1].childNodes[0].data;
          if (text_contents) {
            global.console.debug("error details: "+text_contents);
          }
          error = new Dramatis.Exception.ServiceUnavailable(text);
          var continuation = this.stanzas[id];
          if (continuation && continuation.exception) {
            continuation.exception.call(error);
          } else {
            throw error;
          }
          break;

         case "set":
          try {
            var json = iq.childNodes[0].childNodes[0].data;
            // global.console.debug("json: "+json);
            var task = Dramatis.JSON.parse(json);
            // console.debug("rcv",task.id);
            try {
              Dramatis.Director.current.enqueue(task, {local_only: true});

              global.dom = iq;
              iq = Strophe.copyElement(iq);
              var to = iq.getAttribute("to");
              var from = iq.getAttribute("from");
              iq.setAttribute("to",from);
              iq.setAttribute("from",to);
              var doc = iq.ownerDocument;
              iq.setAttribute("type", "result");
              iq.removeChild(iq.childNodes[0]);
              try {
                this.connection.send(iq);
              } catch(e) {
                console.debug("discarding unsendable reply");
              }

            } catch(e) {
              if (task.continuation && task.continuation.exception) {
                // console.debug(22, $.print(task.continuation.exception));
                task.continuation.exception.call(e);
              } else {
                throw e;
              }
            }
          } catch(ex) {
            global.dom = iq;
            iq = Strophe.copyElement(iq);
            var to = iq.getAttribute("to");
            var from = iq.getAttribute("from");
            iq.setAttribute("to",from);
            iq.setAttribute("from",to);
            var doc = iq.ownerDocument;
            iq.setAttribute("type", "error");
            iq.removeChild(iq.childNodes[0]);
            error = doc.createElement("error");
            error.setAttribute("type","retry");            
            iq.appendChild(error);
            var bad = doc.createElement("bad-request");
            bad.setAttribute("xmlns","urn:ietf:params:xml:ns:xmpp-stanzas");
            text = doc.createElement("text");
            var data = doc.createTextNode(Strophe.xmlescape(ex+"\n"+ex.stack));
            text.appendChild(data);
            error.appendChild(bad);
            error.appendChild(text);
            this.connection.send(iq);
            this.connection.flush();
          }
          break;

        case "result":
          delete this.stanzas[iq.getAttribute("id")];
          break;

        default:
          throw new Error("implement iq type="+type);
        }
      },
      receive: function(stanza) {
        // console.debug("rcv", stanza.nodeName);
        switch(stanza.nodeName) {
         case "iq":
          this.receiveIQ(stanza);
          break;
         case "message":
          throw new Error("implement stanza type "+stanza.nodeName);
        default:
          throw new Error("implement stanza type "+stanza.nodeName);
        }
        return true;
      },
      send: function(route, data, continuation) {
        var json;
        try {
          // console.debug("send",data.id);
          json = JSON.stringify(data);
        } catch(e) {
          global.console.debug("error while trying to stringify a "+data.constructor+"");
          global.console.debug("value: "+global.$.print(data));
          throw e;
        }
        
        var to = (route+"").replace(/^xmpp:/,"");
        var stanza = this.stanza(to, continuation).
          c("task", {xmlns: "urn:dramatis"}).
          t(json).
          up();
        try {
          this.connection.send(stanza);
          this.connection.flush();
        } catch(e1) {
          global.console.debug(e1);
          try { throw new Error(); } catch(e0) {
            global.console.debug("nc",e0.stack);
          }
          throw e1;
        }
      },
      stanza: function(to, continuation) {
        var id = (Math.random()+"").substring(2);
        var stanza = global.$iq({to: to, type: 'set', id: id});
        // console.debug("set",id,$.print(continuation));
        this.stanzas[id] = continuation;
        return stanza;
      }
    });

  new Dramatis.Class.Subscope(XMPP);

  XMPP.Route = new XMPP.Class(function Route(user, host, resource){
      this.user = user;
      this.host = host;
      this.resource = resource;
    }, [ Channel.Route ], {
      uri: function(actor) {
        return this.toString()+"#"+actor;
      },
      toString: function() {
        return [ "xmpp:", this.user, "@", this.host,
          ( this.resource ? "/" + this.resource  : "" ) ].join("");
      }
    });

  XMPP.prototype.route = XMPP.Route;

  var url_parser = /^xmpp:([^@]+)?(@([a-zA-Z0-9\-]+))?(\/([^#]+))?(#(.*))?$/;
  XMPP.parse_url = function(string) {
    var m = string.match(url_parser);
    var result;
    if (m) {
      result = {
        route: new XMPP.Route(m[1], m[3], m[5]),
        id: m[7]
      };
      if (typeof result.id === "string") {
        var numerically = parseInt(result.id,10);
        if (!isNaN(numerically)) {
          result.id = numerically;
        }
      }
    }
    return result;
  };

  Reactor.register_parser(XMPP.parse_url);

}());
