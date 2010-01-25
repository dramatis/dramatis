/*jslint evil: true*/
"use strict";
include = (function () { return this; } () ).include || jasmine.include;

(function ($) {

  var print = ( this.Envjs && this.print ) || ( this.console && this.console.debug ) || function(){};

  var construct_path = function( source, result ) {
    var levels = 0;
    while( source.slice( source.length - 3 ) === "/.." ) {
      levels++;
      source = source.slice( 0, source.length - 3 );
    }
    var prefixes = [];
    while(levels-->0) {
      var slash = source.lastIndexOf("/");
      if (slash<0) {
        return "";
      }
      prefixes.unshift(source.slice(slash+1));
      source = source.slice(0,slash);
    }
    return result+prefixes.join("/");
  };

  var spec_filename = function() {
    var window = this;
    var path = window.location.toString();
    var q = path.lastIndexOf("?");
    if ( q >= 0 ) {
      path = path.slice(0,q);
      // print(path);
    }
    var slash = path.lastIndexOf("/");
    if ( slash < 0 ) {
      return path;
    }
    path = path.slice(slash+1);
    var dot = path.lastIndexOf(".");
    if ( dot > 0 ) {
      path = path.slice(0,dot);
    }
    return path;
  };

  var find_specs = function() {
    var path = window.location.toString();
    var pound = path.lastIndexOf("#");
    if ( pound > 0 ) {
      path = path.slice(0,pound);
    }
    var q = path.lastIndexOf("?");
    if ( q > 0 ) {
      path = path.slice(0,q);
    }
    var slash = path.lastIndexOf("/");
    if ( slash < 0 ) {
      return "";
    }
    var filename = path.slice(slash+1);
    if (filename === "spec_runner.html"){
      return "";
    }
    path = path.slice(0,slash);
    path = construct_path( path + "/" + jazrb_root, jazrb_root + "/spec/" );
    return [ path, spec_filename() ];
  };

  var load_file = function(filename) {
    var contents;
    $.ajax({ url: filename,
             dataType: "text/javascript",
             async: false,
             error: function( xhr ) {
               if(xhr.status !== 404) {
                 throw "error on load_file xhr for " + filename + ": " + xhr.status;
               }
             },
             success: function(data) { contents = data; } });
    if ( contents ) {
      return contents;
    }
    return undefined;
  };

  var load_first = function( fn, endings ) {
    for (var i=0; i<endings.length; i++) {
      var filename = fn + endings[i] + ".js";
      if ( window.Envjs ) {
        var root = window.location.toString();
        var slash = root.lastIndexOf("/");
        root = root.slice(0,slash+1);
        root = root.replace(/^file:\/\//,"");
        window.load(root + filename);
        return;
      } else {
        var contents = load_file( filename );
        if ( contents ) {
          try {
            eval(contents);
          } catch(e) {
            print("could not eval spec: ",e);
          }
	  return;
        }
      }
    }
  };

  var load_specs = function(specs) {
    var path = specs[0], filename = specs[1];
    var endings = [ "_spec", "Spec" ];
    // Guess ending ... FIX: allow spec'ing of default order ...
    if (filename.lastIndexOf("_") >= 0) {
      endings = [ "_spec", "Spec" ];
    } else if (filename.match(/[A-Z]/)) {
      endings = [ "Spec", "_spec" ];
    }
    load_first( path+"/"+filename, endings );
  };

  var run_specs = function(){
    if(!this.jazrb_root) {
      return;
    }
    var specs = find_specs();
    if (specs) {
      load_specs(specs);
    }
  };

  run_specs();

  if(this.jasmine && !this.Envjs) {
    var embedded = window.location+"" !== "about:blank";

    var jasmineEnv = jasmine.getEnv();
    jasmineEnv.updateInterval = 1000;

    if(embedded) {
      var trivialReporter = new jasmine.TrivialReporter();
      jasmineEnv.addReporter(trivialReporter);
      jasmineEnv.specFilter = function(spec) {
        return trivialReporter.specFilter(spec);
      };
    }

    if(window.Envjs && jasmine.EnvjsReporter){
      jasmineEnv.addReporter(new jasmine.EnvjsReporter());
    }

    $(function() {
      jasmineEnv.execute();
    });
  }

}(jQuery));