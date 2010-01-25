/*jslint evil: true*/
"use strict";
jazrb_root = (function(){return this;}()).jazrb_root || "..";
if(!(function(){return this;}()).jasmine){
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/base.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/util.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Env.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Reporter.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Block.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/JsApiReporter.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Matchers.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/mock-timeout.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/MultiReporter.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/NestedResults.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/PrettyPrinter.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Queue.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Reporters.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Runner.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Spec.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/Suite.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/WaitsBlock.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/src/WaitsForBlock.js'></script>");
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/lib/TrivialReporter.js'></script>");
  if((function(){return this;}()).Envjs){
    document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jazrb/vendor/jasmine/lib/EnvjsReporter.js'></script>");
  }
  (function(){
    var headID = document.getElementsByTagName("head")[0];         
    var cssNode = document.createElement('link');
    cssNode.type = 'text/css';
    cssNode.rel = 'stylesheet';
    cssNode.href = jazrb_root + "/vendor/jazrb/vendor/jasmine/lib/jasmine.css";
    cssNode.media = 'screen';
    headID.appendChild(cssNode);
  }());
}
if(!(function(){return this;}()).jQuery){
  document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jquery/dist/jquery.js'></script>");
}
document.write("<script type='text/javascript' src='" + jazrb_root + "/vendor/jquery.print/dist/jquery.print.js'></script>");
document.write("<script type='text/javascript' src='" + jazrb_root + "/spec/spec_runner.js'></script>");
