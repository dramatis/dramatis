/*globals include,jazrb_root*/
"use strict";
if(!(function(){return this;}()).Dramatis){
  include(jazrb_root + '/spec/lib/spec_helper.js');
  include(jazrb_root + '/lib/dramatis.js');
  include(jazrb_root + '/lib/dramatis/class.js');
  include(jazrb_root + '/lib/dramatis/runtime.js');
  include(jazrb_root + '/lib/dramatis/runtime/callable.js');
  include(jazrb_root + '/lib/dramatis/runtime/callable/method.js');
  include(jazrb_root + '/lib/dramatis/continuation.js');
  include(jazrb_root + '/lib/dramatis/actor.js');
  include(jazrb_root + '/lib/dramatis/actor/interface.js');
  include(jazrb_root + '/lib/dramatis/actor/behavior.js');
  include(jazrb_root + '/lib/dramatis/actor/name.js');
  include(jazrb_root + '/lib/dramatis/actor/name/type.js');
  include(jazrb_root + '/lib/dramatis/actor/type.js');
  include(jazrb_root + '/lib/dramatis/director.js');
  include(jazrb_root + '/lib/dramatis/subscriber.js');
  include(jazrb_root + '/lib/dramatis/publisher.js');
  include(jazrb_root + '/lib/dramatis/runtime/reactor.js');
  include(jazrb_root + '/lib/dramatis/runtime/reactor/channel.js');
  include(jazrb_root + '/lib/dramatis/runtime/reactor/channel/xmpp.js');
  include(jazrb_root + '/lib/dramatis/future.js');
  include(jazrb_root + '/vendor/strophejs/strophe.js');
  include(jazrb_root + '/vendor/strophejs/contrib/mock/strophe.js');
}