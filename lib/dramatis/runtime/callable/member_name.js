"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  var MemberName = Callable.MemberName =
    new Dramatis.Runtime.Callable.Class(function MemberName(object, member_name){
      if(typeof object === "undefined" || typeof member_name === "undefined"){
        throw new Error("undefined");
      }
      this.object = object;
      this.member_name = member_name;
    }, [Dramatis.Runtime.Callable ], {
      call: function apply(args) {
        throw new Error("check/implement");
        var method = this.object[this.member_name];
        return method.apply(this.object, args);
      },
      apply: function apply(args) {
        throw new Error("check/implement");
        var method = this.object[this.member_name];
        return method.apply(this.object, args);
      }
    });
}());
