var jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/spec_helper.js");
var ttt_root = this.ttt_root || ".";
include(ttt_root + "/spec/public/spec_helper.js");

(function($){

  describe("play",function(){

    it("should exist",function(){
      expect($("title:contains('Play')").size()).toBeGreaterThan(0);
    });

    it("should have a sign in control",function(){
      expect($("#client").size()).toBe(1);
    });

    it("should have an object associated with the sign in control",function(){
      expect($("#client").data("behavior")).toBeDefined();
    });

    it("should have a join control",function(){
      expect($("#server").size()).toBe(1);
    });

    it("should have an object associated with the server control",function(){
      expect($("#server").data("behavior")).toBeDefined();
    });

    it("should disable link until connect has a string",function(){
      $("#client .value input").val("").change();
      expect($("#client .control a").attr("href")).toBeUndefined();
    });

    it("should enable the link when it has a string",function(){
      $("#client .value a").eq(0).click();
      expect($("#client .control a").attr("href")).toBeDefined();
    });

    it("should connect on click",function(){
      $("#client .value a").eq(0).click();
      spyOn(Dramatis,"connect");
      $("#client .control a").click();
      expect(Dramatis.connect).wasCalledWith($("#client .value a .url").eq(0).text().trim(),
                                             jasmine.any(Function),
                                             jasmine.any(Function)
                                             );
      expect($("#client input").attr("disabled")).toBe(true);
      expect($("#client .control a").text()).toBe("Disconnect");
      $("#client .value a").each(function(){
        expect($(this).attr("href")).toBe(undefined);
      });
      $("#client .control a").click();
    });

    xit("should join on click",function(){
      $("#server .value a").eq(0).click();
      $("#server .control a").click();
    });

  });


})(jQuery);