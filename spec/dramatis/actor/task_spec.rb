require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/task'
require 'dramatis/actor'

describe Dramatis::Runtime::Task do

  include Dramatis

  after do
    begin
      Dramatis::Runtime.current.quiesce
      Dramatis::Runtime.current.exceptions.length.should equal( 0 )
      Thread.list.length.should equal( 1 )
    ensure
      Dramatis::Runtime.reset
    end
  end

  it "should return errors to calling actor even when non-rpc (non-main)" do
    callerClass = Class.new do
      include Dramatis::Actor
      def initalize
        @exception = nil
      end
      def caller callee
        release( callee ).callee
      end
      def dramatis_exception e
        @exception = e
      end
      def exception
        raise @exception if @exception
      end
    end

    caller = callerClass.new
    callee = Dramatis::Actor.new Object.new

    caller.caller callee

    Dramatis::Runtime.current.quiesce

    lambda { caller.exception }.should raise_error( NoMethodError )
  end

  it "should do something reasonable when the caller is main" do
    callee = Dramatis::Actor.new Object.new
    lambda { callee.callee }.should raise_error( NoMethodError )
    release( callee ).callee
    Dramatis::Runtime.current.warnings = false
    lambda { Dramatis::Runtime.current.quiesce }.should raise_error( Dramatis::Error::Uncaught )
    Dramatis::Runtime.current.warnings = true
    Dramatis::Runtime.current.exceptions.length.should equal( 1 )
    Dramatis::Runtime.current.clear_exceptions
  end

  it "should default to global when no dramatis_exception defined" do
    callerClass = Class.new do
      include Dramatis::Actor
      def caller callee
        release( callee ).callee
      end
    end

    caller = callerClass.new
    callee = Dramatis::Actor.new Object.new

    Dramatis::Runtime.current.warnings = false
    caller.caller callee
    lambda { Dramatis::Runtime.current.quiesce }.should raise_error( Dramatis::Error::Uncaught )
    Dramatis::Runtime.current.warnings = true

    lambda { raise Dramatis::Runtime.current.exceptions[0] }.should raise_error( NoMethodError )
    Dramatis::Runtime.current.clear_exceptions
      
  end

  it "should call dramatis_exception on main if that works(?)"

end
