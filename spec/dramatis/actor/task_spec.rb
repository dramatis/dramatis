require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/task'
require 'dramatis/actor'

describe Dramatis::Runtime::Task do

  after do
    begin
      Dramatis::Runtime.the.quiesce
      Dramatis::Runtime.the.exceptions.length.should equal 0
      Thread.list.length.should equal 1
    ensure
      Dramatis::Runtime.reset
    end
  end

  it "should return errors to calling actor even when non-rpc (non-main)" do
    callerClass = Class.new do
      Dramatis::Actor::acts_as self
      def initalize
        @exception = nil
      end
      def caller callee
        Dramatis::Actor::cast( callee ).callee
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

    lambda { caller.exception }.should raise_error NoMethodError
  end

  it "should do something reasonable when the caller is main" do
    callee = Dramatis::Actor.new Object.new
    lambda { callee.callee }.should raise_error NoMethodError
    Dramatis::Actor::cast( callee ).callee
    Dramatis::Runtime::the.warnings = false
    lambda { Dramatis::Runtime.the.quiesce }.should raise_error Dramatis::Runtime::Exception
    Dramatis::Runtime::the.warnings = true
    Dramatis::Runtime.the.exceptions.length.should equal 1
    Dramatis::Runtime.the.clear_exceptions
  end

  it "should default to global when no dramatis_exception defined" do
    callerClass = Class.new do
      Dramatis::Actor::acts_as self
      def caller callee
        Dramatis::Actor::cast( callee ).callee
      end
    end

    caller = callerClass.new
    callee = Dramatis::Actor.new Object.new

    Dramatis::Runtime::the.warnings = false
    caller.caller callee
    lambda { Dramatis::Runtime.the.quiesce }.should raise_error Dramatis::Runtime::Exception
    Dramatis::Runtime::the.warnings = true

    lambda { raise Dramatis::Runtime.the.exceptions[0] }.should raise_error NoMethodError
    Dramatis::Runtime.the.clear_exceptions
      
  end

  it "should call dramatis_exception on main if that works(?)"

end
