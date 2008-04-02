require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/task'
require 'dramatis/actor'

describe Dramatis::Runtime::Task do

  after do
    Dramatis::Runtime.the.quiesce
    warn "after " + Thread.list.join( " " ) if Thread.list.length != 1
    Thread.list.length.should == 1
  end

  it "should return errors to calling actor even when non-rpc (non-main)" do

    pending

    def do_call callee
      lambda { Dramatis::Actor::cast( callee ).call } \
        .should raise_error "something"
    end

    callerClass = Class.new do
      Dramatis::Actor::acts_as self
      def call spec, callee
        spec.do_call callee
      end
    end

    caller = callerClass.new
    callee = Dramatis::Actor.new Object.new

    caller.call self, callee
  end

  it "should do something reasonable when the caller is main" do
  end

end
