require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/actor'
require 'dramatis/runtime'

describe Dramatis::Actor do

  after do
    Dramatis::Runtime.reset
  end

  after do
    Dramatis::Runtime.the.quiesce
    pp "after", Thread.main, Thread.list
    Thread.list.length.should == 1
  end

  it "should be creatable w/o requiring name" do

    f = Class.new do
      Dramatis::Actor::acts_as self
    end

    name = f.new

    # if we get here, we're fine
    # is there an rspec "something" for this?
    # note, this doesn't "test" in autotest,
    # probably because it's eval'ing other tests that
    # do bring in Actor::Name

  end

end
