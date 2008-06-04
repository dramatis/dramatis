require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/actor'
require 'dramatis/runtime'

# NB: don't use the module name here: rspec wants to include described
# modules ... and making the result into an actor, well, needless to say,
# it's not a good idea

describe "Dramatis::Actor" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

  it "should be creatable w/o requiring name" do

    f = Class.new do
      include Dramatis::Actor
    end

    name = f.new

    # if we get here, we're fine
    # is there an rspec "something" for this?
    # note, this doesn't "test" in autotest,
    # probably because it's eval'ing other tests that
    # do bring in Actor::Name

  end

end
