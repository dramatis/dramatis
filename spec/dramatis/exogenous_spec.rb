require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/actor'
require 'dramatis/runtime'

describe "Dramatis::Actor" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

  it "should allow actors to be created from exogenous threads" do

    f = Class.new do
      include Dramatis::Actor

      def hi
        return :there
      end

    end

    t = Thread.new do
      actor = f.new
      actor.hi.should == :there
    end

    t.join

  end

end
