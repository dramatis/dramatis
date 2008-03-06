require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/actor'

describe Dramatis::Actor do

  it "should be creatable w/o requiring name" do

    f = Class.new do
      Actor::acts_as self
    end

    name = f.new
    success

  end

end
