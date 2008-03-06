require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

describe Dramatis::Actor do

  it "should be creatable from an acts_as class and return the right type" do

    f = Class.new do
      Actor::acts_as self
    end

    name = f.new
    name.should be_a_kind_of( Actor::Name )

  end

  it "should not insert new if not asked to" do

    f = Class.new do
      Actor::acts_as self, :new => :object
    end

    name = f.new
    name.should_not be_a_kind_of( Actor::Name )

  end

end
