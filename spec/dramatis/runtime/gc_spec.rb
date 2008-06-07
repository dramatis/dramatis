require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/actor'

describe "dramatis actor gc" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

end
