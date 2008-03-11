require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/shoes'

describe Dramatis::Shoes do

  it "should provide a handle to the runtime" do
    runtime = Dramatis::Shoes.runtime
  end

end
