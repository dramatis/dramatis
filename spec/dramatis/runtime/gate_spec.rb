require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/gate'

describe Dramatis::Runtime::Gate do

  before do
    @gate = Dramatis::Runtime::Gate.new
  end

  it "should accept simple defaults" do
    @gate.accepts?( :actor ).should be_true
    @gate.accepts?( :continuation ).should be_true
    @gate.accepts?( :object ).should be_true
    @gate.accepts?( :foo ).should be_true
  end

  it "should return the gate list" do
    @gate.list.should == [ [[:object], true],
                           [[:continuation], true],
                           [[:actor], true],
                           [[Object], true] ]
  end

  it "should obey simple changes" do
    @gate.refuse :object
    @gate.accepts?( :object ).should be_false
    @gate.list.should == [ [[:object], false],
                           [[:continuation], true],
                           [[:actor], true],
                           [[Object], true] ]
  end

  it "should reorder on change" do
    @gate.refuse :actor
    @gate.accepts?( :actor ).should be_false
    @gate.list.should == [ [[:actor], false],
                           [[:object], true],
                           [[:continuation], true],
                           [[Object], true] ]
  end

  it "should reorder on no change" do
    @gate.accept :actor
    @gate.accepts?( :actor ).should be_true
    @gate.list.should == [ [[:actor], true],
                           [[:object], true],
                           [[:continuation], true],
                           [[Object], true] ]
  end

  it "should obey simple changes" do
    @gate.refuse :object
    @gate.accepts?( :object ).should be_false
  end

end
