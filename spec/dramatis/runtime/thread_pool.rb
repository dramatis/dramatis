require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/thread_pool'

describe Dramatis::Runtime::ThreadPool do

  it "should be resetable" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    thread_pool.reset
  end

  it "should allocate threads that do stuff" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    @x = 1
    thread_pool.new do
      @x = 2
    end
    thread_pool.reset
    @x.should == 2
  end

end
