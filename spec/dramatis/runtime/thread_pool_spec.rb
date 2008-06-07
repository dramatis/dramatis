require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime/thread_pool'

describe Dramatis::Runtime::ThreadPool do

  it "should be resetable" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    thread_pool.reset
  end

  # this could be done with join, but is not currently
  # needed for anything but the test, so ...
  it "should allocate threads that do stuff" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    x = 1
    t = thread_pool.new do
      sleep 0.1
      x = 2
    end
    x.should == 1
    thread_pool.length.should == 0
    sleep 0.2
    x.should == 2
    thread_pool.length.should == 1
    thread_pool.reset
    thread_pool.length.should == 0
  end

  it "should wait for all thread to be checked in" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    thread_pool.new do
      sleep 0.1
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 1
    sleep 0.2
    thread_pool.length.should == 1
    thread_pool.size.should == 1
    thread_pool.new do
      sleep 0.1
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 1
    thread_pool.new do
      sleep 0.3
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 2
    sleep 0.2
    thread_pool.length.should == 1
    thread_pool.size.should == 2
    thread_pool.reset
    thread_pool.length.should == 0
    thread_pool.size.should == 0
  end

  it "shouldn't wait if soft reset" do
    thread_pool = Dramatis::Runtime::ThreadPool.new
    thread_pool.new do
      sleep 0.1
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 1
    sleep 0.2
    thread_pool.length.should == 1
    thread_pool.size.should == 1
    thread_pool.new do
      sleep 0.1
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 1
    thread_pool.new do
      sleep 0.3
    end
    thread_pool.length.should == 0
    thread_pool.size.should == 2
    sleep 0.2
    thread_pool.length.should == 1
    thread_pool.size.should == 2
    thread_pool.reset true
    thread_pool.length.should == 0
    thread_pool.size.should == 1
    thread_pool.reset
    thread_pool.length.should == 0
    thread_pool.size.should == 0
  end

end
