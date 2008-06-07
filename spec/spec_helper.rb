begin
  require 'spec'
rescue LoadError
  require 'rubygems'
  gem 'rspec'
  require 'spec'
end

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis'

module DramatisSpecHelper

  def runtime_check
    begin
      Dramatis::Runtime.current.quiesce
      Dramatis::Runtime.current.exceptions.length.should equal( 0 )
      Thread.list.length.should \
        equal( 1 + Dramatis::Runtime::Scheduler.current.thread_count )
    ensure
      Dramatis::Runtime.reset
      Thread.list.length.should == 1
    end
  end

end

