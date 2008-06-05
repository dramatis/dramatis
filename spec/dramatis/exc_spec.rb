require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/actor'
require 'dramatis/runtime'

# NB: don't use the module name here: rspec wants to include described
# modules ... and making the result into an actor, well, needless to say,
# it's not a good idea

describe "Dramatis::Actor" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

  it "should provide pretty backtraces" do

    foo = Class.new do
      include Dramatis::Actor
      
      @@first_line = nil

      def self.first_line
        return @@first_line
      end

      def foo that
        @@first_line = __LINE__.to_i + 1
        return that.bar
      end

    end

    bar = Class.new do
      include Dramatis::Actor
          
      @@second_line = nil

      def self.second_line
        return @@second_line
      end

      def bar
        @@second_line = __LINE__.to_i + 1
        name_error
        return "foobar"
      end

    end

    aFoo = foo.new
    aBar = bar.new
    okay = false
    r = "xyzzy"
    begin
      r = aFoo.foo aBar
    rescue NameError => ne
      tb = ne.backtrace

      # pp tb

      f,l = tb[0].split ":"
      f.should == __FILE__
      l.to_i.should == bar.second_line

      f,l = tb[1].split ":"
      f.should == __FILE__
      l.to_i.should == foo.first_line

      okay = true
    end

    r.should == "xyzzy"
    okay.should be_true

  end

end
