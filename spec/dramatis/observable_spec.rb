require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

# NB: don't use the module name here: rspec wants to include described
# modules ... and making the result into an actor, well, needless to say,
# it's not a good idea

require 'observer'

describe "Dramatis::Actor" do

  include Dramatis

  include DramatisSpecHelper

  after(:each) { runtime_check }

  Actor = Dramatis::Actor

  it "should play well with observable" do

    observedClass = Class.new do
      include Observable
      include Dramatis::Actor
      def foo
        changed
        notify_observers("foo called")
      end
    end

    observed = observedClass.new
    observed.respond_to?( :foo ).should be_true
    observed.respond_to?( :bar ).should be_false

    observerClass = Class.new do

      include Dramatis::Actor

      attr_reader :message

      def initialize observed
        actor.always :respond_to?, true
        observed.add_observer actor.name
      end

      def update message
        @message = message
      end

    end

    observer = observerClass.new observed
    observer.message.should == nil
    observed.foo
    observer.message.should == "foo called"
    
  end

  it "should play well with observable even when we want async callbacks" do

    observedClass = Class.new do
      include Observable
      include Dramatis::Actor
      def foo
        changed
        notify_observers("foo called")
      end
      def add_observer actor
        release_name = release( actor )
        class << release_name
          def respond_to? message
            message == :update ? true : super
          end
        end
        super release_name
      end
    end

    observed = observedClass.new

    observerClass = Class.new do

      include Dramatis::Actor

      attr_reader :message

      def initialize observed
        actor.always :respond_to?, true
        observed.add_observer actor.name
      end

      def update message
        @message = message
      end

      def doit observed
        observed.foo
      end

    end

    observer = observerClass.new observed
    observer.message.should == nil
    observer.doit observed
    observer.message.should == "foo called"
    
  end

end
