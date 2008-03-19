module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/scheduler'
require 'thread'

class Dramatis::Runtime::Task

  attr_reader :actor

  def type
    @dest
  end

  def method
    @args[0]
  end

  def arguments
    @args[1,@args.length]
  end

  def initialize actor, dest, args, options
    @actor = actor
    @dest = dest
    @args = args
    case options[:continuation]
    when :none
      @continuation = Continuation::None.new
    when :rpc
      @continuation = Continuation::RPC.new
    when Proc
      @continuation = Continuation::Proc.new options[:continuation]
    else
      raise "hell 2 " + options.inspect
    end
  end

  def exception e
    @continuation.exception e
  end

  def queued
    @continuation.queued
  end

  def deliver
    @actor.deliver @dest, @args, @continuation
  end

  private

  module Continuation

    class None
      def queued
      end

      def result result
      end

      def exception exception
        Dramatis::Runtime.the.exception exception
      end

    end

    class RPC

      def initialize
        # all the synchronizaton here probably gets tossed
        # proof:
        # to create the continuation, you have to have the actor lock
        # to deliver the continuation you have to have the actor lock
        # QED
        # when this wasn't a message send, you could try to execute before
        # but now that's not possible; it'll get queued
        # i think
        # of course, it should be harmless
        # fix ... might want to seperate the two parts of a continuation
        # the value part and the thread state part
        @state = :start
        @mutex = Mutex.new
        @wait = ConditionVariable.new
        @actor = Dramatis::Actor::Name( Dramatis::Actor.current ).continuation( self )
      end

      def queued

        @mutex.synchronize do
          raise "hell " + @state.to_s if @state != :start and @state != :signaled
          if @state == :start
            @state = :waiting
            begin
              Dramatis::Runtime::Scheduler.the.suspend_notification self
              begin
                # FIX: only recieve this continuation or call chain
                # note that up until the call to schedule, we hold the actor
                # during the schedule call, we lose it ... but we hold the current
                # lock. I think this means that while the state stuff is not necessary
                # the semaphore is ... of course
                @actor.instance_eval do
                  @actor.schedule
                end
                @wait.wait @mutex
              rescue Exception => exception
                warn "wait said #{exception}"
                raise exception
              end
            ensure
              Dramatis::Runtime::Scheduler.the.wakeup_notification self
            end
            raise "hell" if @state != :done
          end
        end

        raise "hell " + @type.inspect if ![ :return, :exception ].include? @type
        case @type
        when :return
          return @value
        when :exception
          raise @value
        end
      end

      def result result
        @actor.result result
      end

      def exception exception
        # warn "4 exception " + exception.to_s
        @actor.exception exception
        # warn "4 delivered ".to_s
      end

      def continuation_result result
        @mutex.synchronize do
          raise "hell" if @state != :start and @state != :waiting
          @type = :return
          @value = result
          if @state == :start
            @state = :signaled
          else
            @state = :done
            @wait.signal
          end
        end
      end

      def continuation_exception exception
        @mutex.synchronize do
          raise "hell" if @state != :start and @state != :waiting
          @type = :exception
          @value = exception
          if @state == :start
            @state = :signaled
          else
            @state = :done
            @wait.signal
          end
        end
      end

    end

    class Proc

      def initialize block
        @block = block
        @actor = Dramatis::Actor::Name( Dramatis::Actor.current ).continuation( self )
      end

      def queued; end

      def result result
        @actor.result result
      end

      def exception exception
        Dramatis::Runtime.the.exception exception
      end

      def continuation_result result
        @block.call result
      end

    end

  end

end
