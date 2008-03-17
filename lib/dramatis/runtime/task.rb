module Dramatis; end
module Dramatis::Runtime; end

require 'dramatis/runtime/scheduler'
require 'thread'

class Dramatis::Runtime::Task

  attr_reader :actor

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
      @continuation = Continuation::Proc.new
    else
      raise "hell" + options.inspect
    end
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
    end

    class RPC
      def initialize
        @state = :start
        @mutex = Mutex.new
        @wait = ConditionVariable.new
      end

      def queued

        Dramatis::Runtime::Scheduler.the.suspend_notification self

        @mutex.synchronize do
          raise "hell " + @state.to_s if @state != :start and @state != :signaled
          if @state == :start
            @state = :waiting
            @wait.wait @mutex
            raise "hell" if @state != :done
          end
        end

        raise "hell " + @type.inspect if ![ :return, :exception ].include? @type
        case @type
        when :return
          return @value
        when :exception
          warn "this path has no test code"
          raise @value
        end
      end

      def result result
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

      def exception exception
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

      def continue
        kick
      end
      def kick
      end
    end

  end

end
