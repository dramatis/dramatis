module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/scheduler'
require 'dramatis/runtime/future'
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

  attr_reader :call_thread

  def initialize actor, dest, args, options
    @actor = actor
    @dest = dest
    @args = args

    @call_thread = nil

    name = Dramatis::Actor.current
    actor = name.instance_eval { @actor }
    if actor.call_threading?
      # warn "oct #{options[:call_thread]} act #{actor.call_thread}"
      raise "hell" if options[:call_thread] and
                       actor.call_thread and
                       options[:call_thread] != actor.call_thread
      @call_thread = actor.call_thread
      if @call_thread == nil
        @call_thread = self.to_s
      end
    end

    # warn "task #{self} #{args[0]} call thread [ #{@call_thread} ] #{options.to_a.join(' ')}"

    case options[:continuation]
    when :none
      @continuation = Continuation::None.new name, @call_thread
    when :rpc
      @continuation = Continuation::RPC.new name, @call_thread
    when :future
      @continuation = Continuation::Future.new name, @call_thread
    when Proc
      @continuation = Continuation::Proc.new name,  @call_thread, options[:continuation],
                                                                    options[:exception]
    else
      raise Dramatis::Internal.new( "invalid contiunation type" )
    end
  end

  def exception e
    @continuation.exception e
  end

  def queued
    @continuation.queued
  end

  def deliver
    @actor.deliver @dest, @args, @continuation, @call_thread
  end

  private

  module Continuation

    class None
      def queued
      end

      def result result
      end

      def exception exception
        # warn "except nil #{exception}"
        # true and pp exception.backtrace
        Dramatis::Actor::Name( Dramatis::Actor::cast( @name ) ).exception exception
      end

      def initialize name, call_thread
        @name = name
      end
    end

    class RPC

      def actor
        @actor.instance_eval { @actor }
      end

      def initialize name, call_thread
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
        @call_thread = call_thread
        # warn "contiunation to #{actor}"
        @actor = Dramatis::Actor::Name( Dramatis::Actor.current ).send :continuation, self, :call_thread => call_thread
      end

      def queued

        @mutex.synchronize do
          raise "hell " + @state.to_s if @state != :start and @state != :signaled
          if @state == :start
            @state = :waiting
            begin
              tag = to_s
              call_thread = @call_thread
              @actor.instance_eval do
                @actor.instance_eval do
                  # warn "#{self} ct [ #{call_thread} ]"
                  @call_thread = call_thread
                end
                @actor.gate.only [ :continuation, tag ], :tag => tag
                @actor.schedule self
              end
              begin
                Dramatis::Runtime::Scheduler.the.suspend_notification self
                @wait.wait @mutex
                # this causes a deadlock if the waking thread, which may be
                # retiring, does so before this thead has awakend and notified
                # the scheduler
                # sleep 1
              ensure
                # Dramatis::Runtime::Scheduler.the.wakeup_notification self
              end
            ensure
              tag = to_s
              @actor.instance_eval do
                @actor.gate.default_by_tag tag
              end
            end
            raise "hell" if @state != :done
          end
        end

        raise "hell " + @type.inspect if ![ :return, :exception ].include? @type
        case @type
        when :return
          return @value
        when :exception
          begin
            # raise "hell for #{@value}"
          rescue Exception => e
            pp "#{e}", e.backtrace
          end
          raise @value
        end
      end

      def result result
        @actor.result result
      end

      def exception exception
        # warn "4 exception " + exception.to_s
        # warn "4 exception " + exception.backtrace.join("\n")
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
            Dramatis::Runtime::Scheduler.the.wakeup_notification self
            @wait.signal
          end
        end
      end

      def continuation_exception exception
        # warn "except rpc"
        @mutex.synchronize do
          raise "hell" if @state != :start and @state != :waiting
          @type = :exception
          @value = exception
          if @state == :start
            @state = :signaled
          else
            @state = :done
            Dramatis::Runtime::Scheduler.the.wakeup_notification self
            @wait.signal
          end
        end
      end

    end

    class Proc

      def initialize name, call_thread, result, except
        # p "p.n #{call_thread} #{result} #{except}"
        @result_block = result
        @exception_block = except
        @name = name
        @continuation = \
          Dramatis::Actor::Name( Dramatis::Actor.current ) \
             .send :continuation, self, :call_thread => call_thread
      end

      def queued; end

      def result result
        @continuation.result result
      end

      def exception exception
        @continuation.exception exception
      end

      def continuation_result result
        @result_block.call result
      end

      def continuation_exception exception
        # warn "delivering #{exception} => #{@exception_block}"
        # pp exception.backtrace
        if @exception_block
          @exception_block.call exception
        else
          Dramatis::Actor::cast( @name ).dramatis_exception exception
        end
      end

    end

    class Future

      def initialize name, call_thread
        @state = :start
        @mutex = Mutex.new
        @wait = ConditionVariable.new
        @call_thread = call_thread
        # warn "contiunation to #{actor}"
        @actor = Dramatis::Actor::Name( Dramatis::Actor.current ) \
          .send :continuation, self, :call_thread => call_thread
      end

      def ready?
        @mutex.synchronize { @state == :done  or @state == :signaled }
      end

      def value
        @mutex.synchronize do
          if @state == :start
            @state = :waiting
            begin
              tag = to_s
              call_thread = @call_thread
              @actor.instance_eval do
                @actor.instance_eval do
                  @call_thread = call_thread
                end
                @actor.gate.only [ :continuation, tag ], :tag => tag
                @actor.schedule self
              end
              begin
                Dramatis::Runtime::Scheduler.the.suspend_notification self
                @wait.wait @mutex
                # this causes a deadlock if the waking thread, which may be
                # retiring, does so before this thead has awakend and notified
                # the scheduler
                # sleep 1
              ensure
                # Dramatis::Runtime::Scheduler.the.wakeup_notification self
              end
            ensure
              tag = to_s
              @actor.instance_eval do
                @actor.gate.default_by_tag tag
              end
            end
            raise "hell" if @state != :done
          end
        end

        raise "hell " + @type.inspect if ![ :return, :exception ].include? @type
        case @type
        when :return
          return @value
        when :exception
          begin
            # raise "hell for #{@value}"
          rescue Exception => e
            pp "#{e}", e.backtrace
          end
          raise @value
        end
      end

      def queued
        return Dramatis::Runtime::Future.new( self )
      end

      def result result
        @actor.result result
      end

      def exception exception
        # warn "4 exception " + exception.to_s
        # warn "4 exception " + exception.backtrace.join("\n")
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
            Dramatis::Runtime::Scheduler.the.wakeup_notification self
            @wait.signal
          end
        end
      end

      def continuation_exception exception
        # warn "except rpc"
        @mutex.synchronize do
          raise "hell" if @state != :start and @state != :waiting
          @type = :exception
          @value = exception
          if @state == :start
            @state = :signaled
          else
            @state = :done
            Dramatis::Runtime::Scheduler.the.wakeup_notification self
            @wait.signal
          end
        end
      end
    end
  end

end
