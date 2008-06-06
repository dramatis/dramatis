module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/scheduler'
require 'dramatis/future'
require 'thread'

class Dramatis::Runtime::Task #:nodoc: all

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
    @args = args.dup

    @call_thread = nil

    name = Dramatis::Runtime::Scheduler.actor
    actor = name.instance_eval { @actor }

    object_id = actor.object.object_id

    @args.each_with_index do |arg,i|
      if arg.object_id == object_id
        @args[i] = name
      end
    end

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
      @continuation = Continuation::RPC.new name,
                                              @call_thread,
                                              options[:nonblocking]
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

      include Dramatis

      def queued
      end

      def result result
      end

      def exception exception
        # warn "except nil #{exception}"
        # true and pp exception.backtrace
        interface( release( @name ) ).exception exception
      end

      def initialize name, call_thread
        @name = name
      end
    end

    class RPC

      include Dramatis

      def actor
        @actor.instance_eval { @actor }
      end

      def initialize name, call_thread, nonblocking
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
        @blocking = !nonblocking
        @state = :start
        @mutex = Mutex.new
        @wait = ConditionVariable.new
        @call_thread = call_thread
        # warn "contiunation to #{actor}"
        @actor = interface( Dramatis::Runtime::Scheduler.actor ).send :continuation, self, :call_thread => call_thread
      end

      def queued

        @mutex.synchronize do
          raise "hell " + @state.to_s if @state != :start and @state != :signaled
          if @state == :start
            @state = :waiting
            begin
              tag = to_s
              call_thread = @call_thread
              blocking = @blocking
              @actor.instance_eval do
                @actor.instance_eval do
                  # warn "#{self} ct [ #{call_thread} ]"
                  @call_thread = call_thread
                end
                if blocking
                  @actor.gate.only [ :continuation, tag ], :tag => tag
                end
                @actor.schedule self
              end
              begin
                Dramatis::Runtime::Scheduler.current.suspend_notification self
                @wait.wait @mutex
                # this causes a deadlock if the waking thread, which may be
                # retiring, does so before this thead has awakend and notified
                # the scheduler
                # sleep 1
              ensure
                # Dramatis::Runtime::Scheduler.current.wakeup_notification self
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
          # if Dramatis::Deadlock === @value 
          # @value = Dramatis::Deadlock.new nil, :next => @value
          # end
          # pp "reraise", caller
          @value._dramatis_reraise
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
            Dramatis::Runtime::Scheduler.current.wakeup_notification self
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
            Dramatis::Runtime::Scheduler.current.wakeup_notification self
            @wait.signal
          end
        end
      end

    end

    class Proc

      include Dramatis

      def initialize name, call_thread, result, except
        # p "p.n #{call_thread} #{result} #{except}"
        @result_block = result
        @exception_block = except
        @name = name
        @continuation = \
          interface( Dramatis::Runtime::Scheduler.actor ) \
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
          release( @name ).dramatis_exception exception
        end
      end

    end

    class Future

      include Dramatis

      def initialize name, call_thread
        @state = :start
        @mutex = Mutex.new
        @wait = ConditionVariable.new
        @call_thread = call_thread
        # warn "contiunation to #{actor}"
        @actor = interface( Dramatis::Runtime::Scheduler.actor ) \
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
                Dramatis::Runtime::Scheduler.current.suspend_notification self
                @wait.wait @mutex
                # this causes a deadlock if the waking thread, which may be
                # retiring, does so before this thead has awakend and notified
                # the scheduler
                # sleep 1
              ensure
                # Dramatis::Runtime::Scheduler.current.wakeup_notification self
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
        Dramatis::Future.new( self )
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
            Dramatis::Runtime::Scheduler.current.wakeup_notification self
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
            Dramatis::Runtime::Scheduler.current.wakeup_notification self
            @wait.signal
          end
        end
      end
    end
  end

end
