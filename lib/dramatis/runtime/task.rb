module Dramatis; end
module Dramatis::Runtime; end

class Dramatis::Runtime::Task

  module Continuation

    class None
      def queued
      end
    end

    class RPC
      def queued
      end
    end

  end

  def initialize actor, dest, args, options
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

end
