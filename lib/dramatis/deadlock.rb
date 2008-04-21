module Dramatis; end

class Dramatis::Error < StandardError; end

class Dramatis::Deadlock < Dramatis::Error

  attr_accessor :raw_backtrace

  def initialize string = nil, opts = {}
    super string
    @next = opts[:next]
    @raw_backtrace = []
  end

  # how things stand:
  # r18 and r19 call set_backtrace after at the raise
  # jr never calls it; instead the base class synthesizes it at the first backtrace call
  # as far as frames go, it seems lke jr elides sends sometimes

  def set_backtrace *args
    # p "sbt!"
    # pp args[0]
    array = @raw_backtrace = args[0]
    if @next
      @raw_backtrace = @next.raw_backtrace + @raw_backtrace
      array = @next.backtrace + array
    end

    # pp "raw", @raw_backtrace

    # remove the scheduler

    filtered = []
    array.each do |v|
      file, line, func = v.split ':'
      file =~ %r{/lib/dramatis/} or ( filtered <<  v and next )
      func =~ %r{\Wmaybe_deadlock\W} and next
      file =~ %r{/runtime/scheduler} and func =~ %r{\Wrun\W} and break
      filtered <<  v
    end
    
    # remove queueing delivery

    array = filtered
    filtered = []
    skipping = false
    array.each do |v|

      # p v

      file, line, func = v.split ':'

      if file !~ %r{/lib/dramatis/}
        # p "not skipping x"
        skipping = false
        filtered <<  v
        next
      end

      if !skipping and 
         ( ( file =~ %r{/runtime/task} and func =~ %r{\Wqueued\W} ) or
           ( file =~ %r{/runtime/actor} and func =~ %r{\Wsend\W} ) or # r18, r19
           ( file =~ %r{/runtime/actor} and func =~ %r{\Wdeliver\W} )  ) # jr
        # p "skipping"
        skipping = true
        next
      end

      if file =~ %r{/dramatis/actor/name} and func =~ %r{\Wmethod_missing\W}
        # p "not skipping"
        skipping = false
        next
      end

      skipping or filtered <<  v

    end

    # pp "filt", filtered
    # pp args[0]
    super filtered
    # super args[0]
  end

  def backtrace
    if @raw_backtrace.empty?
      bt = super
      if bt
        set_backtrace  bt
      end
    end
    # p "d"
    super
  end

end
