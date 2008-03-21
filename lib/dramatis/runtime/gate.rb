module Dramatis; end
class Dramatis::Runtime; end

class Dramatis::Runtime::Gate

  def self.new default, hash = {}
    actor = Hash.new true
    object = Hash.new default, hash
    continuation = Hash.new true
    gate = Hash.new true, :actor => actor, :object => object, :continuation => continuation
    gate
  end

  class Constant
    def initialize constant
      @constant = constant
    end
    def accepts? *args
      @constant
    end
    def refuse *args
      warn "HELLL"
      raise "hell"
    end
    def accept *args
      warn "HELLL"
      raise "hell"
    end
    def default *args
      warn "HELLL"
      raise "hell"
    end
    def set_default *args
      warn "HELLL"
      raise "hell"
    end
  end

  ACCEPT = Constant.new true
  REJECT = Constant.new false

  class Hash

    def initialize default, hash = {}
      @default = default
      @hash = hash.clone
      @queued = {}
      track and warn "create default = #{@default} hash = [#{@hash.keys.join(' ')}]"
    end

    def track; true; end

    def refuse *args
      track and warn "refuse [#{args.join(' ')}]"
      if args.length == 1
        @hash[args[0]] = REJECT
      else
        @hash[args[0]].refuse( *args[1,args.length] )
      end
    end

    def accept *args
      track and warn "accept [#{args.join(' ')}]"
      if args.length == 1
        @hash[args[0]] = ACCEPT
      else
        @hash[args[0]].accept( *args[1,args.length] )
      end
    end

    def default *args
      track and warn "default [#{args.join(' ')}]"
      if args.length == 1
        @hash.delete args[0]
      else
        @hash[args[0]].default( *args[1,args.length] )
      end
    end

    def set_default value, *args
      track and warn "default = #{value} [#{args.join(' ')}] #{args.length}"
      if args.length == 0
        @default = value
      else
        @hash[args[0]].set_default value, *args[1,args.length]
      end
    end

    def accepts? *args
      v = @hash.has_key?( args[0] ) ? @hash[args[0]].accepts?( *args[1,args.length] ) : @default
      track and warn "accepts? [#{args.join(' ')}] => #{v}"
      v
    end

  end

end
