module Dramatis; end
class Dramatis::Runtime; end

require 'pp' # FIX

class Dramatis::Runtime::Gate

  def self.new
    gate = Case.new
    gate.accept Object
    gate.accept :actor
    gate.accept :continuation
    gate.accept :object
    gate
  end

  def tbd
    actor = Hash.new true
    object = Hash.new default, hash
    continuation = Hash.new true
    gate = Hash.new true, :actor => actor, :object => object, :continuation => continuation
    gate
  end

  class Case
    def change args, value, inplace
      pp "> #{args.join(' ')} #{value} #{inplace}", @list
      prepend = true
      @list.each_with_index do |entry, list_index|
        p "huh?"
        vector, result = entry
        p "ho"
        p "?? #{vector} ?? #{result}"
        matches = true
        args.each_with_index do |arg, arg_index|
          p "compare #{vector[arg_index]} #{arg}"
          if vector[arg_index] != arg
            p "matches"
            matches = false
            break
          end
        end
        if matches
          p "matched"
          if inplace
            p "inplace"
            @list[list_index][1] = value
            prepend = false
          else
            @list[list_index,1] = []
          end
          break
        end
      end
      if prepend
        @list.unshift [ args, value ]
      end
      pp "<", @list
    end
    def accept *args
      change args, true, false
    end
    def refuse *args
      change args, false, false
    end
    def update value, *args
    end
    def accepts? *args
      accepted = nil
      @list.each do |entry|
        vector, result = entry
        args.each_with_index do |arg, i|
          warn "#{vector[i]} #{arg} => #{vector[i] === arg} so #{result}"
          vector[i] === arg and accepted = result
        end
        warn "last" if accepted != nil
        break if accepted != nil
      end
      accepted == true
    end
    def initialize
      @list = []
    end
    def list
      @list
    end
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
      @always = []
      @call_thread = nil
    end

    def track; true; end

    def always *args
      track and warn "always [#{args.join(' ')}]"
      @always << args
    end

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
      v = nil
      if v == nil 
        v = @hash.has_key?( args[0] ) ? @hash[args[0]].accepts?( *args[1,args.length] ) : @default
      end
      track and warn "accepts? [#{args.join(' ')}] => #{v}"
      v
    end

  end

end
