module Dramatis; end
class Dramatis::Runtime; end

begin require 'pp'; rescue Exception; end

class Dramatis::Runtime::Gate

  def self.new
    gate = Case.new
    gate.accept Object
    gate.accept :actor
    gate.accept :continuation
    gate.accept :object
    gate
  end

  class Case
    def always args, value, options = {}
      _change @always, Array( args ), value, options
    end
    def only args, options = {}
      _change @list, [ :object ], false, options
      _change @list, [ :continuation ], false, options
      _change @list, [ :continuation, Object, :exception ], true, options
      _change @list, Array( args ), true, options
    end
    def default args, options = {}
      _change @list, args, nil, options
    end
    def default_by_tag tag
      _change @list, nil, nil, { :tag => tag }
    end
    def _change list, args, value, options = {}
      inplace = options[ :inplace ]
      tag = options[ :tag ]
      # pp ">> #{args and args.join(' ')} #{value} #{inplace} #{tag}", list
      prepend = true
      tbd = []
      list.each_with_index do |entry, list_index|
        vector, result, entry_tag = entry
        matches = true
        if tag and entry_tag != tag
          # p "#{tag} tag and #{entry_tag} dont' match"
          next
        end
        if args
          args.each_with_index do |arg, arg_index|
            # p "compare #{vector[arg_index]} #{arg}"
            if vector[arg_index] != arg
              # p "matches"
              matches = false
              break
            end
          end
        end
        if matches
          # p "matched"
          if inplace and value != nil
            # p "inplace"
            list[list_index][1] = value
            # FIX: tag?
            prepend = false
          else
            # p "schedule remove #{list[list_index].join(' ')} at #{list_index}"
            tbd << list_index
            # list[list_index,1] = []
          end
          # break
        end
      end
      tbd.reverse.each do |index|
        # p "remove #{index} #{list[index].join(' ')} at #{index}"
        list[index,1] = []
      end
      if prepend and value != nil
        list.unshift [ args, value, tag ]
      end
      # pp "<< #{args and args.join(' ')} #{value} #{inplace} #{tag}", list
    end
    def change args, value, inplace
      # pp "> #{args.join(' ')} #{value} #{inplace}", @list
      prepend = true
      @list.each_with_index do |entry, list_index|
        vector, result = entry
        matches = true
        args.each_with_index do |arg, arg_index|
          # p "compare #{vector[arg_index]} #{arg}"
          if vector[arg_index] != arg
            # p "matches"
            matches = false
            break
          end
        end
        if matches
          # p "matched"
          if inplace
            # p "inplace"
            @list[list_index][1] = value
            # FIX: tag?
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
      # pp "<", @list
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
      # p "accepts? #{args.join(" ")}"
      accepted = nil
      ( @always + @list ).each do |entry|
        vector, result = entry
        matches = true
        vector.each_with_index do |v, i|
          # warn "#{v} #{args[i]} => #{v === args[i]} so #{result}"
          if not( v === args[i] )
            matches = false
            break
          end
        end
        if matches
          accepted = result
          break
        end
      end
      # warn "last '#{accepted}'"
      # p "accepts? #{args.join(" ")} => '#{accepted}'"
      accepted == true
    end
    def initialize
      @always = []
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

    def track; false; end

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
