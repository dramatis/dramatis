# :title: Dramatis Actor Library Ruby API

module Dramatis #:doc:
end

# The base class of all non-internal dramatis exceptions.

class Dramatis::Error < StandardError; end

# Raised when an attempt is made to create an interface object on an
# object that does not define an interface class.

class Dramatis::Error::Interface < StandardError; end

# Raised when an attempt is made to bind an already-bound actor.

class Dramatis::Error::Bind < Dramatis::Error; end

require 'dramatis/deadlock'
require 'dramatis/uncaught'

require 'dramatis/future/interface'
require 'dramatis/actor/name/interface'

# The Dramatis module provides methods to manipulate dramatis objects
# by client code. Each function can be accessed as a module function, e.g.,
#   Dramatis.interface(...)
# or by including the Dramatis module and using as an instance method, .e.,g
#   include Dramatis
#   interface(...)

module Dramatis #:doc:

  # :call-seq:
  #   interface(object, *args, &block) -> an_interface
  #
  # Takes a dramatis proxy object and returns an object that can be
  # used to operate directly on the proxy, rather than on the proxied
  # object. Since dramatis objects like actor names and futures are
  # proxy objects, normal method calls on them are directed to the
  # proxied object. In order to perform operations on the proxies
  # themselves, the interface method is used to get access to a
  # non-proxy object. If the object passed is a Dramatis::Actor::Name,
  # the result is a Dramatis::Actor::Name::Interface object. If the
  # object passed is a Dramatis::Future, the result is a
  # Dramatis::Future::Interface object.

  def interface object, *args, &block 
    interface = nil    
    begin
      interface = object.class.const_get( :Interface )
    rescue NameError => name_error
      raise Dramatis::Error::Interface.new(  "object is not a dramatis interfacable object" )
    end
    interface.new( object, *args, &block )
  end

  module_function :interface

  # :call-seq:
  #   release(name) -> a_name
  #
  # Takes an actor name and returns a new actor name which, when used
  # as the target of a method call, will pass a null continuation. As
  # a result, the call will not block or otherwise wait for a
  # result. The result of such a call is always nil.

  def release name
    interface( name ).continue nil
  end

  module_function :release

  # :call-seq:
  #   future(name) -> a_name
  #
  # Takes an actor name and returns a new actor name which, when used
  # as the target of a method call, will pass a future
  # continuation. It immediately returns a Dramatis::Future object.

  def future name
    interface( name ).future
  end

  module_function :future

end
