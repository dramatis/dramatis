module Dramatis #:doc:
end

class Dramatis::Error < StandardError; end
class Dramatis::Error::Name < StandardError; end
class Dramatis::Error::Bind < Dramatis::Error; end

require 'dramatis/deadlock'

require 'dramatis/future/interface'
require 'dramatis/actor/name/interface'

module Dramatis #:doc:

  def interface *args, &block 
    interface = nil    
    begin
      interface = args[0].class.const_get( :Interface )
    rescue NameError => name_error
      raise Dramatis::Error::Name.new(  "object is not a dramatis interfacable object" )
    end
    interface.new( *args, &block )
  end

  module_function :interface

  def cast name
    interface( name ).continue nil
  end

  module_function :cast

  def future name
    interface( name ).future
  end

  module_function :future

end
