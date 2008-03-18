module Dramatis; end

class Dramatis::Error < StandardError; end
class Dramatis::Deadlock < Dramatis::Error; end
class Dramatis::BindError < Dramatis::Error; end


