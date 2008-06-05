#!/usr/bin/env ruby

begin
  raise Exception.new( "hell" )
rescue Exception => e
  p e
  p e.backtrace
end

begin
  begin
    raise Exception.new( "hell" )
  rescue Exception => e
    p e
    p e.backtrace
    raise e
  end
rescue Exception => e
  p e
  p e.backtrace
end

