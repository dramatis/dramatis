begin
  require 'spec'
rescue LoadError
  require 'rubygems'
  gem 'rspec'
  require 'spec'
end

$:.push File.join( File.dirname(__FILE__), "..", "lib" )
