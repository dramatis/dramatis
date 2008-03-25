def f &block
  re = Regexp.new( "^lib\\/(.*)\\.rb$" )
  yield re
end

fn = "lib/foo.rb"

f { |re|
  fn =~ re
  raise "hell" if Regexp.last_match.to_s != "lib/foo.rb"
}

raise "hell" if Regexp.last_match.to_s != "lib/foo.rb"
