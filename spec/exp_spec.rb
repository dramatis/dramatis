class Demo < StandardError

  def set_backtrace *args
    p "sbt!"
    super
  end

  def backtrace
    p "bt!"
    super
  end

end

d = Demo.new

p d.backtrace

begin
  raise d
rescue Exception => e
  p e.backtrace
end
