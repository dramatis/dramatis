class Demo < StandardError

  def set_backtrace *args
    super
  end

  def backtrace
    super
  end

end

d = Demo.new

# p d.backtrace

begin
  raise d
rescue Exception => e
  # p e.backtrace
end
