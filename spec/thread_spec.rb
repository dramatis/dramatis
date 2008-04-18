describe "ruby" do

  it "should allow threads to access lexically scoped vars" do

    x = 12;

    Thread.new do
      p x
    end

  end

end
