describe "become" do

  after do
    begin
      Dramatis::Runtime.the.quiesce
      Dramatis::Runtime.the.exceptions.length.should equal( 0 )
      Thread.list.length.should equal( 1 )
    ensure
      Dramatis::Runtime.reset
    end
  end

  it "should forward messages on become"

  it "should do something with the patterns on become"

end
