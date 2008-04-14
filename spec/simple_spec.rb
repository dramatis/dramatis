describe "ruby" do

  it "should work" do

    module SimpleM

      class Wrapper
        def initialize wrapped
        end
      end

      module Class
        def new
          Wrapper.new super
        end
      end

      def self.included cls
        cls.extend Class
      end

    end

    class SimpleC; include SimpleM; end

    SimpleC.new.should be_kind_of( SimpleM::Wrapper )

  end

end
