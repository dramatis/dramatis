describe "ruby" do

  it "should work" do

    module Simple2M

      class Wrapper
        def initialize wrapped
        end
      end

      def self.included cls
        class << cls
          def new
            Wrapper.new super
          end
        end
      end

    end

    class Simple2C; include Simple2M; end

    Simple2C.new.should be_kind_of( Simple2M::Wrapper )

    class Simple2C
      class << self
        remove_method :new
      end
    end

    Simple2C.new.should be_kind_of( Simple2C )

  end

end
