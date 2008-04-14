describe "ruby" do

  it "should work" do

    module M

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

    class C; include M; end

    C.new.should be_kind_of( M::Wrapper )

    class C
      class << self
        remove_method :new
      end
    end

    C.new.should be_kind_of( C )

  end

end
