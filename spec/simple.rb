describe "ruby" do

  it "should work" do

    module M

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

    class C; include M; end

    C.new.should be_kind_of( M::Wrapper )

  end

end
