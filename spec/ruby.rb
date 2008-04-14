require File.join( File.dirname(__FILE__), '/spec_helper.rb' )

describe "ruby" do

  it "should include the way expected" do

    class O
      def initialize o
      end
    end

    module M

      module Class
        def new
          warn "here"
          o = super
          O.new o
        end
      end

      def self.included cls
        # cls.extend Class
        class << cls
          def new
            warn "there"
            o = super
            O.new o
        end
        end
      end

    end

    class C
      include M;
    end

    c = C.new

    C.new.should be_kind_of( O )

    class C
      if false
        class << self
          remove_method :new
        end
      else
        def self.new
          super
        end
      end
    end

    C.new.should be_kind_of( C )

  end

end
