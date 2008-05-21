require 'pp'

class Mangler
  
  def self.mangle_word what
    patterns = [ what, what+"." ]
    index = 0
    (what.length).times do

      # delete a character
      if what.length > 3
        mod = what.clone
        mod[index] = ""
        patterns << mod
      end

      # insert a character
      mod = what.clone
      mod[index,0] = "."
      patterns << mod

      #change a character (biased towards making words longer)
      if what.length > 3
        mod = what.clone
        mod[index] = "."
        patterns << mod
      end

      index += 1
    end
    patterns = "^((" + patterns.join( ")|(" ) + "))$"
    # pp patterns
    matches = words.grep Regexp.new( patterns, Regexp::IGNORECASE )
    # pp matches
    matches[rand( matches.length )]
  end

  def self.mangle words
    words.split.map { |word| mangle_word word }.join(" ")
  end

  private

  def self.words
    @words ||=
      begin
        lines = IO.readlines File.join( File.dirname(__FILE__), "3esl.txt")
        lines.each { |line| line.chomp! } 
        lines
      end
  end

end
