require 'pp'

class Mangler
  
  def self.mangle_word what
    patterns = []
    index = 0
    patterns << what
    (what.length).times do
      if what.length > 3
        mod = what.clone
        mod[index] = ""
        patterns << mod
      end

      mod = what.clone
      mod[index,0] = "."
      patterns << mod

      if what.length > 3
        mod = what.clone
        mod[index] = "."
        patterns << mod
      end

      index += 1
    end
    patterns = "^((" + patterns.join( ")|(" ) + "))$"
    # pp patterns
    matches = words.grep Regexp.new patterns, Regexp::IGNORECASE
    # pp matches
    matches[rand matches.length]
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
