# rubocop:disable Style/Documentation
# frozen_string_literal: true

def ensure_equal(expected, actual)
  puts "NOT EQUAL! Expected: #{expected} / Actual: #{actual}" if actual != expected
end

class Easy
  class << self
    def one
      input = %w[this is an array]
      expected = [4, 2, 2, 5]
      actual = input.map(&:length)
      ensure_equal actual, expected
    end

    def five
      input = 'abccba'
      expected = true
      actual = input.reverse == input
      ensure_equal actual, expected
    end

    def nine
      input = %w[fynn nyfn]
      expected = true
      actual = input.map { |word| word.chars.sort }.uniq.length == 1
      ensure_equal actual, expected
    end
  end
end

class Medium
  class << self
    def eight
      input = [[3, 4, 1, 2], [9, 4, 8, 2]]
      expected = 70 # Abbreviated output, eliding the text output in the original problem.
      actual = input.map(&:sort).transpose.map { |x| x.reduce(:*) }.sum
      ensure_equal expected, actual
    end
  end
end

Easy.one
Easy.five
Easy.nine
Medium.eight

# rubocop:enable Style/Documentation
