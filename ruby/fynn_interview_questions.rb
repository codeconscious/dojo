# rubocop:disable Style/Documentation
# frozen_string_literal: true

def ensure_equal(expected, actual)
  puts "NOT EQUAL! Expected: #{expected} / Actual: #{actual}" if actual != expected
end

module Medium
  module Eight
    def self.run
      input = [[3, 4, 1, 2], [9, 4, 8, 2]]
      expected = 70 # Abbreviated output, eliding the text output in the original problem.
      actual = input.map(&:sort).transpose.map { |x| x.reduce(:*) }.sum
      ensure_equal expected, actual
    end
  end
end

Medium::Eight.run

# rubocop:enable Style/Documentation
