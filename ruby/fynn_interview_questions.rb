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

    def ten
      input = 'Aloha! My name is Fynn.'
      expected = 6
      actual = input.downcase.chars.filter { |ch| %w[a e i o u].include? ch }.length
      ensure_equal actual, expected
    end

    def twelve
      input = 'hello my name is fynn and this is kind of funny. Is this real?'
      expected = 'is'
      actual = input.downcase.split(' ').tally.max_by { |_, v| v }.first
      ensure_equal actual, expected
    end

    def sixteen
      input = 'Hello my name is Fynn!!'
      expected = 'Hello'
      is_valid_char = ->(ch) { ch.match?(/[a-zA-Z ]/) }
      actual = input.chars.filter(&is_valid_char).join.split(' ').group_by(&:length).max[1][0]
      ensure_equal actual, expected
    end

    def seventeen
      input = 4
      expected = 10
      actual = Array(1..input).reduce(:+)
      ensure_equal actual, expected
    end

    def eighteen
      input = 'Hello my name is Fynn'
      expected = 'helo mynaisf'
      actual = input.downcase.chars.to_set.join
      ensure_equal actual, expected
    end

    def twenty_two
      input = 4.step(1, -1)
      expected = 24
      actual = input.reduce(:*)
      ensure_equal actual, expected
    end
  end
end

class Medium
  class << self
    def one
      input = 10
      expected = 55

      # I admittedly got some help for this one as I was unsure how to set up a recursive lambda...
      fibonacci = ->(n) { ->(x) { x < 2 ? x : fibonacci.call(x - 1) + fibonacci.call(x - 2) }.call(n) }

      actual = fibonacci.call input
      ensure_equal actual, expected
    end

    def three
      input = 'Hello my name is Fynn'
      expected = 'olleH ym eman si nnyF'
      actual = input.split(' ').map(&:reverse).join(' ')
      ensure_equal actual, expected
    end

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
Easy.ten
Easy.twelve
Easy.sixteen
Easy.seventeen
Easy.eighteen
Easy.twenty_two
Medium.one
Medium.three
Medium.eight

# rubocop:enable Style/Documentation
