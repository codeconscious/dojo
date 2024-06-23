# A simple ROT13 exercise to get better acquainted with Ruby syntax.
# Pass in one or more strings to process.

# Represents starting and ending values for character ranges.
# Ex.: Use 97 ('a') and 122 ('z') to represent lowercase letters.
# Due to the simple nature of this current implementation,
# the range must contain an even number of items. (Luckily, the
# English alphabet has an even number of letters.)
class CharRange
    def initialize(starts, ends)
        @starts = starts
        @ends = ends
    end

    def starts
        @starts
    end

    def ends
        @ends
    end

    def itemCount
        @ends - @starts + 1
    end

    def midPoint
        itemCount / 2
    end
end

# Handles ROT13 conversion of strings using provided character ranges.
class RotProcessor
    def initialize(ranges)
        @ranges = ranges
    end

    def convert(text)
        output = ""
        old_char_code = ""
        new_char_code = ""

        text.split('').each do |char|
            old_char_code = char.ord

            # Find a relevant range.
            range = @ranges.select { |r| old_char_code >= r.starts &&
                                         old_char_code <= r.ends }
                           .first

            # ROT process the character if a relevant range was found.
            # Otherwise, simply use the unsupported character as-is.
            if range
                new_char_code = old_char_code + range.midPoint
                if new_char_code > range.ends
                    new_char_code -= range.itemCount
                end
                output += new_char_code.chr
            else
                output += char
            end
        end
        output
    end
end

if ARGV.length == 0
    puts "You must enter at least one string argument."
    return
end

# Set up the supported character types.
number_range = CharRange.new(48, 57)
upper_letter_range = CharRange.new(65, 90)
lower_letter_range = CharRange.new(97, 122)
supported_ranges = [number_range, upper_letter_range, lower_letter_range]

rot_processor = RotProcessor.new(supported_ranges)

ARGV.each do |arg|
    puts " • " + rot_processor.convert(arg)
end
