# A practice file to get acquainted with Ruby syntax.
$LOAD_PATH << '.'
require 'person.rb'

puts 'こんにちは!'

# Prints a description of a given item, then optionally prints the items of an array of an arbitrary length.
def describe_item(itemName, itemNumericValue = 0, *others)
    # TODO: Add simple guard clauses.
    printf("The approximate value of %s is %1.4f.\n", itemName.upcase(), itemNumericValue)
    if others.length > 0 # Parentheses are optional.
        puts others.join("; ")
    end
end

piValue = 3.14159265359
piName = 'pi'
describe_item(piName, piValue)
describe_item piName, piValue, %w(Scott Jean Henry Warren Robert)

# Print list of prefecture match-ups
prefectures = %w(北海道 長崎県 沖縄県 東京都)
get_matchup = -> (prefecture1, prefecture2, index) {
    # Example: "2. 愛知県 vs. 佐賀県"
    return "#{index.to_s.rjust(2, ' ')}. #{prefecture1} vs. #{prefecture2}"
}
puts 'Prefectures: ' + prefectures.join(' • ')
prefectures.product(prefectures)
           .reject { |p1, p2| p1 == p2 } # Disallow same-prefecture match-ups
           .each_with_index do |(p1, p2), index|
                puts get_matchup.call(p1, p2, index + 1)
           end

# Lambdas
numbers = (1..200)
is_multiple_of_10 = -> (n) { return n % 10 == 0 }
multiplesOf10 = numbers.select { |n| is_multiple_of_10.call(n) } # Or .reject with a negative condition
puts 'Multiples of ten: ' + multiplesOf10.join(', ')

# Named variables
heroes = [
    Person.new(name: 'Scott Summers', supername: 'Cyclops', powers: 'concussive optic blasts', team: 'the X-Men'),
    Person.new(team: 'the X-Men', name: 'Henry McCoy', supername: 'Beast', powers: 'enhanced strength and agility'),
    Person.new(supername: 'Mr. Fantastic', name: 'Reed Richards', powers: 'elasticity', team: 'the Fantastic Four'),
    Person.new(supername: 'Captain America', name: 'Steven Rogers', powers: 'peak human performance', team: 'the Avengers')
]
heroes.each { |hero| puts '- ' + hero.describe() }
heroes[0].team = "X-Factor"
puts "UPDATE: " + heroes[0].describe
# puts "That was a summary featuring #{heroes.m}."
puts "There are #{Person.count} people registered."

# Double splat operator
# def introducer(name:, age:) = "#{name} is #{age} years old"
def introducer(name:, age:)
    "#{name} is #{age} years old."
end
apocalypse = { age: 5000, name: 'En Sabah Nur' }
captain_america = { age: 150, name: 'Steven Rogers' }
long_lived_people = [ apocalypse, captain_america ]
long_lived_people.each do |person|
    puts introducer(**person)
end

# defined?
puts defined? 42.abs
puts defined? "何じゃ？"
