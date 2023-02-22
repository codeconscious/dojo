for num in 0..10 do # Include final item
    print num.to_s + '  '
end; puts

for num in 0...10 do # Excludes final item
    print num.to_s + '  '
end; puts

0.upto(10) { |x| print x.to_s + '  ' }; puts

0.step(10, 2) { |x| print x.to_s + '  ' }; puts
