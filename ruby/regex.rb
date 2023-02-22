text = 'おい、何をしてやがる？！'
foundAt = text =~ /やがる/
puts foundAt ? "#{foundAt+1}個目からの3文字が失礼ですぞ。" : '問題はありませんでした。'

text = "あなたを必ず助け出しますから！"

puts text =~ /を/ ? "OK" : "NO"
puts text =~ /ヲ/ ? "OK" : "NO"
%w(を ヲ).each { |ch| puts /#{ch}/ ? "OK!" : "NO!" }

# Use regex pairs as a range!
file = File.open("都道府県.txt")
while file.gets
    print if /東京都/../愛知県/
end
