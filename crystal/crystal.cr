def random_positive_int(ceiling : Int16) : Int16
  ((Random.rand * ceiling).floor + 1).to_i16
end

def greet(names : Array(String), count : Int16)
  names.each do |n|
    puts "こんにちは、#{n}" + "さん" + ("!" * count)
  end
end

greet(["マリオ", "ルイージ", "ピーチ", "キノピオ"], random_positive_int(20))
