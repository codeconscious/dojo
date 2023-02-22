class StringTooLongException < RuntimeError
    attr :okToRetry
    def initialize(okToRetry)
      @okToRetry = okToRetry
    end
  end

begin
    # puts "例の数字は" + 16
    text = "大した内容はないんだけど…"
    raise StringTooLongException.new(true), "This string is too long!" if text.length > 5
rescue TypeError => typeError
    puts "Sorry, there was a type error: " + typeError.to_s
rescue StringTooLongException => exception
    print "STRING ERROR: " + exception.to_s
    puts exception.okToRetry ? ", but CAN retry" : "and CANNOT retry"
rescue => exception
    puts "Sorry, an unexpected exception occurred: " + exception.to_s
else
    puts "Sweet, no errors!"
ensure
    puts "All done."
end
