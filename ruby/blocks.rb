# SUMMARY: Working with blocks.

# Determine if a block was supplied.
def takeBlock(p1)
    if block_given?
        yield(p1)
    else
        p1
    end
end

puts takeBlock("こんにちは NO block")
puts takeBlock("こんにちは WITH block") { |s| s.sub(/は/, 'わ') }

# If the last param name is prepended with '&', the supplied block is
# converted to a Proc object, which can be called.
def takeBlockAsParam(text, &p1)
    if p1 == nil
        "Expected block is nil." # Perhaps never hit.
    end
    if block_given?
        p1.call(text)
    else
        text + " (No block was given)"
    end
end

puts takeBlockAsParam('Input text')
puts takeBlockAsParam('Input text') { |t| t.upcase }
