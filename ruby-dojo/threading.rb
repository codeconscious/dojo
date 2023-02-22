require 'net/http'

pages = %w[ www.codeconscio.us
            www.github.com
            www.duckduckgo.com ]

threads = []

pages.each do |page|
  threads << Thread.new(page) do |this_page|
    http = Net::HTTP.new(this_page, 80)
    puts "Fetching: #{this_page}"
    resp, = http.get('/', nil) # Discards the data
    puts "Got #{this_page}:  #{resp.message}"
  end
end

threads.each { |thread| thread.join }
