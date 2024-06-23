require 'net/http' # Also auto-`require`s `uri`

pages = %w[ www.codeconscio.us
            www.github.com
            www.duckduckgo.com
            www.wikipedia.com
            www.wildly-invalid.co.jp ]

threads = []

pages.each do |page|
  threads << Thread.new(page) do |this_page|
    http = Net::HTTP.new(this_page, 80)
    puts "• Fetching: #{this_page}"
    begin
      resp, = http.get('/', nil) # Discards the data
      puts "✔︎ Got #{this_page}: #{resp.code} (#{resp.message})"
    rescue SocketError
      puts "✖︎ Failed to retrieve #{this_page}"
    end
  end
end

threads.each { |thread| thread.join }
