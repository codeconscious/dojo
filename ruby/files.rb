# encoding: UTF-8

require 'mp3info'

def summarize(mp3)
    if mp3.tag2.TPE1 && mp3.tag2.TIT2
        "#{mp3.tag2.TPE1} / #{mp3.tag2.TIT2} (Encoding: #{mp3.tag2.TIT2.encoding})"
        # mp3.tag.title
    elsif mp3.tag2.TPE1
        mp3.tag2.TPE1
    elsif mp3.tag2.TIT2
        mp3.tag2.TIT2
    else
        'Unknown Track Data'
    end
end

def process_mp3(mp3)
    Mp3Info.open(mp3) do |mp3|
        # if mp3.hastag1?
        #     puts '- ID3v1の情報あり'
        # end
        if mp3.hastag2?
            # puts '- ID3v2の情報あり'
            puts '> ' + summarize(mp3)
            mp3.tag2.each do |key, val|
                if key == "APIC"
                    puts '• APIC: Album art found'
                else
                    puts "• #{key.dup.to_s.force_encoding("UTF-8")}: #{val.dup.to_s.force_encoding("UTF-8")}"
                end
            end
        end
    end
end

ARGV.each do |arg|
    if File.directory?(arg)
        # Dir.entries.each { |mp3| process_mp3(mp3) }
        Dir.glob("#{arg}/*")
            .select{ |file| /.+\.mp3/.match file }
            .each { |mp3| process_mp3(mp3) }
    else # File
        puts "Path: \"#{arg}\""
        process_mp3(arg)
    end
end
