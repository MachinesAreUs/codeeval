#!/usr/bin/env ruby

MINIMUM_D_TO_BUILDING = 6
DEBUG = false

def solve_segment left_pos, right_pos, min_distance

    puts "left_pos #{left_pos} right_pos #{right_pos} min_distance #{min_distance}" if DEBUG

    count = 1
    distance = left_pos
    puts "count #{count} distance #{distance}" if DEBUG 

    until distance > right_pos - min_distance do
        distance += min_distance
        count += 1 unless distance > right_pos - min_distance
        puts "count #{count} distance #{distance}" if DEBUG
    end
    puts "count #{count}" if DEBUG
    count
end

def solve_bats wire_length, min_distance, hanging_bats

    segments = hanging_bats.inject([[0,MINIMUM_D_TO_BUILDING]]) {|x,y| x << [x[-1][1], y] }
    segments << [segments[-1][1], wire_length - MINIMUM_D_TO_BUILDING] 

    puts "Segments #{segments}" if DEBUG

    segments = segments.drop(1).find_all {|x,y| y-x >= min_distance}

    #puts "Segments #{segments}"

    bats = segments.map {|s| solve_segment s[0], s[1], min_distance}.inject(:+)
    bats = bats + 1 - hanging_bats.length
end

File.open(ARGV[0]).each_line do |line| 
	game = line.split(" ").map {|x| x.to_i}
    puts solve_bats game[0], game[1], game.drop(3)
end


