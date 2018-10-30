class Replicate < ApplicationRecord
	
	#import replicate data
	def self.import(data)
		#get number of rows in solution_elements and set beginnig row for replicates
		seRows = ActiveRecord::Base.connection.query("SELECT count(*) from solution_elements;") 
		startRow = seRows[0][0] - (data.length-6)

		#declare queries array
		queries = []

		#make queries array
		(2..data.length-4).each do |row|
			cols = data[row].split(",")
			counter = 23
			#puts "THE VALUE IS: " + cols[22] 		#for debugging
			for i in 1..cols[22].to_i
				queries.push("(" + startRow.to_s + ", " + cols[counter] + ")")
				counter += 2
			end
			startRow += 1
		end

		#construct and execute query
		ActiveRecord::Base.connection.execute("INSERT INTO replicate (solution_id, value) values " + queries.join(",") + ";")
	end
end
