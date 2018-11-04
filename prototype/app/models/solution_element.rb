class SolutionElement < ApplicationRecord
	#require 'csv'
        def self.import(data)
		#array to store query info
		queries = []
		
		#loop to put data into query format
                (2..data.length-4).each do |row|
                        cols = data[row].split(",")
                        queries.push("('" + cols[5][0..1] + "', " + cols[7] + ", '" + cols[14] + "', " + cols[11] + ", " + cols[15] + ", " + cols[16] + ", " + cols[17] + ", " + cols[18] + ", '" + cols[6] + "')")	
                end

		#one big SQL query
                ActiveRecord::Base.connection.execute("INSERT INTO solution_elements (element_id, solid_conc, time, inte, SD, RSD, intSD, intRSD, flag) values " + queries.join(",") + ";")
        end

	def self.getKLabelData(num)
		result = ActiveRecord::Base.connection.query("SELECT solutions.label, sum(solution_elements.solid_conc) from solution_elements, solutions where solution_elements.solution_id = solutions.solution_id and element_id = 'K' and solutions.solution_id >= (select min(solution_id) from solutions where session_id = "+num+") and solutions.solution_id <= (select max(solution_id) from solutions where session_id = "+num+") group by solutions.label")
		return result
	end

end
