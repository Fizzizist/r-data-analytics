class Solution < ApplicationRecord
	def self.import(data)
		#get latest session_id from sessions table
		sessID = ActiveRecord::Base.connection.query("SELECT max(session_id) from sessions;")
		queries = []
		(2..data.length-4).each do |row|
			cols = data[row].split(",")
			queries.push("('" + cols[0] + "', '" + cols[4] + "', " + cols[19] + ", " + cols[20] + ", " + cols[21] + ", " + sessID[0][0].to_s + ")")
		end
		ActiveRecord::Base.connection.execute("INSERT INTO solutions (label, type_of, act_wgt, act_vol, DF, session_id) values " + queries.join(",") + ";")
		return sessID
	end
end
