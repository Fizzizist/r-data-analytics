class Element < ApplicationRecord
	require 'csv'
	def self.import(file)
		CSV.foreach(file.path, headers: true).drop(1) do |row|
			Element.create! row.to_hash
			puts row
		end
	end
end
