class Session < ApplicationRecord
	def self.make
		Session.create(:date => Time.now)
	end

end
