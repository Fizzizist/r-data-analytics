class SessionsController < ApplicationController

	#main page controller
	def index
		@sessions = Session.all
	end

	#sessions/new
	def new
	end

	#create function called from new form above
	def create
		#inserts session data into sessions table
		Session.make()
		
		#read file from form and grab data from it
		file = File.open(params[:file].path, "r")
		data = file.readlines
		file.close

		#insert file data into database
		sessID = Solution.import(data)
		SolutionElement.import(data)
		Replicate.import(data)
		redirect_to '/sessions/' + sessID[0][0].to_s
	end

	#a page for every session
	def show
		@session = Session.find(params[:id])
		@solutions = Solution.all
		@num = params[:id]
	end

end
