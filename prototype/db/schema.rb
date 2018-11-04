# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 0) do

  create_table "element", primary_key: "element_id", id: :string, limit: 3, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
    t.string "name", limit: 25
    t.float "mol_wgt"
  end

  create_table "replicate", primary_key: "replicate_id", id: :integer, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
    t.integer "solution_id", null: false
    t.float "value", null: false
  end

  create_table "session", primary_key: "session_id", id: :integer, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
    t.date "date", null: false
    t.binary "prodecure"
  end

  create_table "session_element", primary_key: ["session_id", "element_id"], options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
    t.integer "session_id", null: false
    t.string "element_id", limit: 3, null: false
    t.float "ppm"
  end

  create_table "solution", primary_key: "solution_id", id: :integer, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
    t.string "label", limit: 25, null: false
    t.string "type", limit: 25
    t.integer "act_wgt", null: false
    t.integer "act_vol", null: false
    t.integer "DF", null: false
  end

  create_table "solution_element", primary_key: ["solution_id", "element_id"], options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
 ct   t.integer "solution_id", null: false
    t.string "element_id", limit: 3, null: false
    t.float "solid_conc", null: false
    t.time "time"
    t.float "inte", null: false
    t.float "SD", null: false
    t.float "RSD", null: false
    t.float "intSD", null: false
    t.float "intRSD", null: false
    t.string "flag", limit: 5
  end

end
