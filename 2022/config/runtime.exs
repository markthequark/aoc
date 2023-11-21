import Config
import Dotenvy

source!([".env", System.get_env()])

config :advent_of_code, aoc_session_cookie: env!("AOC_SESSION_COOKIE", :string!)
