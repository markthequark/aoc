import Config

config :logger, level: :debug

if config_env() == :prod do
  config :advent_of_code, input_file: :non_test
else
  config :advent_of_code, input_file: :test
end
