# AdventOfCode

Elixir solutions to the [Advent of Code 2023](https://adventofcode.com/2023/)

[![test 2023](https://github.com/markthequark/aoc/actions/workflows/test_2023.yml/badge.svg)](https://github.com/markthequark/aoc/actions/workflows/test_2023.yml)

Build
---

    $ make build

Test
---

    $ make test

Run
---

    $ make run <day number>

Generate files for new day
---
    $ make new_day

Environment Variables Configuration
---
This project uses environment variables for some configuration settings. To facilitate the setup process, we use two files: `.env` and `.env.example`

The `.env` file is used to store environment variables required for the application to run locally. This file can contain sensitive information and is not committed to the repo.

The `.env.example` file serves as a template that lists all the environment variables required by the application. It contains placeholder or default values for each variable.

Before starting development, duplicate the `.env.example` file and rename it to `.env`. Replace the placeholder values with your actual configuration settings.

    $ cp .env.example .env
