- Run against the entire sinatra codebase and compare input to output files. Nothing should change...Salmon should leave regular ruby totally as-is.

- write specs for string interpolation, bet that's a bad corner case

- support in vim

# CURRENT BUGS:
- can't pass somthing complicated like this into a function:

    sum(.chomp <$> File.readlines(file))

- Basically, can't handle nested parenthesis very well right now because one of the parsers for parseLine is greedy

- Support multi-line strings with %{} etc
