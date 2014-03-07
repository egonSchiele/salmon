- Run against the entire sinatra codebase and compare input to output files. Nothing should change...Salmon should leave regular ruby totally as-is.

- pattern matching

- write specs for string interpolation, bet that's a bad corner case

- support in vim

# CURRENT BUGS:
- adding a space for every whitespace is not good enough...it doesn't handle indentation very well! Fix this.
- can't pass somthing complicated like this into a function:

    sum(.chomp <$> File.readlines(file))
