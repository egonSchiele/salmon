data Maybe = Nothing | Just val
p Just "hello"

class Just
  def run
    if val % 2 == 0
      p val
      self
    else
      nil
    end
  end

  def run_with obj
    run if obj
  end
end

# simple interpolation
# join (+) do
#   2
#   3
#   4
#   5
# end

# join (,) do
#   "hello"
#   "there"
#   "good"
#   "sir"
# end

# # each object on each line gets a function called on it, and the result
# # is passed in to the object on the next line. Eg:

run do
  Just 2
  Just 4
  Just 5
end

# # becomes:

# baz.run_with(bar.run_with(foo.run))

# # function composition, on multiple lines
# call do
#   JSON.parse
#   File.read
#   File.basename
#   file
# end

# # send multiple methods to an object
# tmp\
#   write
#   flush(contents)
