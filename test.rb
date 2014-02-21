Just = Struct.new(:val)
p Just.new("hello")

def add(a, b)
  a + b
end

p add(2, 5)

p add(2, add(5, 3))