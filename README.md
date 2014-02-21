# Salmon

A transpiler to Ruby that adds some nice syntax.
Some features are only compatible with Ruby 1.9+.

Current status: not usable yet. Pull requests welcome!

## Spec

### Simple classes - DONE

```ruby
data Just val
```

becomes

```ruby
class Just

  attr_accessor :val
  def initialize val
    @val = val
  end

end
```

### Single-line functions - DONE

```
add a b = a + b
```

becomes

```ruby
def add a, b
  a + b
end
```

### ...with pattern matching

```
factorial 1 = 1
factorial n = n * factorial(n - 1)
```

becomes

```ruby
def factorial n
  if n == 1
    1
  else
    n * factorial(n - 1)
  end
end
```

### Function composition

```
JSON.parse . File.read . File.basename $ file
```

becomes

```ruby
JSON.parse(File.read(File.basename(file)))
```

TODO how does this look in a block?

```
filenames.map(JSON.parse . File.read . File.basename)
# or
filenames.map(&(JSON.parse . File.read . File.basename))
```

??

### Currying

```
incr = add(1, _)
```

becomes

```ruby
def incr b
  add(1, b)
end
```

But anonymous functions are lambdas depending on the context:

```
add(1, _) $ 12
```

becomes

```
(->(b) { add(1, b) })[12]
```

Or you could also do:

(1+_) $ 12

Or:

```
(1..10).map(&add(1, _))
```

would work as a lambda:

```ruby
(1..10).map(&(->(b) { add(1, b) }))
```

But we want it to be a little more readable, so it gets written as a block instead:

```ruby
(1..10).map { |b| add(1, b) }
```

### Infix function names


```
1 `add` 2 # DONE
```

becomes

```ruby
add(1, 2)
```

And you can curry like this too:

```
incr = 1 `add` _
```

becomes

```ruby
def incr b
  add(1, b)
end
```

### Defining operators - DONE

You can define your own operators. For example:

```
op (<|>) either
```

Now

```
a <|> b
```

becomes

```ruby
either(a, b)
```

### fmap

```
(1+_) <$> (1..10)
```

is the same as

```
(1..10).fmap(&(1+_))
```

Which is the same as:

```ruby
(1..10).map { |b| 1 + b }
```

This is because more objects are encouraged to be mappable (i.e. functors).


For example, here's `Maybe` as a functor:

```ruby
data Maybe val

class Maybe
  def map(&blk)
    if val
      blk.call(val)
    else
      nil
    end
  end
end
```

Then you can use it:

```
JSON.parse . File.read . File.basename <$> Maybe file
```

becomes:

```ruby
Maybe.new(file).map { |x| JSON.parse(File.read(File.basename(x))) }
```
