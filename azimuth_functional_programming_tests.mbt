// Azimuth Functional Programming Tests
// This file contains tests for functional programming concepts and patterns

// Test 1: Pure Functions
test "pure function properties" {
  // Pure function: same input always produces same output
  let add = fn(a : Int, b : Int) { a + b }
  let multiply = fn(a : Int, b : Int) { a * b }
  
  assert_eq(add(2, 3), 5)
  assert_eq(add(2, 3), 5) // Idempotent
  
  assert_eq(multiply(4, 5), 20)
  assert_eq(multiply(4, 5), 20) // Idempotent
  
  // Test commutativity
  assert_eq(add(2, 3), add(3, 2))
  assert_eq(multiply(2, 3), multiply(3, 2))
  
  // Test associativity
  assert_eq(add(add(1, 2), 3), add(1, add(2, 3)))
  assert_eq(multiply(multiply(1, 2), 3), multiply(1, multiply(2, 3)))
}

// Test 2: Function Composition
test "function composition and chaining" {
  let add_one = fn(x : Int) { x + 1 }
  let double = fn(x : Int) { x * 2 }
  let square = fn(x : Int) { x * x }
  
  // Composition function
  let compose = fn(f : (Int) -> Int, g : (Int) -> Int) {
    fn(x : Int) { f(g(x)) }
  }
  
  // Create composed functions
  let add_then_double = compose(double, add_one)
  let double_then_square = compose(square, double)
  let add_one_then_square = compose(square, add_one)
  
  assert_eq(add_then_double(5), 12)  // (5 + 1) * 2
  assert_eq(double_then_square(3), 36)  // (3 * 2)²
  assert_eq(add_one_then_square(4), 25)  // (4 + 1)²
  
  // Test function chaining
  let chain = fn(x : Int, funcs : [(Int) -> Int]) {
    let mut result = x
    for f in funcs {
      result = f(result)
    }
    result
  }
  
  let chained_result = chain(3, [add_one, double, square])
  assert_eq(chained_result, 64)  // ((3 + 1) * 2)²
}

// Test 3: Higher-Order Functions
test "higher-order functions with arrays" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Map function
  let squared = numbers.map(fn(x) { x * x })
  assert_eq(squared, [1, 4, 9, 16, 25, 36, 49, 64, 81, 100])
  
  // Filter function
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4, 6, 8, 10])
  
  let odds = numbers.filter(fn(x) { x % 2 != 0 })
  assert_eq(odds, [1, 3, 5, 7, 9])
  
  // Reduce function
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 55)
  
  let product = numbers.reduce(fn(acc, x) { acc * x }, 1)
  assert_eq(product, 3628800)
  
  // Find function
  let find_even = numbers.find(fn(x) { x % 2 == 0 })
  match find_even {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  let find_large = numbers.find(fn(x) { x > 10 })
  match find_large {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Currying
test "currying and partial application" {
  // Curried add function
  let curried_add = fn(a : Int) {
    fn(b : Int) {
      a + b
    }
  }
  
  let add_5 = curried_add(5)
  assert_eq(add_5(3), 8)
  assert_eq(add_5(10), 15)
  
  // Curried multiply function
  let curried_multiply = fn(a : Int) {
    fn(b : Int) {
      fn(c : Int) {
        a * b * c
      }
    }
  }
  
  let multiply_by_2 = curried_multiply(2)
  let multiply_by_2_and_3 = multiply_by_2(3)
  assert_eq(multiply_by_2_and_3(4), 24)
  
  // Partial application
  let partial_apply = fn(f : (Int, Int) -> Int, a : Int) {
    fn(b : Int) { f(a, b) }
  }
  
  let add = fn(a : Int, b : Int) { a + b }
  let add_10 = partial_apply(add, 10)
  assert_eq(add_10(5), 15)
}

// Test 5: Immutable Data Structures
test "immutable operations" {
  // Original array remains unchanged
  let original = [1, 2, 3]
  
  // Create new array with added element
  let with_new = original.push(4)
  assert_eq(original, [1, 2, 3])  // Original unchanged
  assert_eq(with_new, [1, 2, 3, 4])
  
  // Create new array with mapped values
  let mapped = original.map(fn(x) { x * 2 })
  assert_eq(original, [1, 2, 3])  // Original unchanged
  assert_eq(mapped, [2, 4, 6])
  
  // Create new array with filtered values
  let filtered = original.filter(fn(x) { x > 1 })
  assert_eq(original, [1, 2, 3])  // Original unchanged
  assert_eq(filtered, [2, 3])
  
  // Test immutable record operations
  type Person {
    name : String
    age : Int
  }
  
  let person = { name: "Alice", age: 30 }
  let older_person = { name: person.name, age: person.age + 1 }
  
  assert_eq(person.age, 30)  // Original unchanged
  assert_eq(older_person.age, 31)
}

// Test 6: Recursion and Tail Recursion
test "recursive functions and tail recursion" {
  // Regular recursive factorial
  let factorial = fn(n : Int) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  
  // Tail recursive factorial
  let factorial_tail = fn(n : Int) {
    let helper = fn(n : Int, acc : Int) {
      if n <= 1 {
        acc
      } else {
        helper(n - 1, n * acc)
      }
    }
    helper(n, 1)
  }
  
  assert_eq(factorial_tail(0), 1)
  assert_eq(factorial_tail(1), 1)
  assert_eq(factorial_tail(5), 120)
  
  // Recursive Fibonacci
  let fibonacci = fn(n : Int) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
}

// Test 7: Lazy Evaluation
test "lazy evaluation patterns" {
  // Simulate lazy evaluation with closures
  let lazy_value = fn() {
    // Expensive computation
    let mut result = 0
    for i in 0..=1000 {
      result = result + i
    }
    result
  }
  
  // Value not computed until called
  let computed = lazy_value()
  assert_eq(computed, 500500)
  
  // Lazy infinite sequence simulation
  let make_sequence = fn(start : Int) {
    let mut current = start
    fn() {
      let result = current
      current = current + 1
      result
    }
  }
  
  let sequence = make_sequence(10)
  assert_eq(sequence(), 10)
  assert_eq(sequence(), 11)
  assert_eq(sequence(), 12)
  
  // Lazy filtering
  let lazy_filter = fn(arr : [Int], predicate : (Int) -> Bool) {
    let mut index = 0
    fn() {
      while index < arr.length() {
        let value = arr[index]
        index = index + 1
        if predicate(value) {
          return Some(value)
        }
      }
      None
    }
  }
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let even_filter = lazy_filter(numbers, fn(x) { x % 2 == 0 })
  
  match even_filter() {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  match even_filter() {
    Some(value) => assert_eq(value, 4)
    None => assert_true(false)
  }
}

// Test 8: Function Memoization
test "function memoization" {
  // Create memoized function
  let memoize = fn(f : (Int) -> Int) {
    let cache = []
    fn(input : Int) {
      // Check if value is in cache
      let cached = cache.find(fn(pair) { pair.0 == input })
      match cached {
        Some((_, value)) => value
        None => {
          let result = f(input)
          cache.push((input, result))
          result
        }
      }
    }
  }
  
  // Expensive function
  let expensive = fn(n : Int) {
    let mut sum = 0
    for i in 0..=n {
      sum = sum + i
    }
    sum
  }
  
  let memoized_expensive = memoize(expensive)
  
  // First call computes value
  let result1 = memoized_expensive(100)
  assert_eq(result1, 5050)
  
  // Second call uses cached value
  let result2 = memoized_expensive(100)
  assert_eq(result2, 5050)
  
  // Different input computes new value
  let result3 = memoized_expensive(50)
  assert_eq(result3, 1275)
}

// Test 9: Monads and Functors
test "monad and functor patterns" {
  // Option as a functor
  let option_map = fn(opt : Option[Int], f : (Int) -> Int) {
    match opt {
      Some(value) => Some(f(value))
      None => None
    }
  }
  
  let some_value = Some(5)
  let none_value = None
  
  let mapped_some = option_map(some_value, fn(x) { x * 2 })
  match mapped_some {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  let mapped_none = option_map(none_value, fn(x) { x * 2 })
  match mapped_none {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Option as a monad (bind/flatMap)
  let option_bind = fn(opt : Option[Int], f : (Int) -> Option[Int]) {
    match opt {
      Some(value) => f(value)
      None => None
    }
  }
  
  let safe_divide = fn(a : Int, b : Int) {
    if b != 0 {
      Some(a / b)
    } else {
      None
    }
  }
  
  let result = option_bind(Some(10), fn(x) { safe_divide(x, 2) })
  match result {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  let division_by_zero = option_bind(Some(10), fn(x) { safe_divide(x, 0) })
  match division_by_zero {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Function Pointers and Closures
test "function pointers and closure behavior" {
  // Function that returns a function (closure)
  let make_adder = fn(n : Int) {
    fn(x : Int) { x + n }
  }
  
  let add_3 = make_adder(3)
  let add_7 = make_adder(7)
  
  assert_eq(add_3(10), 13)
  assert_eq(add_7(10), 17)
  
  // Closure capturing environment
  let make_multiplier = fn() {
    let mut counter = 0
    fn(x : Int) {
      counter = counter + 1
      x * counter
    }
  }
  
  let multiplier = make_multiplier()
  assert_eq(multiplier(5), 5)   // 5 * 1
  assert_eq(multiplier(5), 10)  // 5 * 2
  assert_eq(multiplier(5), 15)  // 5 * 3
  
  // Function as parameter
  let apply_twice = fn(f : (Int) -> Int, x : Int) {
    f(f(x))
  }
  
  let double = fn(x : Int) { x * 2 }
  assert_eq(apply_twice(double, 3), 12)  // (3 * 2) * 2
  
  let increment = fn(x : Int) { x + 1 }
  assert_eq(apply_twice(increment, 5), 7)  // (5 + 1) + 1
  
  // Array of functions
  let operations = [
    fn(x) { x + 1 },
    fn(x) { x * 2 },
    fn(x) { x * x }
  ]
  
  let apply_operations = fn(x : Int, ops : [(Int) -> Int]) {
    let mut result = x
    for op in ops {
      result = op(result)
    }
    result
  }
  
  assert_eq(apply_operations(3, operations), 25)  // ((3 + 1) * 2)²
}