// Azimuth Type System and Functional Programming Tests
// This file contains test cases focusing on MoonBit's type system and functional programming features

// Test 1: Advanced Type System Features
test "advanced type system features" {
  // Test type inference
  let inferred = 42
  assert_eq(inferred, 42)
  
  // Test generic function
  let identity = fn[T](x: T) -> T { x }
  assert_eq(identity(5), 5)
  assert_eq(identity("hello"), "hello")
  assert_eq(identity(true), true)
  
  // Test type constraints
  let compare = fn[T: Compare](a: T, b: T) -> Bool {
    a < b
  }
  assert_true(compare(3, 5))
  assert_false(compare(10, 2))
}

// Test 2: Functional Data Structures
test "functional data structures" {
  // Immutable list operations
  let list = [1, 2, 3, 4, 5]
  let reversed = list.reverse()
  assert_eq(reversed, [5, 4, 3, 2, 1])
  
  // Functional map
  let map = Map::empty()
    .insert("key1", 100)
    .insert("key2", 200)
    .insert("key3", 300)
  
  assert_eq(map.get("key1"), Some(100))
  assert_eq(map.get("key2"), Some(200))
  assert_eq(map.get("key4"), None)
  
  let updated_map = map.insert("key1", 150)
  assert_eq(map.get("key1"), Some(100)) // Original unchanged
  assert_eq(updated_map.get("key1"), Some(150)) // Updated version
}

// Test 3: Monadic Operations
test "monadic operations with Option type" {
  let safe_divide = fn(a: Int, b: Int) -> Option[Int] {
    if b == 0 { None } else { Some(a / b) }
  }
  
  let chain_divide = fn(a: Int, b: Int, c: Int) -> Option[Int] {
    safe_divide(a, b).flat_map(fn(x) { safe_divide(x, c) })
  }
  
  assert_eq(chain_divide(100, 5, 2), Some(10))
  assert_eq(chain_divide(100, 0, 2), None)
  assert_eq(chain_divide(100, 5, 0), None)
  
  // Test monadic composition
  let result = Some(42)
    .map(fn(x) { x * 2 })
    .filter(fn(x) { x > 50 })
    .map(fn(x) { x + 8 })
  
  assert_eq(result, Some(92))
}

// Test 4: Algebraic Data Types
test "algebraic data types and pattern matching" {
  enum Tree[T] {
    Empty
    Leaf(T)
    Node(Tree[T], T, Tree[T])
  }
  
  let tree_sum = fn[T: Numeric](tree: Tree[T]) -> T {
    match tree {
      Tree::Empty => 0
      Tree::Leaf(value) => value
      Tree::Node(left, value, right) => tree_sum(left) + value + tree_sum(right)
    }
  }
  
  let tree = Tree::Node(
    Tree::Leaf(10),
    20,
    Tree::Node(
      Tree::Empty,
      30,
      Tree::Leaf(40)
    )
  )
  
  assert_eq(tree_sum(tree), 100)
}

// Test 5: Higher-Order Functions and Currying
test "higher-order functions and currying" {
  // Curried function
  let add = fn(a: Int) -> (Int -> Int) {
    fn(b: Int) -> Int { a + b }
  }
  
  let add_5 = add(5)
  assert_eq(add_5(10), 15)
  assert_eq(add_5(20), 25)
  
  // Function composition
  let compose = fn[A, B, C](f: B -> C, g: A -> B) -> (A -> C) {
    fn(x: A) -> C { f(g(x)) }
  }
  
  let square = fn(x: Int) -> Int { x * x }
  let increment = fn(x: Int) -> Int { x + 1 }
  
  let square_then_increment = compose(increment, square)
  let increment_then_square = compose(square, increment)
  
  assert_eq(square_then_increment(5), 26) // (5^2) + 1
  assert_eq(increment_then_square(5), 36) // (5 + 1)^2
}

// Test 6: Lazy Evaluation and Memoization
test "lazy evaluation and memoization" {
  // Simple memoization
  let memoize = fn[A: Hash + Eq, B](f: A -> B) -> (A -> B) {
    let cache = Map::empty()
    fn(x: A) -> B {
      match cache.get(x) {
        Some(result) => result
        None => {
          let result = f(x)
          cache.insert(x, result)
          result
        }
      }
    }
  }
  
  let expensive_fibonacci = fn(n: Int) -> Int {
    if n <= 1 { n } else {
      expensive_fibonacci(n - 1) + expensive_fibonacci(n - 2)
    }
  }
  
  let memoized_fibonacci = memoize(expensive_fibonacci)
  
  // Test memoization works
  assert_eq(memoized_fibonacci(10), 55)
  assert_eq(memoized_fibonacci(15), 610)
}

// Test 7: Type Classes and Traits
test "type classes and traits" {
  trait Show {
    show(Self) -> String
  }
  
  impl Show for Int {
    show(self) -> String { self.to_string() }
  }
  
  impl Show for String {
    show(self) -> String { self }
  }
  
  impl Show for Bool {
    show(self) -> String { if self { "true" } else { "false" } }
  }
  
  let generic_show = fn[T: Show](value: T) -> String {
    value.show()
  }
  
  assert_eq(generic_show(42), "42")
  assert_eq(generic_show("hello"), "hello")
  assert_eq(generic_show(true), "true")
}

// Test 8: Functional Error Handling
test "functional error handling with Result type" {
  let safe_sqrt = fn(x: Double) -> Result[Double, String] {
    if x < 0.0 {
      Err("Cannot calculate square root of negative number")
    } else {
      Ok(x.sqrt())
    }
  }
  
  let safe_divide = fn(a: Double, b: Double) -> Result[Double, String] {
    if b == 0.0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // Test error chaining with monadic operations
  let calculate = fn(x: Double, y: Double, z: Double) -> Result[Double, String] {
    safe_sqrt(x)
      .flat_map(fn(sqrt_x) { safe_divide(sqrt_x, y) })
      .map(fn(result) { result * z })
  }
  
  assert_eq(calculate(16.0, 2.0, 3.0), Ok(12.0))
  assert_eq(calculate(-4.0, 2.0, 3.0), Err("Cannot calculate square root of negative number"))
  assert_eq(calculate(16.0, 0.0, 3.0), Err("Division by zero"))
}

// Test 9: Recursion and Tail Call Optimization
test "recursion and tail call optimization" {
  // Tail-recursive factorial
  let factorial_tail = fn(n: Int) -> Int {
    let aux = fn(n: Int, acc: Int) -> Int {
      if n <= 1 { acc } else { aux(n - 1, acc * n) }
    }
    aux(n, 1)
  }
  
  assert_eq(factorial_tail(0), 1)
  assert_eq(factorial_tail(1), 1)
  assert_eq(factorial_tail(5), 120)
  assert_eq(factorial_tail(10), 3628800)
  
  // Recursive list processing
  let sum_list = fn(list: List[Int]) -> Int {
    match list {
      [] => 0
      [head, ..tail] => head + sum_list(tail)
    }
  }
  
  assert_eq(sum_list([]), 0)
  assert_eq(sum_list([1, 2, 3, 4, 5]), 15)
  assert_eq(sum_list([10, 20, 30]), 60)
}

// Test 10: Functional Pipeline Operations
test "functional pipeline operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Complex pipeline transformation
  let result = numbers
    .filter(fn(x) { x % 2 == 0 }) // Even numbers only
    .map(fn(x) { x * x }) // Square them
    .filter(fn(x) { x > 20 }) // Keep squares greater than 20
    .map(fn(x) { x / 2 }) // Divide by 2
    .reduce(fn(acc, x) { acc + x }, 0) // Sum them up
  
  assert_eq(result, 72) // (36 + 64 + 100) / 2 = 100
  
  // Pipeline with custom types
  enum Status {
    Pending
    Processing
    Completed
    Failed
  }
  
  type Task = { id: Int, status: Status, priority: Int }
  
  let tasks = [
    { id: 1, status: Status::Pending, priority: 3 },
    { id: 2, status: Status::Completed, priority: 1 },
    { id: 3, status: Status::Processing, priority: 2 },
    { id: 4, status: Status::Pending, priority: 1 },
    { id: 5, status: Status::Failed, priority: 2 }
  ]
  
  let high_priority_pending = tasks
    .filter(fn(task) { task.status == Status::Pending })
    .filter(fn(task) { task.priority <= 2 })
    .map(fn(task) { task.id })
    .sort()
  
  assert_eq(high_priority_pending, [4])
}