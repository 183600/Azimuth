// Azimuth Telemetry System - MoonBit Advanced Features Tests
// This file contains comprehensive test cases for advanced MoonBit language features

// Test 1: Advanced Pattern Matching
test "advanced pattern matching with nested patterns" {
  let data = Some((1, "test", true))
  
  match data {
    Some((num, str, bool_val)) => {
      assert_eq(num, 1)
      assert_eq(str, "test")
      assert_true(bool_val)
    }
    None => assert_true(false)
  }
  
  // Test with enum pattern matching
  enum Status {
    Active(Int)
    Inactive(String)
    Pending
  }
  
  let status1 = Active(42)
  let status2 = Inactive("reason")
  let status3 = Pending
  
  match status1 {
    Active(code) => assert_eq(code, 42)
    _ => assert_true(false)
  }
  
  match status2 {
    Inactive(reason) => assert_eq(reason, "reason")
    _ => assert_true(false)
  }
  
  match status3 {
    Pending => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 2: Higher-Order Functions
test "higher-order functions and functional composition" {
  // Function that takes another function as parameter
  let apply_twice = fn(f: (Int) -> Int, x: Int) -> Int {
    f(f(x))
  }
  
  let add_one = fn(x: Int) -> Int { x + 1 }
  let result = apply_twice(add_one, 5)
  assert_eq(result, 7)
  
  // Function that returns another function
  let create_multiplier = fn(factor: Int) -> (Int) -> Int {
    fn(x: Int) -> Int { x * factor }
  }
  
  let double = create_multiplier(2)
  let triple = create_multiplier(3)
  
  assert_eq(double(5), 10)
  assert_eq(triple(5), 15)
  
  // Function composition
  let compose = fn(f: (Int) -> Int, g: (Int) -> Int) -> (Int) -> Int {
    fn(x: Int) -> Int { f(g(x)) }
  }
  
  let add_then_multiply = compose(add_one, double)
  assert_eq(add_then_multiply(5), 11) // (5 * 2) + 1
}

// Test 3: Advanced Type System Features
test "advanced type system features" {
  // Test with generic types
  enum Option[T] {
    Some(T)
    None
  }
  
  let map_option = fn[T, U](opt: Option[T], f: (T) -> U) -> Option[U] {
    match opt {
      Some(value) => Some(f(value))
      None => None
    }
  }
  
  let string_opt = Some("hello")
  let length_opt = map_option(string_opt, fn(s: String) -> Int { s.length() })
  
  match length_opt {
    Some(len) => assert_eq(len, 5)
    None => assert_true(false)
  }
  
  // Test with type constraints
  trait Comparable {
    compare(Self, Self) -> Int
  }
  
  impl Comparable for Int {
    compare(Self, other: Self) -> Int {
      if self < other { -1 } else if self > other { 1 } else { 0 }
    }
  }
  
  let max = fn[T: Comparable](a: T, b: T) -> T {
    if Comparable::compare(a, b) > 0 { a } else { b }
  }
  
  assert_eq(max(10, 20), 20)
  assert_eq(max(30, 15), 30)
}

// Test 4: Advanced Data Structures
test "advanced data structures operations" {
  // Binary tree implementation
  enum Tree[T] {
    Empty
    Node(T, Tree[T], Tree[T])
  }
  
  let tree_insert = fn[T: Comparable](tree: Tree[T], value: T) -> Tree[T] {
    match tree {
      Empty => Node(value, Empty, Empty)
      Node(current, left, right) => {
        if Comparable::compare(value, current) < 0 {
          Node(current, tree_insert(left, value), right)
        } else {
          Node(current, left, tree_insert(right, value))
        }
      }
    }
  }
  
  let tree_contains = fn[T: Comparable](tree: Tree[T], value: T) -> Bool {
    match tree {
      Empty => false
      Node(current, left, right) => {
        if Comparable::compare(value, current) == 0 {
          true
        } else if Comparable::compare(value, current) < 0 {
          tree_contains(left, value)
        } else {
          tree_contains(right, value)
        }
      }
    }
  }
  
  let mut int_tree = Empty
  int_tree = tree_insert(int_tree, 5)
  int_tree = tree_insert(int_tree, 3)
  int_tree = tree_insert(int_tree, 7)
  int_tree = tree_insert(int_tree, 1)
  int_tree = tree_insert(int_tree, 9)
  
  assert_true(tree_contains(int_tree, 5))
  assert_true(tree_contains(int_tree, 1))
  assert_true(tree_contains(int_tree, 9))
  assert_false(tree_contains(int_tree, 4))
  assert_false(tree_contains(int_tree, 10))
}

// Test 5: Advanced Error Handling
test "advanced error handling with result types" {
  enum Result[T, E] {
    Ok(T)
    Err(E)
  }
  
  let safe_divide = fn(a: Int, b: Int) -> Result[Int, String] {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let result1 = safe_divide(10, 2)
  let result2 = safe_divide(10, 0)
  
  match result1 {
    Ok(value) => assert_eq(value, 5)
    Err(_) => assert_true(false)
  }
  
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
  
  // Chaining operations with result types
  let chain_operations = fn() -> Result[Int, String] {
    let step1 = safe_divide(20, 2)?
    let step2 = safe_divide(step1, 2)?
    let step3 = safe_divide(step2, 5)?
    Ok(step3)
  }
  
  match chain_operations() {
    Ok(value) => assert_eq(value, 2)
    Err(_) => assert_true(false)
  }
}

// Test 6: Advanced List Operations
test "advanced list operations and transformations" {
  // Map operation
  let map = fn[T, U](list: Array[T], f: (T) -> U) -> Array[U] {
    let mut result = []
    for item in list {
      result = result + [f(item)]
    }
    result
  }
  
  // Filter operation
  let filter = fn[T](list: Array[T], predicate: (T) -> Bool) -> Array[T] {
    let mut result = []
    for item in list {
      if predicate(item) {
        result = result + [item]
      }
    }
    result
  }
  
  // Fold/Reduce operation
  let fold = fn[T, U](list: Array[T], initial: U, f: (U, T) -> U) -> U {
    let mut acc = initial
    for item in list {
      acc = f(acc, item)
    }
    acc
  }
  
  let numbers = [1, 2, 3, 4, 5]
  
  // Test map
  let doubled = map(numbers, fn(x: Int) -> Int { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // Test filter
  let evens = filter(numbers, fn(x: Int) -> Bool { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // Test fold
  let sum = fold(numbers, 0, fn(acc: Int, x: Int) -> Int { acc + x })
  assert_eq(sum, 15)
  
  // Test composition of operations
  let result = fold(
    filter(map(numbers, fn(x: Int) -> Int { x * 2 }), fn(x: Int) -> Bool { x > 5 }),
    1,
    fn(acc: Int, x: Int) -> Int { acc * x }
  )
  assert_eq(result, 384) // 1 * 6 * 8 * 10
}

// Test 7: Advanced String Processing
test "advanced string processing and manipulation" {
  let text = "Hello, World! This is a test string."
  
  // Word splitting
  let split_words = fn(s: String) -> Array[String] {
    let mut words = []
    let mut current_word = ""
    
    for char in s.to_char_array() {
      if char == ' ' {
        if current_word.length() > 0 {
          words = words + [current_word]
          current_word = ""
        }
      } else {
        current_word = current_word + char.to_string()
      }
    }
    
    if current_word.length() > 0 {
      words = words + [current_word]
    }
    
    words
  }
  
  let words = split_words(text)
  assert_eq(words.length(), 7)
  assert_eq(words[0], "Hello,")
  assert_eq(words[1], "World!")
  assert_eq(words[6], "string.")
  
  // Palindrome check
  let is_palindrome = fn(s: String) -> Bool {
    let chars = s.to_char_array()
    let mut left = 0
    let mut right = chars.length() - 1
    
    while left < right {
      if chars[left] != chars[right] {
        return false
      }
      left = left + 1
      right = right - 1
    }
    
    true
  }
  
  assert_true(is_palindrome("racecar"))
  assert_true(is_palindrome("level"))
  assert_false(is_palindrome("hello"))
  assert_true(is_palindrome("")) // Empty string is a palindrome
}

// Test 8: Advanced Number Theory
test "advanced number theory operations" {
  // Greatest Common Divisor
  let gcd = fn(a: Int, b: Int) -> Int {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  // Least Common Multiple
  let lcm = fn(a: Int, b: Int) -> Int {
    if a == 0 || b == 0 {
      0
    } else {
      (a / gcd(a, b)) * b
    }
  }
  
  // Prime check
  let is_prime = fn(n: Int) -> Bool {
    if n <= 1 {
      return false
    }
    if n <= 3 {
      return true
    }
    if n % 2 == 0 || n % 3 == 0 {
      return false
    }
    
    let mut i = 5
    while i * i <= n {
      if n % i == 0 || n % (i + 2) == 0 {
        return false
      }
      i = i + 6
    }
    
    true
  }
  
  // Test GCD
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 13), 1)
  assert_eq(gcd(100, 25), 25)
  
  // Test LCM
  assert_eq(lcm(4, 6), 12)
  assert_eq(lcm(5, 7), 35)
  assert_eq(lcm(10, 15), 30)
  
  // Test prime check
  assert_true(is_prime(2))
  assert_true(is_prime(3))
  assert_true(is_prime(5))
  assert_true(is_prime(7))
  assert_true(is_prime(11))
  assert_true(is_prime(13))
  assert_true(is_prime(17))
  assert_true(is_prime(19))
  assert_true(is_prime(23))
  assert_true(is_prime(29))
  
  assert_false(is_prime(1))
  assert_false(is_prime(4))
  assert_false(is_prime(6))
  assert_false(is_prime(8))
  assert_false(is_prime(9))
  assert_false(is_prime(10))
  assert_false(is_prime(12))
  assert_false(is_prime(14))
  assert_false(is_prime(15))
  assert_false(is_prime(16))
}

// Test 9: Advanced Recursive Algorithms
test "advanced recursive algorithms" {
  // Fibonacci sequence with memoization
  let fib_memo = fn(n: Int) -> Int {
    let mut memo = {}
    
    let fib_helper = fn(k: Int) -> Int {
      if memo.contains(k) {
        return memo[k]
      }
      
      let result = if k <= 1 {
        k
      } else {
        fib_helper(k - 1) + fib_helper(k - 2)
      }
      
      memo[k] = result
      result
    }
    
    fib_helper(n)
  }
  
  // Tower of Hanoi solver
  let hanoi_moves = fn(n: Int) -> Array[(Int, Int, Int)] {
    let mut moves = []
    
    let hanoi_helper = fn(k: Int, source: Int, target: Int, auxiliary: Int) -> Unit {
      if k == 1 {
        moves = moves + [(source, target, auxiliary)]
      } else {
        hanoi_helper(k - 1, source, auxiliary, target)
        moves = moves + [(source, target, auxiliary)]
        hanoi_helper(k - 1, auxiliary, target, source)
      }
    }
    
    hanoi_helper(n, 1, 3, 2)
    moves
  }
  
  // Test Fibonacci
  assert_eq(fib_memo(0), 0)
  assert_eq(fib_memo(1), 1)
  assert_eq(fib_memo(2), 1)
  assert_eq(fib_memo(3), 2)
  assert_eq(fib_memo(4), 3)
  assert_eq(fib_memo(5), 5)
  assert_eq(fib_memo(6), 8)
  assert_eq(fib_memo(7), 13)
  assert_eq(fib_memo(8), 21)
  assert_eq(fib_memo(9), 34)
  assert_eq(fib_memo(10), 55)
  
  // Test Tower of Hanoi
  let moves_1 = hanoi_moves(1)
  assert_eq(moves_1.length(), 1)
  assert_eq(moves_1[0], (1, 3, 2))
  
  let moves_2 = hanoi_moves(2)
  assert_eq(moves_2.length(), 3)
  assert_eq(moves_2[0], (1, 2, 3))
  assert_eq(moves_2[1], (1, 3, 2))
  assert_eq(moves_2[2], (2, 3, 1))
  
  let moves_3 = hanoi_moves(3)
  assert_eq(moves_3.length(), 7)
}

// Test 10: Advanced State Management
test "advanced state management patterns" {
  // State machine implementation
  enum State {
    Idle
    Active(Int)
    Paused(String)
    Completed
  }
  
  enum Event {
    Start(Int)
    Pause(String)
    Resume
    Stop
    Reset
  }
  
  let transition = fn(state: State, event: Event) -> State {
    match (state, event) {
      (Idle, Start(value)) => Active(value)
      (Active(_), Pause(reason)) => Paused(reason)
      (Paused(_), Resume) => Active(0) // Resume with default value
      (Active(_), Stop) => Completed
      (Paused(_), Stop) => Completed
      (Completed, Reset) => Idle
      (Idle, Reset) => Idle
      (Completed, Start(value)) => Active(value)
      _ => state // Invalid transition, stay in current state
    }
  }
  
  // Test state transitions
  let mut state = Idle
  
  // Start the machine
  state = transition(state, Start(42))
  match state {
    Active(value) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  // Pause the machine
  state = transition(state, Pause("maintenance"))
  match state {
    Paused(reason) => assert_eq(reason, "maintenance")
    _ => assert_true(false)
  }
  
  // Resume the machine
  state = transition(state, Resume)
  match state {
    Active(value) => assert_eq(value, 0)
    _ => assert_true(false)
  }
  
  // Stop the machine
  state = transition(state, Stop)
  match state {
    Completed => assert_true(true)
    _ => assert_true(false)
  }
  
  // Reset the machine
  state = transition(state, Reset)
  match state {
    Idle => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test invalid transitions
  state = transition(state, Pause("invalid")) // Invalid: can't pause from Idle
  match state {
    Idle => assert_true(true) // Should remain in Idle
    _ => assert_true(false)
  }
}