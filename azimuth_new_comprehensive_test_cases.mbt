// Azimuth New Comprehensive Test Cases
// This file contains new comprehensive test cases for the Azimuth telemetry system

// Test 1: Data Type Conversion and Validation
test "data type conversion and validation" {
  // Test string to number conversion
  let string_to_int = fn(s: String) -> Option[Int] {
    match s.parse_int() {
      Some(n) => Some(n),
      None => None
    }
  }
  
  let string_to_float = fn(s: String) -> Option[Float] {
    match s.parse_float() {
      Some(f) => Some(f),
      None => None
    }
  }
  
  // Test valid conversions
  assert_eq(string_to_int("42"), Some(42))
  assert_eq(string_to_int("-10"), Some(-10))
  assert_eq(string_to_float("3.14"), Some(3.14))
  assert_eq(string_to_float("-2.5"), Some(-2.5))
  
  // Test invalid conversions
  assert_eq(string_to_int("abc"), None)
  assert_eq(string_to_float("xyz"), None)
  assert_eq(string_to_int("3.14"), None)  // Float string can't convert to int
  
  // Test boolean conversion
  let string_to_bool = fn(s: String) -> Option[Bool] {
    match s.to_lowercase() {
      "true" => Some(true),
      "false" => Some(false),
      "1" => Some(true),
      "0" => Some(false),
      _ => None
    }
  }
  
  assert_eq(string_to_bool("true"), Some(true))
  assert_eq(string_to_bool("false"), Some(false))
  assert_eq(string_to_bool("TRUE"), Some(true))
  assert_eq(string_to_bool("1"), Some(true))
  assert_eq(string_to_bool("0"), Some(false))
  assert_eq(string_to_bool("maybe"), None)
}

// Test 2: Collection Operations and Utilities
test "collection operations and utilities" {
  // Test array operations
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Filter even numbers
  let even_numbers = numbers.filter(fn(n) { n % 2 == 0 })
  assert_eq(even_numbers, [2, 4, 6, 8, 10])
  
  // Map to squares
  let squares = numbers.map(fn(n) { n * n })
  assert_eq(squares[0], 1)
  assert_eq(squares[4], 25)
  assert_eq(squares[9], 100)
  
  // Reduce to sum
  let sum = numbers.reduce(fn(acc, n) { acc + n }, 0)
  assert_eq(sum, 55)
  
  // Test string operations
  let words = ["hello", "world", "azimuth", "telemetry"]
  
  // Filter by length
  let long_words = words.filter(fn(w) { w.length() > 5 })
  assert_eq(long_words, ["azimuth", "telemetry"])
  
  // Map to lengths
  let lengths = words.map(fn(w) { w.length() })
  assert_eq(lengths, [5, 5, 7, 9])
  
  // Join strings
  let joined = words.reduce(fn(acc, w) { acc + " " + w }, "")
  assert_eq(joined, " hello world azimuth telemetry")
  
  // Test custom utility functions
  let find_max = fn(arr: Array[Int]) -> Option[Int] {
    if arr.length() == 0 {
      None
    } else {
      let mut max_val = arr[0]
      for i in 1..arr.length() {
        if arr[i] > max_val {
          max_val = arr[i]
        }
      }
      Some(max_val)
    }
  }
  
  let find_min = fn(arr: Array[Int]) -> Option[Int] {
    if arr.length() == 0 {
      None
    } else {
      let mut min_val = arr[0]
      for i in 1..arr.length() {
        if arr[i] < min_val {
          min_val = arr[i]
        }
      }
      Some(min_val)
    }
  }
  
  assert_eq(find_max(numbers), Some(10))
  assert_eq(find_min(numbers), Some(1))
  assert_eq(find_max([]), None)
  assert_eq(find_min([]), None)
}

// Test 3: Error Handling and Edge Cases
test "error handling and edge cases" {
  // Test division with safety checks
  let safe_divide = fn(a: Float, b: Float) -> Option[Float] {
    if b == 0.0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  assert_eq(safe_divide(10.0, 2.0), Some(5.0))
  assert_eq(safe_divide(10.0, 0.0), None)
  
  // Test array access with bounds checking
  let safe_get = fn<T>(arr: Array[T>, index: Int) -> Option[T] {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let test_array = ["a", "b", "c"]
  assert_eq(safe_get(test_array, 0), Some("a"))
  assert_eq(safe_get(test_array, 2), Some("c"))
  assert_eq(safe_get(test_array, 3), None)
  assert_eq(safe_get(test_array, -1), None)
  
  // Test string operations with edge cases
  let safe_substring = fn(s: String, start: Int, length: Int) -> Option[String] {
    if start < 0 || length < 0 || start >= s.length() {
      None
    } else {
      let end = (start + length).min(s.length())
      Some(s.substring(start, end - start))
    }
  }
  
  let test_string = "hello world"
  assert_eq(safe_substring(test_string, 0, 5), Some("hello"))
  assert_eq(safe_substring(test_string, 6, 5), Some("world"))
  assert_eq(safe_substring(test_string, 0, 11), Some("hello world"))
  assert_eq(safe_substring(test_string, 0, 12), Some("hello world"))  // Truncated
  assert_eq(safe_substring(test_string, 11, 1), None)  // Start out of bounds
  assert_eq(safe_substring(test_string, -1, 5), None)  // Negative start
  assert_eq(safe_substring(test_string, 0, -1), None)  // Negative length
}

// Test 4: Mathematical Functions and Algorithms
test "mathematical functions and algorithms" {
  // Test factorial function
  let factorial = fn(n: Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // Test fibonacci sequence
  let fibonacci = fn(n: Int) -> Int {
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
  
  // Test greatest common divisor
  let gcd = fn(a: Int, b: Int) -> Int {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 13), 1)
  assert_eq(gcd(100, 25), 25)
  assert_eq(gcd(0, 5), 5)
  assert_eq(gcd(5, 0), 5)
  
  // Test prime number checking
  let is_prime = fn(n: Int) -> Bool {
    if n <= 1 {
      false
    } else if n <= 3 {
      true
    } else if n % 2 == 0 || n % 3 == 0 {
      false
    } else {
      let mut i = 5
      let mut prime = true
      while i * i <= n && prime {
        if n % i == 0 || n % (i + 2) == 0 {
          prime = false
        }
        i = i + 6
      }
      prime
    }
  }
  
  assert_false(is_prime(0))
  assert_false(is_prime(1))
  assert_true(is_prime(2))
  assert_true(is_prime(3))
  assert_true(is_prime(5))
  assert_true(is_prime(7))
  assert_true(is_prime(11))
  assert_true(is_prime(13))
  assert_false(is_prime(15))
  assert_false(is_prime(21))
  assert_true(is_prime(23))
}

// Test 5: Data Structure Implementations
test "data structure implementations" {
  // Simple stack implementation
  type Stack<T> = {
    mut items: Array[T>
  }
  
  let stack_create = fn<T>() -> Stack<T> { { mut items: [] } }
  let stack_push = fn<T>(stack: Stack<T>, item: T) { stack.items = stack.items.push(item) }
  let stack_pop = fn<T>(stack: Stack<T>) -> Option<T> {
    if stack.items.length() == 0 {
      None
    } else {
      let item = stack.items[stack.items.length() - 1]
      stack.items = stack.items.slice(0, stack.items.length() - 1)
      Some(item)
    }
  }
  let stack_peek = fn<T>(stack: Stack<T>) -> Option<T> {
    if stack.items.length() == 0 {
      None
    } else {
      Some(stack.items[stack.items.length() - 1])
    }
  }
  let stack_is_empty = fn<T>(stack: Stack<T>) -> Bool { stack.items.length() == 0 }
  
  let mut int_stack = stack_create<Int>()
  assert_true(stack_is_empty(int_stack))
  assert_eq(stack_peek(int_stack), None)
  assert_eq(stack_pop(int_stack), None)
  
  stack_push(int_stack, 1)
  stack_push(int_stack, 2)
  stack_push(int_stack, 3)
  
  assert_false(stack_is_empty(int_stack))
  assert_eq(stack_peek(int_stack), Some(3))
  
  assert_eq(stack_pop(int_stack), Some(3))
  assert_eq(stack_pop(int_stack), Some(2))
  assert_eq(stack_pop(int_stack), Some(1))
  assert_eq(stack_pop(int_stack), None)
  assert_true(stack_is_empty(int_stack))
  
  // Simple queue implementation
  type Queue<T> = {
    mut items: Array[T]
  }
  
  let queue_create = fn<T>() -> Queue<T> { { mut items: [] } }
  let queue_enqueue = fn<T>(queue: Queue<T>, item: T) { queue.items = queue.items.push(item) }
  let queue_dequeue = fn<T>(queue: Queue<T>) -> Option<T> {
    if queue.items.length() == 0 {
      None
    } else {
      let item = queue.items[0]
      queue.items = queue.items.slice(1, queue.items.length())
      Some(item)
    }
  }
  let queue_front = fn<T>(queue: Queue<T>) -> Option<T> {
    if queue.items.length() == 0 {
      None
    } else {
      Some(queue.items[0])
    }
  }
  let queue_is_empty = fn<T>(queue: Queue<T>) -> Bool { queue.items.length() == 0 }
  
  let mut string_queue = queue_create<String>()
  assert_true(queue_is_empty(string_queue))
  assert_eq(queue_front(string_queue), None)
  assert_eq(queue_dequeue(string_queue), None)
  
  queue_enqueue(string_queue, "first")
  queue_enqueue(string_queue, "second")
  queue_enqueue(string_queue, "third")
  
  assert_false(queue_is_empty(string_queue))
  assert_eq(queue_front(string_queue), Some("first"))
  
  assert_eq(queue_dequeue(string_queue), Some("first"))
  assert_eq(queue_dequeue(string_queue), Some("second"))
  assert_eq(queue_dequeue(string_queue), Some("third"))
  assert_eq(queue_dequeue(string_queue), None)
  assert_true(queue_is_empty(string_queue))
}

// Test 6: String Processing and Pattern Matching
test "string processing and pattern matching" {
  // Test string reversal
  let reverse_string = fn(s: String) -> String {
    let mut chars = []
    for i in 0..s.length() {
      chars = chars.push(s[i])
    }
    
    let mut reversed = ""
    for i in (chars.length() - 1)..=0 {
      reversed = reversed + chars[i]
    }
    reversed
  }
  
  assert_eq(reverse_string("hello"), "olleh")
  assert_eq(reverse_string("azimuth"), "htimuza")
  assert_eq(reverse_string(""), "")
  assert_eq(reverse_string("a"), "a")
  
  // Test palindrome checking
  let is_palindrome = fn(s: String) -> Bool {
    let cleaned = s.to_lowercase().filter(fn(c) { c.is_alphanumeric() })
    let reversed = reverse_string(cleaned)
    cleaned == reversed
  }
  
  assert_true(is_palindrome("racecar"))
  assert_true(is_palindrome("RaceCar"))
  assert_true(is_palindrome("A man, a plan, a canal: Panama"))
  assert_false(is_palindrome("hello"))
  assert_true(is_palindrome(""))
  assert_true(is_palindrome("a"))
  
  // Test word counting
  let count_words = fn(s: String) -> Int {
    if s.trim() == "" {
      0
    } else {
      let words = s.split_whitespace()
      words.length()
    }
  }
  
  assert_eq(count_words(""), 0)
  assert_eq(count_words("   "), 0)
  assert_eq(count_words("hello"), 1)
  assert_eq(count_words("hello world"), 2)
  assert_eq(count_words("  hello   world  "), 2)
  assert_eq(count_words("the quick brown fox jumps over the lazy dog"), 9)
  
  // Test character counting
  let count_char = fn(s: String, target: Char) -> Int {
    let mut count = 0
    for c in s {
      if c == target {
        count = count + 1
      }
    }
    count
  }
  
  assert_eq(count_char("hello", 'l'), 2)
  assert_eq(count_char("hello", 'o'), 1)
  assert_eq(count_char("hello", 'x'), 0)
  assert_eq(count_char("", 'a'), 0)
  assert_eq(count_char("aaaaa", 'a'), 5)
}

// Test 7: Date and Time Operations
test "date and time operations" {
  // Simple timestamp representation
  type Timestamp = Int
  
  // Convert timestamp to date components (simplified)
  let timestamp_to_date = fn(ts: Timestamp) -> (Int, Int, Int) {
    // This is a simplified calculation for demonstration
    // In real implementation, would use proper date libraries
    let days_since_epoch = ts / 86400  // Seconds per day
    let year = 1970 + days_since_epoch / 365  // Simplified, ignoring leap years
    let day_of_year = days_since_epoch % 365
    let month = (day_of_year / 30) + 1  // Simplified, assuming 30 days per month
    let day = (day_of_year % 30) + 1
    
    (year, month, day)
  }
  
  // Test timestamp conversion
  let (year, month, day) = timestamp_to_date(1640995200)  // 2022-01-01 00:00:00 UTC
  assert_eq(year, 2022)
  assert_eq(month, 1)
  assert_eq(day, 1)
  
  // Test time difference calculation
  let time_difference = fn(ts1: Timestamp, ts2: Timestamp) -> Int {
    if ts1 > ts2 {
      ts1 - ts2
    } else {
      ts2 - ts1
    }
  }
  
  let ts1 = 1640995200  // 2022-01-01 00:00:00 UTC
  let ts2 = 1641081600  // 2022-01-02 00:00:00 UTC
  let diff = time_difference(ts1, ts2)
  assert_eq(diff, 86400)  // 1 day in seconds
  
  // Test time formatting (simplified)
  let format_time = fn(ts: Timestamp) -> String {
    let hours = (ts % 86400) / 3600
    let minutes = (ts % 3600) / 60
    let seconds = ts % 60
    
    let hour_str = if hours < 10 { "0" + hours.to_string() } else { hours.to_string() }
    let min_str = if minutes < 10 { "0" + minutes.to_string() } else { minutes.to_string() }
    let sec_str = if seconds < 10 { "0" + seconds.to_string() } else { seconds.to_string() }
    
    hour_str + ":" + min_str + ":" + sec_str
  }
  
  assert_eq(format_time(0), "00:00:00")
  assert_eq(format_time(3661), "01:01:01")
  assert_eq(format_time(43200), "12:00:00")
  assert_eq(format_time(86399), "23:59:59")
}

// Test 8: Hash and Equality Functions
test "hash and equality functions" {
  // Simple hash function for strings
  let simple_hash = fn(s: String) -> Int {
    let mut hash = 0
    for i in 0..s.length() {
      hash = hash + s[i].to_int() * (i + 1)
    }
    hash
  }
  
  // Test hash consistency
  let test_str = "azimuth"
  let hash1 = simple_hash(test_str)
  let hash2 = simple_hash(test_str)
  assert_eq(hash1, hash2)
  
  // Test hash differences
  assert_true(simple_hash("hello") != simple_hash("world"))
  assert_true(simple_hash("test") != simple_hash("tset"))
  
  // Test array equality
  let arrays_equal = fn<T>(arr1: Array<T>, arr2: Array<T>) -> Bool {
    if arr1.length() != arr2.length() {
      false
    } else {
      let mut equal = true
      for i in 0..arr1.length() {
        if arr1[i] != arr2[i] {
          equal = false
        }
      }
      equal
    }
  }
  
  assert_true(arrays_equal([1, 2, 3], [1, 2, 3]))
  assert_false(arrays_equal([1, 2, 3], [1, 2, 4]))
  assert_false(arrays_equal([1, 2, 3], [1, 2]))
  assert_true(arrays_equal([], []))
  assert_true(arrays_equal(["a", "b"], ["a", "b"]))
  assert_false(arrays_equal(["a", "b"], ["b", "a"]))
  
  // Test deep equality for nested structures
  type Point = { x: Int, y: Int }
  type Rectangle = { top_left: Point, bottom_right: Point }
  
  let rectangles_equal = fn(r1: Rectangle, r2: Rectangle) -> Bool {
    r1.top_left.x == r2.top_left.x &&
    r1.top_left.y == r2.top_left.y &&
    r1.bottom_right.x == r2.bottom_right.x &&
    r1.bottom_right.y == r2.bottom_right.y
  }
  
  let rect1 = { top_left: { x: 0, y: 0 }, bottom_right: { x: 10, y: 10 } }
  let rect2 = { top_left: { x: 0, y: 0 }, bottom_right: { x: 10, y: 10 } }
  let rect3 = { top_left: { x: 0, y: 0 }, bottom_right: { x: 10, y: 11 } }
  
  assert_true(rectangles_equal(rect1, rect2))
  assert_false(rectangles_equal(rect1, rect3))
}

// Test 9: Functional Programming Patterns
test "functional programming patterns" {
  // Test function composition
  let compose = fn<A, B, C>(f: (B) -> C, g: (A) -> B) -> (A) -> C {
    fn(a: A) { f(g(a)) }
  }
  
  let add_one = fn(x: Int) { x + 1 }
  let double = fn(x: Int) { x * 2 }
  
  let add_one_then_double = compose(double, add_one)
  let double_then_add_one = compose(add_one, double)
  
  assert_eq(add_one_then_double(5), 12)  // (5 + 1) * 2
  assert_eq(double_then_add_one(5), 11)  // (5 * 2) + 1
  
  // Test currying
  let curried_add = fn(a: Int) -> (Int) -> Int {
    fn(b: Int) { a + b }
  }
  
  let add_five = curried_add(5)
  assert_eq(add_five(3), 8)
  assert_eq(add_five(10), 15)
  
  // Test higher-order functions with custom predicates
  let filter_with_predicate = fn<T>(arr: Array<T>, predicate: (T) -> Bool) -> Array<T> {
    let mut result = []
    for item in arr {
      if predicate(item) {
        result = result.push(item)
      }
    }
    result
  }
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Filter with custom predicate
  let is_multiple_of_three = fn(n: Int) { n % 3 == 0 }
  let multiples_of_three = filter_with_predicate(numbers, is_multiple_of_three)
  assert_eq(multiples_of_three, [3, 6, 9])
  
  // Filter with lambda predicate
  let greater_than_five = filter_with_predicate(numbers, fn(n) { n > 5 })
  assert_eq(greater_than_five, [6, 7, 8, 9, 10])
  
  // Test map with custom transformation
  let map_with_transform = fn<T, U>(arr: Array<T>, transform: (T) -> U) -> Array<U> {
    let mut result = []
    for item in arr {
      result = result.push(transform(item))
    }
    result
  }
  
  let strings = ["hello", "world", "azimuth"]
  let string_lengths = map_with_transform(strings, fn(s) { s.length() })
  assert_eq(string_lengths, [5, 5, 7])
  
  // Test reduce with custom accumulator
  let reduce_with_accumulator = fn<T, U>(arr: Array<T>, initial: U, accumulator: (U, T) -> U) -> U {
    let mut result = initial
    for item in arr {
      result = accumulator(result, item)
    }
    result
  }
  
  let product = reduce_with_accumulator(numbers, 1, fn(acc, n) { acc * n })
  assert_eq(product, 3628800)  // 10!
  
  let concatenated = reduce_with_accumulator(strings, "", fn(acc, s) { acc + s })
  assert_eq(concatenated, "helloworldazimuth")
}

// Test 10: Performance and Optimization Tests
test "performance and optimization tests" {
  // Test memoization for expensive functions
  type MemoCache<T, R> = {
    mut cache: Array[(T, R)]
  }
  
  let memo_cache_create = fn<T, R>() -> MemoCache<T, R> { { mut cache: [] } }
  
  let memoized_fibonacci = fn(n: Int, cache: MemoCache<Int, Int>) -> Int {
    // Check if value is in cache
    for i in 0..cache.cache.length() {
      if cache.cache[i].0 == n {
        return cache.cache[i].1
      }
    }
    
    // Calculate and cache the result
    let result = if n <= 1 {
      n
    } else {
      memoized_fibonacci(n - 1, cache) + memoized_fibonacci(n - 2, cache)
    }
    
    cache.cache = cache.cache.push((n, result))
    result
  }
  
  let fib_cache = memo_cache_create<Int, Int>()
  
  // Test memoized fibonacci
  assert_eq(memoized_fibonacci(0, fib_cache), 0)
  assert_eq(memoized_fibonacci(1, fib_cache), 1)
  assert_eq(memoized_fibonacci(5, fib_cache), 5)
  assert_eq(memoized_fibonacci(10, fib_cache), 55)
  
  // Test binary search
  let binary_search = fn(arr: Array[Int], target: Int) -> Option[Int] {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        return Some(mid)
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    None
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  assert_eq(binary_search(sorted_array, 7), Some(3))
  assert_eq(binary_search(sorted_array, 1), Some(0))
  assert_eq(binary_search(sorted_array, 19), Some(9))
  assert_eq(binary_search(sorted_array, 2), None)
  assert_eq(binary_search(sorted_array, 20), None)
  
  // Test lazy evaluation simulation
  type Lazy<T> = {
    mut evaluated: Bool,
    mut value: Option<T>,
    mut thunk: () -> T
  }
  
  let lazy_create = fn<T>(thunk: () -> T) -> Lazy<T> {
    {
      mut evaluated: false,
      mut value: None,
      mut thunk
    }
  }
  
  let lazy_force = fn<T>(lazy: Lazy<T>) -> T {
    if !lazy.evaluated {
      lazy.value = Some(lazy.thunk())
      lazy.evaluated = true
    }
    lazy.value.unwrap()
  }
  
  let expensive_computation = fn() -> Int {
    // Simulate expensive computation
    let mut result = 0
    for i in 0..1000 {
      result = result + i
    }
    result
  }
  
  let lazy_value = lazy_create(expensive_computation)
  
  // Value not computed yet
  assert_false(lazy_value.evaluated)
  assert_eq(lazy_value.value, None)
  
  // Force evaluation
  let result = lazy_force(lazy_value)
  assert_eq(result, 499500)  // Sum of 0 to 999
  
  // Value is now cached
  assert_true(lazy_value.evaluated)
  assert_eq(lazy_value.value, Some(499500))
  
  // Subsequent accesses use cached value
  let cached_result = lazy_force(lazy_value)
  assert_eq(cached_result, 499500)
}