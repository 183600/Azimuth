// Azimuth New MoonBit Feature Tests
// 新的MoonBit特性测试

// Test 1: 递归函数测试
test "recursive function tests" {
  let factorial = fn(n : Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  let fibonacci = fn(n : Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(6), 8)
}

// Test 2: 结构体和记录类型测试
test "struct and record type tests" {
  struct Point {
    x : Int
    y : Int
  }
  
  let p1 = { x: 10, y: 20 }
  let p2 = { x: 5, y: 15 }
  
  assert_eq(p1.x, 10)
  assert_eq(p1.y, 20)
  
  let distance = fn(p1 : Point, p2 : Point) -> Int {
    let dx = p1.x - p2.x
    let dy = p1.y - p2.y
    dx * dx + dy * dy
  }
  
  assert_eq(distance(p1, p2), 200)
}

// Test 3: 字典/映射操作测试
test "dictionary/map operations tests" {
  let scores = Map::empty()
  let scores = scores.insert("alice", 95)
  let scores = scores.insert("bob", 87)
  let scores = scores.insert("charlie", 92)
  
  assert_eq(scores.get("alice"), Some(95))
  assert_eq(scores.get("bob"), Some(87))
  assert_eq(scores.get("david"), None)
  
  let updated = scores.insert("bob", 90)
  assert_eq(updated.get("bob"), Some(90))
  assert_eq(scores.get("bob"), Some(87))
}

// Test 4: 异常处理测试
test "exception handling tests" {
  let divide = fn(a : Int, b : Int) -> Result[Int, String] {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let result1 = divide(10, 2)
  let result2 = divide(10, 0)
  
  match result1 {
    Ok(value) => assert_eq(value, 5)
    Err(_) => assert_true(false)
  }
  
  match result2 {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
}

// Test 5: 列表推导和高级操作测试
test "list comprehension and advanced operations tests" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let evens_squared = numbers
    .filter(fn(x) { x % 2 == 0 })
    .map(fn(x) { x * x })
  
  assert_eq(evens_squared, [4, 16, 36, 64, 100])
  
  let sum_of_squares = numbers
    .filter(fn(x) { x > 5 })
    .map(fn(x) { x * x })
    .reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(sum_of_squares, 295)
}

// Test 6: 可变状态测试
test "mutable state tests" {
  let mut counter = 0
  
  let increment = fn() {
    counter = counter + 1
  }
  
  let reset = fn() {
    counter = 0
  }
  
  assert_eq(counter, 0)
  increment()
  assert_eq(counter, 1)
  increment()
  increment()
  assert_eq(counter, 3)
  reset()
  assert_eq(counter, 0)
}

// Test 7: 函数式编程模式测试
test "functional programming patterns tests" {
  let pipeline = fn(initial, functions) {
    functions.reduce(fn(acc, f) { f(acc) }, initial)
  }
  
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  let result = pipeline(5, [add_one, double, square])
  assert_eq(result, 144)
  
  let curry = fn(f) {
    fn(a) {
      fn(b) {
        f(a, b)
      }
    }
  }
  
  let add = fn(a, b) { a + b }
  let curried_add = curry(add)
  let add_five = curried_add(5)
  
  assert_eq(add_five(3), 8)
}

// Test 8: 字符串高级操作测试
test "advanced string operations tests" {
  let text = "Azimuth Telemetry System"
  
  let words = text.split(" ")
  assert_eq(words.length(), 3)
  assert_eq(words[0], "Azimuth")
  assert_eq(words[1], "Telemetry")
  assert_eq(words[2], "System")
  
  let joined = words.join("-")
  assert_eq(joined, "Azimuth-Telemetry-System")
  
  let reversed = text.reverse()
  assert_eq(reversed, "metsySl ymeleT mhtuziA")
  
  let trimmed = "  hello world  ".trim()
  assert_eq(trimmed, "hello world")
}

// Test 9: 类型转换测试
test "type conversion tests" {
  let int_val = 42
  let float_val = 3.14159
  let string_val = "123"
  
  let int_to_float = int_val.to_float()
  let float_to_int = float_val.to_int()
  let string_to_int = string_val.to_int()
  
  assert_eq(int_to_float, 42.0)
  assert_eq(float_to_int, 3)
  assert_eq(string_to_int, Some(123))
  
  let invalid_string = "not a number"
  let invalid_to_int = invalid_string.to_int()
  assert_eq(invalid_to_int, None)
}