// Azimuth 函数式编程测试
// 专注于测试函数式编程概念和高阶函数

// 测试1: 高阶函数 - map
test "高阶函数map测试" {
  let numbers = [1, 2, 3, 4, 5]
  
  let doubled = array_map(numbers, fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let squared = array_map(numbers, fn(x) { x * x })
  assert_eq(squared, [1, 4, 9, 16, 25])
  
  let to_strings = array_map(numbers, fn(x) { x.to_string() })
  assert_eq(to_strings, ["1", "2", "3", "4", "5"])
  
  let empty_array = []
  let mapped_empty = array_map(empty_array, fn(x) { x * 2 })
  assert_eq(mapped_empty, [])
}

fn array_map[T, U](arr : Array[T], f : (T) -> U) -> Array[U] {
  let result = []
  for i = 0; i < arr.length(); i = i + 1 {
    result.push(f(arr[i]))
  }
  result
}

// 测试2: 高阶函数 - filter
test "高阶函数filter测试" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let evens = array_filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4, 6, 8, 10])
  
  let odds = array_filter(numbers, fn(x) { x % 2 == 1 })
  assert_eq(odds, [1, 3, 5, 7, 9])
  
  let greater_than_five = array_filter(numbers, fn(x) { x > 5 })
  assert_eq(greater_than_five, [6, 7, 8, 9, 10])
  
  let empty_array = []
  let filtered_empty = array_filter(empty_array, fn(x) { x > 5 })
  assert_eq(filtered_empty, [])
}

fn array_filter[T](arr : Array[T], predicate : (T) -> Bool) -> Array[T] {
  let result = []
  for i = 0; i < arr.length(); i = i + 1 {
    if (predicate(arr[i])) {
      result.push(arr[i])
    }
  }
  result
}

// 测试3: 高阶函数 - reduce
test "高阶函数reduce测试" {
  let numbers = [1, 2, 3, 4, 5]
  
  let sum = array_reduce(numbers, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
  
  let product = array_reduce(numbers, 1, fn(acc, x) { acc * x })
  assert_eq(product, 120)
  
  let max_value = array_reduce(numbers, numbers[0], fn(acc, x) { 
    if (x > acc) { x } else { acc } 
  })
  assert_eq(max_value, 5)
  
  let concatenated = array_reduce(["a", "b", "c"], "", fn(acc, x) { acc + x })
  assert_eq(concatenated, "abc")
}

fn array_reduce[T, U](arr : Array[T], initial : U, reducer : (U, T) -> U) -> U {
  let result = initial
  for i = 0; i < arr.length(); i = i + 1 {
    result = reducer(result, arr[i])
  }
  result
}

// 测试4: 函数组合
test "函数组合测试" {
  let add_one = fn(x : Int) { x + 1 }
  let double = fn(x : Int) { x * 2 }
  let square = fn(x : Int) { x * x }
  
  let add_one_then_double = compose(double, add_one)
  assert_eq(add_one_then_double(3), 8)  // (3 + 1) * 2 = 8
  
  let double_then_square = compose(square, double)
  assert_eq(double_then_square(3), 36)  // (3 * 2)^2 = 36
  
  let add_one_then_double_then_square = compose(square, compose(double, add_one))
  assert_eq(add_one_then_double_then_square(3), 64)  // ((3 + 1) * 2)^2 = 64
}

fn compose[A, B, C](g : (B) -> C, f : (A) -> B) -> (A) -> C {
  fn(x : A) { g(f(x)) }
}

// 测试5: 柯里化
test "柯里化测试" {
  let add = fn(a : Int, b : Int) { a + b }
  let curried_add = curry(add)
  
  let add_five = curried_add(5)
  assert_eq(add_five(3), 8)
  assert_eq(add_five(10), 15)
  
  let multiply = fn(a : Int, b : Int) { a * b }
  let curried_multiply = curry(multiply)
  
  let double = curried_multiply(2)
  assert_eq(double(5), 10)
  assert_eq(double(7), 14)
}

fn curry[A, B, C](f : (A, B) -> C) -> (A) -> (B) -> C {
  fn(a : A) {
    fn(b : B) {
      f(a, b)
    }
  }
}

// 测试6: 函数作为参数和返回值
test "函数作为参数和返回值测试" {
  let numbers = [1, 2, 3, 4, 5]
  
  let adder = create_adder(10)
  assert_eq(adder(5), 15)
  
  let multiplier = create_multiplier(3)
  assert_eq(multiplier(4), 12)
  
  let transformed1 = apply_twice(numbers, adder)
  assert_eq(transformed1, [21, 22, 23, 24, 25])  // 每个元素加10，再加10
  
  let transformed2 = apply_twice(numbers, multiplier)
  assert_eq(transformed2, [9, 18, 27, 36, 45])  // 每个元素乘3，再乘3
}

fn create_adder(n : Int) -> (Int) -> Int {
  fn(x : Int) { x + n }
}

fn create_multiplier(n : Int) -> (Int) -> Int {
  fn(x : Int) { x * n }
}

fn apply_twice(arr : Array[Int], f : (Int) -> Int) -> Array[Int] {
  let result = []
  for i = 0; i < arr.length(); i = i + 1 {
    let value = arr[i]
    let transformed = f(f(value))  // 应用函数两次
    result.push(transformed)
  }
  result
}

// 测试7: 递归函数
test "递归函数测试" {
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(10), 55)
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(54, 24), 6)
  assert_eq(gcd(17, 5), 1)
}

fn factorial(n : Int) -> Int {
  if (n <= 1) {
    1
  } else {
    n * factorial(n - 1)
  }
}

fn fibonacci(n : Int) -> Int {
  if (n <= 1) {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}

fn gcd(a : Int, b : Int) -> Int {
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}

// 测试8: 尾递归优化
test "尾递归优化测试" {
  assert_eq(tail_factorial(0), 1)
  assert_eq(tail_factorial(1), 1)
  assert_eq(tail_factorial(5), 120)
  assert_eq(tail_factorial(10), 3628800)
  
  assert_eq(tail_sum([1, 2, 3, 4, 5]), 15)
  assert_eq(tail_sum([10, 20, 30]), 60)
  assert_eq(tail_sum([]), 0)
  
  assert_eq(tail_reverse([1, 2, 3, 4, 5]), [5, 4, 3, 2, 1])
  assert_eq(tail_reverse(["a", "b", "c"]), ["c", "b", "a"])
  assert_eq(tail_reverse([]), [])
}

fn tail_factorial(n : Int) -> Int {
  tail_factorial_helper(n, 1)
}

fn tail_factorial_helper(n : Int, acc : Int) -> Int {
  if (n <= 1) {
    acc
  } else {
    tail_factorial_helper(n - 1, n * acc)
  }
}

fn tail_sum(arr : Array[Int]) -> Int {
  tail_sum_helper(arr, 0, 0)
}

fn tail_sum_helper(arr : Array[Int], index : Int, acc : Int) -> Int {
  if (index >= arr.length()) {
    acc
  } else {
    tail_sum_helper(arr, index + 1, acc + arr[index])
  }
}

fn tail_reverse[T](arr : Array[T]) -> Array[T] {
  tail_reverse_helper(arr, 0, [])
}

fn tail_reverse_helper[T](arr : Array[T], index : Int, acc : Array[T]) -> Array[T] {
  if (index >= arr.length()) {
    acc
  } else {
    let new_acc = []
    new_acc.push(arr[index])
    for i = 0; i < acc.length(); i = i + 1 {
      new_acc.push(acc[i])
    }
    tail_reverse_helper(arr, index + 1, new_acc)
  }
}