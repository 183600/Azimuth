// Azimuth 数值操作测试
// 专注于测试数值运算和数学函数

// 测试1: 基本算术运算
test "基本算术运算测试" {
  // 加法
  assert_eq(1 + 2, 3)
  assert_eq(0 + 0, 0)
  assert_eq(-5 + 5, 0)
  assert_eq(100 + 200, 300)
  
  // 减法
  assert_eq(5 - 3, 2)
  assert_eq(0 - 0, 0)
  assert_eq(10 - 5, 5)
  assert_eq(5 - 10, -5)
  
  // 乘法
  assert_eq(3 * 4, 12)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(-3 * -3, 9)
  assert_eq(-3 * 3, -9)
  
  // 除法
  assert_eq(10 / 2, 5)
  assert_eq(9 / 3, 3)
  assert_eq(7 / 3, 2)  // 整数除法
  assert_eq(0 / 5, 0)
  
  // 取模
  assert_eq(10 % 3, 1)
  assert_eq(9 % 3, 0)
  assert_eq(7 % 3, 1)
  assert_eq(0 % 5, 0)
}

// 测试2: 数值比较
test "数值比较测试" {
  // 等于
  assert_eq(5 == 5, true)
  assert_eq(5 == 3, false)
  assert_eq(0 == 0, true)
  assert_eq(-5 == -5, true)
  
  // 不等于
  assert_eq(5 != 3, true)
  assert_eq(5 != 5, false)
  
  // 大于
  assert_eq(5 > 3, true)
  assert_eq(3 > 5, false)
  assert_eq(5 > 5, false)
  
  // 小于
  assert_eq(3 < 5, true)
  assert_eq(5 < 3, false)
  assert_eq(5 < 5, false)
  
  // 大于等于
  assert_eq(5 >= 3, true)
  assert_eq(5 >= 5, true)
  assert_eq(3 >= 5, false)
  
  // 小于等于
  assert_eq(3 <= 5, true)
  assert_eq(5 <= 5, true)
  assert_eq(5 <= 3, false)
}

// 测试3: 数值边界测试
test "数值边界测试" {
  // 零值操作
  assert_eq(0 + 0, 0)
  assert_eq(0 - 0, 0)
  assert_eq(0 * 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  
  // 负数操作
  assert_eq(-5 + 5, 0)
  assert_eq(-10 + -5, -15)
  assert_eq(-5 * -5, 25)
  assert_eq(-5 * 5, -25)
  assert_eq(-10 / -2, 5)
  assert_eq(-10 / 2, -5)
  
  // 大数操作
  assert_eq(1000000 + 1000000, 2000000)
  assert_eq(1000000 * 2, 2000000)
  assert_eq(1000000 / 2, 500000)
  
  // 混合正负数操作
  assert_eq(10 + (-5), 5)
  assert_eq(10 - (-5), 15)
  assert_eq(-10 + 5, -5)
  assert_eq(-10 - 5, -15)
}

// 测试4: 数学函数 - 幂运算
test "幂运算测试" {
  assert_eq(power(2, 0), 1)
  assert_eq(power(2, 1), 2)
  assert_eq(power(2, 3), 8)
  assert_eq(power(3, 2), 9)
  assert_eq(power(5, 3), 125)
  assert_eq(power(10, 2), 100)
  
  assert_eq(power(0, 5), 0)
  assert_eq(power(5, 0), 1)
  assert_eq(power(0, 0), 1)  // 按照数学惯例
  
  assert_eq(power(-2, 2), 4)
  assert_eq(power(-2, 3), -8)
}

fn power(base : Int, exponent : Int) -> Int {
  if (exponent < 0) {
    0  // 负指数返回0，简化处理
  } else if (exponent == 0) {
    1
  } else {
    let result = 1
    for i = 0; i < exponent; i = i + 1 {
      result = result * base
    }
    result
  }
}

// 测试5: 数学函数 - 绝对值
test "绝对值测试" {
  assert_eq(abs(5), 5)
  assert_eq(abs(-5), 5)
  assert_eq(abs(0), 0)
  assert_eq(abs(100), 100)
  assert_eq(abs(-100), 100)
  assert_eq(abs(-1), 1)
}

fn abs(x : Int) -> Int {
  if (x < 0) { -x } else { x }
}

// 测试6: 数学函数 - 最大值和最小值
test "最大值和最小值测试" {
  assert_eq(max(5, 3), 5)
  assert_eq(max(3, 5), 5)
  assert_eq(max(5, 5), 5)
  assert_eq(max(-5, -3), -3)
  assert_eq(max(-3, -5), -3)
  assert_eq(max(0, -5), 0)
  assert_eq(max(-5, 0), 0)
  
  assert_eq(min(5, 3), 3)
  assert_eq(min(3, 5), 3)
  assert_eq(min(5, 5), 5)
  assert_eq(min(-5, -3), -5)
  assert_eq(min(-3, -5), -5)
  assert_eq(min(0, -5), -5)
  assert_eq(min(-5, 0), -5)
}

fn max(a : Int, b : Int) -> Int {
  if (a > b) { a } else { b }
}

fn min(a : Int, b : Int) -> Int {
  if (a < b) { a } else { b }
}

// 测试7: 数组数值操作
test "数组数值操作测试" {
  let numbers = [1, 2, 3, 4, 5]
  
  assert_eq(array_sum(numbers), 15)
  assert_eq(array_product(numbers), 120)
  assert_eq(array_average(numbers), 3)
  assert_eq(array_max(numbers), 5)
  assert_eq(array_min(numbers), 1)
  
  let empty_array = []
  assert_eq(array_sum(empty_array), 0)
  assert_eq(array_product(empty_array), 1)
  assert_eq(array_average(empty_array), 0)
  
  let negative_numbers = [-5, -3, -1, -7, -2]
  assert_eq(array_sum(negative_numbers), -18)
  assert_eq(array_max(negative_numbers), -1)
  assert_eq(array_min(negative_numbers), -7)
  
  let mixed_numbers = [-5, 0, 5, 10, -10]
  assert_eq(array_sum(mixed_numbers), 0)
  assert_eq(array_max(mixed_numbers), 10)
  assert_eq(array_min(mixed_numbers), -10)
}

fn array_sum(arr : Array[Int]) -> Int {
  let result = 0
  for i = 0; i < arr.length(); i = i + 1 {
    result = result + arr[i]
  }
  result
}

fn array_product(arr : Array[Int]) -> Int {
  if (arr.length() == 0) {
    return 1
  }
  
  let result = 1
  for i = 0; i < arr.length(); i = i + 1 {
    result = result * arr[i]
  }
  result
}

fn array_average(arr : Array[Int]) -> Int {
  if (arr.length() == 0) {
    return 0
  }
  array_sum(arr) / arr.length()
}

fn array_max(arr : Array[Int]) -> Int {
  if (arr.length() == 0) {
    return 0
  }
  
  let result = arr[0]
  for i = 1; i < arr.length(); i = i + 1 {
    if (arr[i] > result) {
      result = arr[i]
    }
  }
  result
}

fn array_min(arr : Array[Int]) -> Int {
  if (arr.length() == 0) {
    return 0
  }
  
  let result = arr[0]
  for i = 1; i < arr.length(); i = i + 1 {
    if (arr[i] < result) {
      result = arr[i]
    }
  }
  result
}

// 测试8: 数值范围检查
test "数值范围检查测试" {
  assert_eq(is_in_range(5, 1, 10), true)
  assert_eq(is_in_range(1, 1, 10), true)
  assert_eq(is_in_range(10, 1, 10), true)
  assert_eq(is_in_range(0, 1, 10), false)
  assert_eq(is_in_range(11, 1, 10), false)
  
  assert_eq(is_in_range(-5, -10, -1), true)
  assert_eq(is_in_range(-10, -10, -1), true)
  assert_eq(is_in_range(-1, -10, -1), true)
  assert_eq(is_in_range(-11, -10, -1), false)
  assert_eq(is_in_range(0, -10, -1), false)
  
  assert_eq(clamp_to_range(5, 1, 10), 5)
  assert_eq(clamp_to_range(0, 1, 10), 1)
  assert_eq(clamp_to_range(15, 1, 10), 10)
  assert_eq(clamp_to_range(-5, -10, -1), -5)
  assert_eq(clamp_to_range(-15, -10, -1), -10)
  assert_eq(clamp_to_range(5, -10, -1), -1)
}

fn is_in_range(value : Int, min : Int, max : Int) -> Bool {
  value >= min and value <= max
}

fn clamp_to_range(value : Int, min : Int, max : Int) -> Int {
  if (value < min) {
    min
  } else if (value > max) {
    max
  } else {
    value
  }
}