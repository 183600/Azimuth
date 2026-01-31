// 简单的高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_greeting" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "basic_divide_with_ceil" {
  // 基本向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "basic_subtraction" {
  // 基本减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "edge_cases" {
  // 边界情况测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, subtract(5, 5))
}

test "negative_numbers" {
  // 负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "real_world_scenario" {
  // 真实世界场景：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "complex_calculation" {
  // 复杂计算测试
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  let result = add(c, 1)
  
  assert_eq(5, result)
}