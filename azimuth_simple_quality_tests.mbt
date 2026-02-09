// 简化的高质量 MoonBit 测试用例
// 包含10个标准测试用例

// 测试用例1: 基本数学运算测试
test "basic_mathematical_operations" {
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(2, subtract(5, 3))
}

// 测试用例2: 负数运算测试
test "negative_number_operations" {
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-8, subtract(-5, 3))
}

// 测试用例3: 边界条件测试
test "boundary_conditions" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, subtract(5, 5))
}

// 测试用例4: 大数运算测试
test "large_number_operations" {
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(0, subtract(1000000, 1000000))
}

// 测试用例5: 字符串处理测试
test "string_processing" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

// 测试用例6: 数学交换律测试
test "mathematical_commutative_laws" {
  let a = 12
  let b = 8
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

// 测试用例7: 复杂计算测试
test "complex_calculations" {
  let step1 = add(10, 5)
  let step2 = multiply(step1, 3)
  let step3 = divide_with_ceil(step2, 8)
  let step4 = subtract(step3, 2)
  
  assert_eq(15, step1)
  assert_eq(45, step2)
  assert_eq(6, step3)
  assert_eq(4, step4)
}

// 测试用例8: 业务场景测试
test "business_scenario" {
  let items = 157
  let capacity = 24
  let boxes = divide_with_ceil(items, capacity)
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  
  assert_eq(7, boxes)
  assert_eq(168, total_capacity)
  assert_eq(11, remaining)
}

// 测试用例9: 除法精度测试
test "division_precision" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

// 测试用例10: 综合功能测试
test "comprehensive_functionality" {
  let result1 = add(multiply(2, 3), multiply(4, 5))
  let result2 = divide_with_ceil(subtract(20, 5), 3)
  let result3 = greet("Test")
  
  assert_eq(26, result1)
  assert_eq(5, result2)
  assert_eq_string("Hello, Test!", result3)
}