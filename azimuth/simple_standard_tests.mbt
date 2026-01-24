// 简单的标准 MoonBit 测试套件
// 包含10个基本测试用例，使用标准的 MoonBit 测试语法

// 测试1: 基本加法运算
test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

// 测试2: 基本乘法运算
test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
}

// 测试3: 问候函数测试
test "greet_function" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

// 测试4: 向上取整除法基本测试
test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0)) // 除数为0的情况
}

// 测试5: 向上取整除法负数测试
test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(6, -3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

// 测试6: 复杂算术运算组合
test "complex_arithmetic" {
  let a = 12
  let b = 8
  let c = 3
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 12 * (8 + 3) = 12 * 8 + 12 * 3 = 132
  
  // 测试结合律: (a + b) + c = a + (b + c)
  let left_add = add(add(a, b), c)
  let right_add = add(a, add(b, c))
  assert_eq(left_add, right_add)  // (12 + 8) + 3 = 12 + (8 + 3) = 23
}

// 测试7: 边界条件测试
test "boundary_conditions" {
  // 测试零值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试单位值
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
}

// 测试8: 大数运算测试
test "large_number_operations" {
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 测试大数加法
  assert_eq(3000000, add(large_num1, large_num2))
  
  // 测试大数乘法
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 测试大数除法
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
}

// 测试9: 字符串处理测试
test "string_processing" {
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 测试数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
}

// 测试10: 实际应用场景测试
test "real_world_scenario" {
  // 场景：计算装盒问题
  // 有25个物品，每盒最多装6个，需要多少盒？
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证结果
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)  // 5 * 6 = 30
  
  // 验证剩余空间
  let remaining_space = total_capacity - total_items
  assert_eq(5, remaining_space)  // 30 - 25 = 5
}