// Azimuth 标准测试用例 - 8个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_functions" {
  // 基本算术函数测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "zero_element_properties" {
  // 零元素性质测试
  let x = 42
  
  // 加法零元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法零元素
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "greeting_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 10
  let step1 = add(initial, 5)
  let step2 = multiply(step1, 2)
  let step3 = divide_with_ceil(step2, 8)
  let step4 = subtract(step3, 1)
  
  assert_eq(15, step1)  // 10 + 5 = 15
  assert_eq(30, step2)  // 15 * 2 = 30
  assert_eq(4, step3)  // ceil(30/8) = 4
  assert_eq(3, step4)  // 4 - 1 = 3
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}