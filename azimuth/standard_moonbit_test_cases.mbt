// 标准 MoonBit 测试用例
// 为 azimuth 库添加10个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "ceil_division_basic_cases" {
  // 向上取整除法基本案例测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 15
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, subtract(2, 7))
  assert_eq(-2, add(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}