// 标准 MoonBit 测试用例 - 10个高质量测试
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "greeting_function_basic" {
  // 问候函数基本测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "ceil_division_basic" {
  // 向上取整除法基本测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtraction_function" {
  // 减法函数测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 10
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, divide_with_ceil(-8, 3))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 23
  let capacity_per_box = 5
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(25, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(2, remaining_space)
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let initial = 10
  let step1 = add(initial, multiply(2, 3))     // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)       // ceil(16/5) = 4
  let step3 = multiply(step2, add(1, 1))       // 4 * (1+1) = 8
  let step4 = divide_with_ceil(step3, 3)       // ceil(8/3) = 3
  assert_eq(3, step4)
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}