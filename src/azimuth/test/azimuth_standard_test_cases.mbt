// Azimuth 标准测试用例 - 10个高质量的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_positive_numbers" {
  // 测试加法函数的正数运算
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(100, add(50, 50))
}

test "add_function_negative_numbers" {
  // 测试加法函数的负数运算
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(-7, 5))
}

test "multiply_function_basic" {
  // 测试乘法函数的基本运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_standard" {
  // 测试问候函数的标准输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本运算
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法的负数运算
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(1, -2))
}

test "mathematical_commutative_property" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let result = add(step3, 1)         // 5
  
  assert_eq(5, result)
}

test "unicode_character_support" {
  // 测试Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}