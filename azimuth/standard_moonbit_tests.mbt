// 标准 MoonBit 测试用例
// 为 azimuth 库添加的核心测试用例

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(25, multiply(5, 5))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_function_various_names" {
  // 测试问候函数的各种名称输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_logic_packaging_calculation" {
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
  let base = 10
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(10, result)  // ceil(10*3/4) + 2 = 8 + 2 = 10
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "unicode_character_support" {
  // 测试Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}